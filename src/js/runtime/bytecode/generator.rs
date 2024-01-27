use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::{self},
};

use crate::js::{
    common::wtf_8::Wtf8String,
    parser::{
        ast::{self, AstPtr},
        parser::ParseProgramResult,
        scope_tree::{ScopeTree, VMLocation},
    },
    runtime::{
        bytecode::function::BytecodeFunction,
        gc::{Escapable, HandleScope},
        interned_strings::InternedStrings,
        regexp::compiler::compile_regexp,
        value::BigIntValue,
        Context, Handle, Value,
    },
};

use super::{
    constant_table_builder::{ConstantTableBuilder, ConstantTableIndex},
    exception_handlers::{ExceptionHandlerBuilder, ExceptionHandlersBuilder},
    instruction::{DecodeInfo, OpCode},
    operand::{min_width_for_signed, ConstantIndex, Operand, Register, SInt, UInt},
    register_allocator::TemporaryRegisterAllocator,
    width::{ExtraWide, Narrow, Wide, Width, WidthEnum},
    writer::BytecodeWriter,
};

/// Bytecode generator for an entire program. Handles generating the global function as well as all
/// functions within the program.
pub struct BytecodeProgramGenerator<'a> {
    cx: Context,
    scope_tree: &'a ScopeTree,

    /// Queue of functions that still need to be generated, along with the information needed to
    /// patch their creation into their parent function.
    pending_functions_queue: VecDeque<PatchablePendingFunction>,
}

impl<'a> BytecodeProgramGenerator<'a> {
    pub fn new(cx: Context, scope_tree: &'a ScopeTree) -> Self {
        Self { cx, scope_tree, pending_functions_queue: VecDeque::new() }
    }

    /// Generate an entire program from a program parse result. Return the toplevel program function
    /// used to execute the program.
    pub fn generate_from_program_parse_result(
        cx: Context,
        parse_result: &ParseProgramResult,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        HandleScope::new(cx, |_| {
            let mut generator = BytecodeProgramGenerator::new(cx, &parse_result.scope_tree);
            generator.generate_program(&parse_result.program)
        })
    }

    fn generate_program(&mut self, program: &ast::Program) -> EmitResult<Handle<BytecodeFunction>> {
        let program_function = self.gen_script_function(program)?;

        while let Some(pending_function) = self.pending_functions_queue.pop_front() {
            self.gen_enqueued_function(pending_function)?;
        }

        Ok(program_function)
    }

    fn gen_script_function(
        &mut self,
        program: &ast::Program,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        // Handle where the result BytecodeFunction is placed
        let mut bytecode_function_handle = Handle::<BytecodeFunction>::empty(self.cx);

        HandleScope::new(self.cx, |_| {
            let mut generator =
                BytecodeFunctionGenerator::new_for_program(self.cx, program, &self.scope_tree)?;

            // Script function consists of toplevel statements
            for toplevel in &program.toplevels {
                generator.gen_toplevel(toplevel)?;
            }

            // Implicitly return undefined at end of script function
            generator.gen_return_undefined()?;

            let emit_result = generator.finish();

            // Store the generated function into the parent handle scope so the enqueued references
            // remain valid.
            bytecode_function_handle.replace(emit_result.bytecode_function.get_());

            self.enqueue_pending_functions(bytecode_function_handle, emit_result.pending_functions);

            Ok(())
        })?;

        Ok(bytecode_function_handle)
    }

    fn gen_enqueued_function(
        &mut self,
        pending_function: PatchablePendingFunction,
    ) -> EmitResult<()> {
        let PatchablePendingFunction { func_node, parent_function, constant_index } =
            pending_function;

        // Handle where the result BytecodeFunction is placed
        let mut bytecode_function_handle = Handle::<BytecodeFunction>::empty(self.cx);

        HandleScope::new(self.cx, |_| {
            let ast_node = func_node.ast_ptr();
            let generator =
                BytecodeFunctionGenerator::new_for_function(self.cx, func_node, &self.scope_tree)?;

            let emit_result = generator.generate(ast_node.as_ref())?;

            // Store the generated function into the parent handle scope so the enqueued references
            // remain valid.
            bytecode_function_handle.replace(emit_result.bytecode_function.get_());

            self.enqueue_pending_functions(bytecode_function_handle, emit_result.pending_functions);

            // Patch function into parent function's constant table
            let mut parent_constant_table = parent_function.constant_table_ptr().unwrap();
            parent_constant_table.set_constant(
                constant_index as usize,
                bytecode_function_handle.cast::<Value>().get(),
            );

            Ok(())
        })
    }

    fn enqueue_pending_functions(
        &mut self,
        parent_function: Handle<BytecodeFunction>,
        pending_functions: PendingFunctions,
    ) {
        for (func_node, constant_index) in pending_functions {
            self.pending_functions_queue
                .push_back(PatchablePendingFunction { func_node, parent_function, constant_index });
        }
    }
}

/// Bytecode generator for a single function.
pub struct BytecodeFunctionGenerator<'a> {
    pub writer: BytecodeWriter,
    cx: Context,
    _scope_tree: Option<&'a ScopeTree>,

    /// Optional name of the function, used for debugging.
    debug_name: Option<Wtf8String>,

    /// Number of blocks currently allocated in the function.
    num_blocks: usize,

    /// Map from block id to the offset of that block in the bytecode. Contains an entry for every
    /// block that has been started in the bycode. Used to calculate backwards jump offsets.
    ///
    /// Note that not every block that has been allocated has actually been started, e.g. the
    /// continue block allocated for labeled non-loop statements.
    block_offsets: HashMap<BlockId, usize>,

    /// Map from block id to all unresolved forward jumps that target that block. Only contains
    /// entries for blocks that have not yet been started.
    unresolved_forward_jumps: HashMap<BlockId, Vec<ForwardJumpInfo>>,

    /// Stack of jump statement (break and continue) targets that the generator is currently inside.
    /// The top of the stack is the innermost jump statement target.
    jump_statement_target_stack: Vec<JumpStatementTarget>,

    /// Number of toplevel parameters to this function, not counting the rest parameter.
    num_parameters: u32,

    /// Whether this function is in strict mode.
    is_strict: bool,

    // Whether this function is a constructor
    is_constructor: bool,

    constant_table_builder: ConstantTableBuilder,
    register_allocator: TemporaryRegisterAllocator,
    exception_handler_builder: ExceptionHandlersBuilder,

    /// Queue of functions that still need to be generated.
    pending_functions_queue: PendingFunctions,
}

impl<'a> BytecodeFunctionGenerator<'a> {
    fn new(
        cx: Context,
        scope_tree: Option<&'a ScopeTree>,
        debug_name: Option<Wtf8String>,
        num_parameters: u32,
        num_local_registers: u32,
        is_strict: bool,
        is_constructor: bool,
    ) -> Self {
        Self {
            writer: BytecodeWriter::new(),
            cx,
            _scope_tree: scope_tree,
            debug_name,
            num_blocks: 0,
            block_offsets: HashMap::new(),
            unresolved_forward_jumps: HashMap::new(),
            jump_statement_target_stack: vec![],
            num_parameters,
            is_strict,
            is_constructor,
            constant_table_builder: ConstantTableBuilder::new(),
            register_allocator: TemporaryRegisterAllocator::new(num_local_registers),
            exception_handler_builder: ExceptionHandlersBuilder::new(),
            pending_functions_queue: vec![],
        }
    }

    fn new_for_function(
        cx: Context,
        pending_function: PendingFunctionNode,
        scope_tree: &'a ScopeTree,
    ) -> EmitResult<Self> {
        let is_constructor = pending_function.is_constructor();
        let func_ptr = pending_function.ast_ptr();
        let func = func_ptr.as_ref();

        // Number of arguments does not count the rest parameter
        let num_parameters = if let Some(ast::FunctionParam::Rest(_)) = func.params.last() {
            func.params.len() - 1
        } else {
            func.params.len()
        };

        // Number of local registers was determined while creating the VM scope tree
        let num_local_registers = func.scope.as_ref().num_local_registers();

        // Validate that the number of arguments and local registers are within the limits of the
        // bytecode format.
        if num_parameters > GenRegister::MAX_ARGUMENT_INDEX as usize {
            return Err(EmitError::TooManyFunctionParameters);
        }

        if num_local_registers > GenRegister::MAX_LOCAL_INDEX as usize {
            return Err(EmitError::TooManyRegisters);
        }

        let anonymous_name = pending_function.name();
        let debug_name = func
            .id
            .as_ref()
            .map(|id| Wtf8String::from_str(&id.name))
            .or(anonymous_name);

        Ok(Self::new(
            cx,
            Some(scope_tree),
            debug_name,
            num_parameters as u32,
            num_local_registers as u32,
            func.is_strict_mode,
            is_constructor,
        ))
    }

    fn new_for_program(
        cx: Context,
        program: &ast::Program,
        scope_tree: &'a ScopeTree,
    ) -> EmitResult<Self> {
        // Number of local registers was determined while creating the VM scope tree
        let num_local_registers = program.scope.as_ref().num_local_registers();

        // Validate that the number of local registers is within the limits of the bytecode format
        if num_local_registers > GenRegister::MAX_LOCAL_INDEX as usize {
            return Err(EmitError::TooManyRegisters);
        }

        Ok(Self::new(
            cx,
            Some(scope_tree),
            Some(Wtf8String::from_str("<global>")),
            0,
            num_local_registers as u32,
            program.is_strict_mode,
            /* is_constructor */ false,
        ))
    }

    fn new_block(&mut self) -> BlockId {
        let next_block_id = self.num_blocks;
        self.num_blocks += 1;
        next_block_id
    }

    fn start_block(&mut self, block_id: BlockId) {
        let current_offset = self.writer.current_offset();
        self.block_offsets.insert(block_id, current_offset);

        if let Some(unresolved_forward_jumps) = self.unresolved_forward_jumps.remove(&block_id) {
            for jump_info in unresolved_forward_jumps {
                self.patch_forward_jump(jump_info, block_id);
            }
        }
    }

    /// Patch the jump specified by `jump_info` to jump to `target_block_id`. Only called when the
    /// target block has already been started.
    fn patch_forward_jump(&mut self, jump_info: ForwardJumpInfo, target_block_id: BlockId) {
        let target_offset = self.block_offsets[&target_block_id];

        // Offset is from the start of the jump instruction (including prefixes) to the start of the
        // target block.
        let relative_offset = (target_offset - jump_info.jump_offset) as isize;
        let min_width_needed = min_width_for_signed(relative_offset);

        let jump_instr_info = self
            .writer
            .decode_width_and_opcode_at_index(jump_info.jump_offset);

        // Determine if the relative offset can fit inline into the instruction. Be sure to check if
        // the offset can fit in an extra wide operand or if must become a constant jump.
        if min_width_needed <= jump_instr_info.width
            && GenSInt::try_from_signed(relative_offset).is_some()
        {
            // Offset fits inline, remove constant reservation and write inline into instruction
            self.constant_table_builder
                .remove_reserved(jump_info.constant_table_width);

            self.set_jump_offset_operand(&jump_instr_info, relative_offset as usize)
        } else {
            // Offset does not fit inline, so this must become a constant jump
            let constant_index = self
                .constant_table_builder
                .commit_reserved_bytecode_offset(jump_info.constant_table_width, relative_offset);

            self.set_jump_offset_operand(&jump_instr_info, constant_index as usize);

            // Convert to the equivalent constant jump instruction. Each constant jump opcode is
            // exactly one above it's corresponding immediate jump instruction.
            self.writer
                .set_u8(jump_instr_info.opcode_index, jump_instr_info.opcode as u8 + 1)
        }
    }

    fn set_jump_offset_operand(&mut self, jump_instr_info: &DecodeInfo, offset: usize) {
        // Unconditional jumps store the offset in the first operand, while all conditional jumps
        // store the offset in the second operand.
        let operand_index = if jump_instr_info.opcode == OpCode::Jump {
            0
        } else {
            1
        };

        // Determine the offset of the operand in the bytecode
        let operand_offset =
            jump_instr_info.opcode_index + 1 + (operand_index * jump_instr_info.width.num_bytes());

        match jump_instr_info.width {
            WidthEnum::Narrow => {
                self.writer.set_u8(operand_offset, offset as u8);
            }
            WidthEnum::Wide => {
                self.writer.set_u16(operand_offset, offset as u16);
            }
            WidthEnum::ExtraWide => {
                self.writer.set_u32(operand_offset, offset as u32);
            }
        }
    }

    /// Return the operand that is needed to perform a jump to the given target block. Returns an
    /// inline operand when possible, otherwise returns a constant table index. Sets up forward
    /// jumps to be patched later.
    ///
    /// Must be called when the writer is at the start of a new jump instruction.
    fn jump_target_operand(&mut self, target_block: BlockId) -> EmitResult<JumpOperand> {
        let jump_offset = self.writer.current_offset();

        if let Some(target_block_offset) = self.block_offsets.get(&target_block) {
            // Backwards jump - target block offset is already known
            let relative_offset = *target_block_offset as isize - jump_offset as isize;

            // If the offset fits into an operand use it directly, otherwise store in constant table
            if let Some(operand) = GenSInt::try_from_signed(relative_offset) {
                Ok(JumpOperand::RelativeOffset(operand))
            } else {
                let constant_index = self
                    .constant_table_builder
                    .add_bytecode_offset(relative_offset)?;
                Ok(JumpOperand::ConstantIndex(ConstantIndex::new(constant_index)))
            }
        } else {
            // Forwards jump - target block offset is not yet known so reserve an entry in the
            // constant table and patch as either an inline or constant jump later.
            let constant_table_width = self.constant_table_builder.reserve_entry()?;

            let forward_jump_info = ForwardJumpInfo { jump_offset, constant_table_width };
            self.unresolved_forward_jumps
                .entry(target_block)
                .or_default()
                .push(forward_jump_info);

            // Create a dummy operand that requires the specified width, which will be patched later
            let dummy_offset = match constant_table_width {
                WidthEnum::Narrow => Narrow::SIGNED_MIN as i32,
                WidthEnum::Wide => Wide::SIGNED_MIN as i32,
                WidthEnum::ExtraWide => ExtraWide::SIGNED_MIN as i32,
            };

            Ok(JumpOperand::RelativeOffset(SInt::new(dummy_offset)))
        }
    }

    fn write_jump_instruction(&mut self, target_block: BlockId) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => {
                self.writer.jump_instruction(relative_offset)
            }
            JumpOperand::ConstantIndex(constant_index) => {
                self.writer.jump_constant_instruction(constant_index)
            }
        }

        Ok(())
    }

    fn write_jump_true_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_true_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_true_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_to_boolean_true_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_to_boolean_true_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_to_boolean_true_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_true_for_expression(
        &mut self,
        condition_expr: &ast::Expression,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        if self.evaluates_to_boolean(condition_expr) {
            self.write_jump_true_instruction(condition, target_block)
        } else {
            self.write_jump_to_boolean_true_instruction(condition, target_block)
        }
    }

    fn write_jump_false_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_false_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_false_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_to_boolean_false_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_to_boolean_false_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_to_boolean_false_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_nullish_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_nullish_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_nullish_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_not_nullish_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_not_nullish_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_not_nullish_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_false_for_expression(
        &mut self,
        condition_expr: &ast::Expression,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        if self.evaluates_to_boolean(condition_expr) {
            self.write_jump_false_instruction(condition, target_block)
        } else {
            self.write_jump_to_boolean_false_instruction(condition, target_block)
        }
    }

    fn write_mov_instruction(&mut self, dest: GenRegister, src: GenRegister) {
        // Prevent no-op moves
        if src == dest {
            return;
        }

        self.writer.mov_instruction(dest, src);
    }

    fn add_string_constant(&mut self, str: &str) -> EmitResult<GenConstantIndex> {
        let string = InternedStrings::get_str(self.cx, str).as_flat();
        let constant_index = self.constant_table_builder.add_string(string)?;
        Ok(ConstantIndex::new(constant_index))
    }

    fn add_wtf8_string_constant(&mut self, str: &Wtf8String) -> EmitResult<GenConstantIndex> {
        let string = InternedStrings::get_wtf8_str(self.cx, str).as_flat();
        let constant_index = self.constant_table_builder.add_string(string)?;
        Ok(ConstantIndex::new(constant_index))
    }

    fn enqueue_function_to_generate(
        &mut self,
        function: PendingFunctionNode,
    ) -> EmitResult<ConstantTableIndex> {
        let constant_index = self
            .constant_table_builder
            .add_heap_object(Handle::dangling())?;
        self.pending_functions_queue
            .push((function, constant_index));

        Ok(constant_index)
    }

    /// Generate the bytecode for a function.
    fn generate(mut self, func: &ast::Function) -> EmitResult<EmitFunctionResult> {
        if func.is_async || func.is_generator {
            unimplemented!("bytecode for async and generator functions")
        }

        match func.body.as_ref() {
            ast::FunctionBody::Block(block_body) => {
                let body_completion = self.gen_statement_list(&block_body.body)?;

                // If the body continues, meaning there was no return statement (or throw, break,
                // etc.) then implicitly return undefined.
                if !body_completion.is_abrupt() {
                    self.gen_return_undefined()?;
                }
            }
            ast::FunctionBody::Expression(expr_body) => {
                let return_value_reg = self.gen_expression(expr_body)?;
                self.writer.ret_instruction(return_value_reg);
                self.register_allocator.release(return_value_reg);
            }
        }

        Ok(self.finish())
    }

    /// Finish generating the bytecode for a function. Returns the bytecode function and the
    /// queue of AST node functions from the body that need to be generated.
    fn finish(self) -> EmitFunctionResult {
        debug_assert!(self.block_offsets.len() == self.num_blocks);
        debug_assert!(self.unresolved_forward_jumps.is_empty());
        debug_assert!(self.register_allocator.is_empty());

        let constant_table = self.constant_table_builder.finish(self.cx);
        let exception_handlers = self.exception_handler_builder.finish(self.cx);

        let num_registers = self.register_allocator.max_allocated();
        let debug_name = self
            .debug_name
            .as_ref()
            .map(|name| InternedStrings::get_wtf8_str(self.cx, name));
        let bytecode = self.writer.finish();

        let bytecode_function = BytecodeFunction::new(
            self.cx,
            bytecode,
            constant_table,
            exception_handlers,
            num_registers,
            self.num_parameters,
            self.is_strict,
            self.is_constructor,
            debug_name,
        );

        EmitFunctionResult {
            bytecode_function,
            pending_functions: self.pending_functions_queue,
        }
    }

    fn gen_toplevel(&mut self, toplevel: &ast::Toplevel) -> EmitResult<()> {
        match toplevel {
            ast::Toplevel::Statement(stmt) => {
                // Ignore completion of toplevel statements, generate later toplevels even if abrupt
                let _ = self.gen_statement(stmt)?;
                Ok(())
            }
            ast::Toplevel::Import(_) => unimplemented!("bytecode for import declarations"),
            ast::Toplevel::ExportDefault(_)
            | ast::Toplevel::ExportNamed(_)
            | ast::Toplevel::ExportAll(_) => unimplemented!("bytecode for export declarations"),
        }
    }

    fn gen_statement(&mut self, stmt: &ast::Statement) -> EmitResult<StmtCompletion> {
        // Every statement is generated with its own temporary register scope
        debug_assert!(self.register_allocator.is_empty());

        let result = match stmt {
            ast::Statement::VarDecl(var_decl) => self.gen_variable_declaration(var_decl),
            ast::Statement::FuncDecl(func_decl) => self.gen_function_declaraton(func_decl.as_ref()),
            ast::Statement::ClassDecl(_) => unimplemented!("bytecode for class declaration"),
            ast::Statement::Expr(stmt) => self.gen_expression_statement(stmt),
            ast::Statement::Block(stmt) => self.gen_block_statement(stmt),
            ast::Statement::If(stmt) => self.gen_if_statement(stmt),
            ast::Statement::Switch(_) => unimplemented!("bytecode for switch statement"),
            ast::Statement::For(stmt) => self.gen_for_statement(stmt, None),
            ast::Statement::ForEach(_) => unimplemented!("bytecode for for-each statement"),
            ast::Statement::While(stmt) => self.gen_while_statement(stmt, None),
            ast::Statement::DoWhile(stmt) => self.gen_do_while_statement(stmt, None),
            ast::Statement::With(_) => unimplemented!("bytecode for with statement"),
            ast::Statement::Try(stmt) => self.gen_try_statement(stmt),
            ast::Statement::Throw(stmt) => self.gen_throw_statement(stmt),
            ast::Statement::Return(stmt) => self.gen_return_statement(stmt),
            ast::Statement::Break(stmt) => self.gen_break_statement(stmt),
            ast::Statement::Continue(stmt) => self.gen_continue_statement(stmt),
            ast::Statement::Labeled(stmt) => self.gen_labeled_statement(stmt),
            // Intentionally ignored as there is no runtime effect
            ast::Statement::Empty(_) | ast::Statement::Debugger(_) => Ok(StmtCompletion::Normal),
        };

        debug_assert!(self.register_allocator.is_empty());

        result
    }

    fn gen_expression_with_dest(
        &mut self,
        expr: &ast::Expression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match expr {
            ast::Expression::Id(expr) => self.gen_load_identifier(expr, dest),
            ast::Expression::Null(_) => self.gen_null_literal_expression(dest),
            ast::Expression::Boolean(expr) => self.gen_boolean_literal_expression(expr, dest),
            ast::Expression::Number(expr) => self.gen_number_literal(expr.value, dest),
            ast::Expression::String(expr) => self.gen_string_literal_expression(expr, dest),
            ast::Expression::BigInt(expr) => self.gen_bigint_literal_expression(expr, dest),
            ast::Expression::RegExp(expr) => self.gen_regexp_literal_expression(expr, dest),
            ast::Expression::This(_) => self.gen_this_expression(dest),
            ast::Expression::Unary(expr) => match expr.operator {
                ast::UnaryOperator::Plus => self.gen_unary_plus_expression(expr, dest),
                ast::UnaryOperator::Minus => self.gen_unary_minus_expression(expr, dest),
                ast::UnaryOperator::LogicalNot => self.gen_logical_not_expression(expr, dest),
                ast::UnaryOperator::BitwiseNot => self.gen_bitwise_not_expression(expr, dest),
                ast::UnaryOperator::TypeOf => self.gen_typeof_expression(expr, dest),
                ast::UnaryOperator::Void => self.gen_void_expression(expr, dest),
                ast::UnaryOperator::Delete => unimplemented!("bytecode for delete expressions"),
            },
            ast::Expression::Binary(expr) => self.gen_binary_expression(expr, dest),
            ast::Expression::Logical(expr) => self.gen_logical_expression(expr, dest),
            ast::Expression::Assign(expr) => self.gen_assignment_expression(expr, dest),
            ast::Expression::Update(expr) => self.gen_update_expression(expr, dest),
            ast::Expression::Member(expr) => self.gen_member_expression(expr, dest, None, None),
            ast::Expression::Chain(expr) => self.gen_chain_expression(expr, dest, None),
            ast::Expression::Conditional(expr) => self.gen_conditional_expression(expr, dest),
            ast::Expression::Call(expr) => self.gen_call_expression(expr, dest, None),
            ast::Expression::New(expr) => self.gen_new_expresssion(expr, dest),
            ast::Expression::Sequence(expr) => self.gen_sequence_expression(expr, dest),
            ast::Expression::Array(expr) => self.gen_array_literal(expr, dest),
            ast::Expression::Object(expr) => self.gen_object_literal(expr, dest),
            ast::Expression::Function(expr) => self.gen_function_expression(expr, None, dest),
            ast::Expression::ArrowFunction(expr) => {
                self.gen_arrow_function_expression(expr, None, dest)
            }
            ast::Expression::Class(_) => unimplemented!("bytecode for class expressions"),
            ast::Expression::Await(_) => unimplemented!("bytecode for await expressions"),
            ast::Expression::Yield(_) => unimplemented!("bytecode for yield expressions"),
            ast::Expression::SuperMember(_) => {
                unimplemented!("bytecode for super member expressions")
            }
            ast::Expression::SuperCall(_) => unimplemented!("bytecode for super call expressions"),
            ast::Expression::Template(_) => unimplemented!("bytecode for template literals"),
            ast::Expression::TaggedTemplate(_) => {
                unimplemented!("bytecode for tagged template expressions")
            }
            ast::Expression::MetaProperty(_) => unimplemented!("bytecode for meta property"),
            ast::Expression::Import(_) => unimplemented!("bytecode for import expressions"),
        }
    }

    /// Generate an expression with any destination register.
    #[inline]
    fn gen_expression(&mut self, expr: &ast::Expression) -> EmitResult<GenRegister> {
        self.gen_expression_with_dest(expr, ExprDest::Any)
    }

    fn gen_load_identifier(
        &mut self,
        id: &ast::Identifier,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        if id.scope.is_none() {
            // TODO: Generate dynamic lookup from current scope if in eval or with
            return self.gen_load_global_identifier(id, dest);
        }

        let scope = id.scope.unwrap();
        let binding = scope.as_ref().get_binding(&id.name);

        match binding.vm_location().unwrap() {
            // Variables in arguments and registers can be encoded directly as register operands.
            // Make sure to load to a new temporary if necessary.
            VMLocation::Argument(index) => {
                let arg_reg = Register::argument(index);
                self.gen_mov_reg_to_dest(arg_reg, dest)
            }
            VMLocation::LocalRegister(index) => {
                let local_reg = Register::local(index);
                self.gen_mov_reg_to_dest(local_reg, dest)
            }
            // Global variables must first be loaded to a register
            VMLocation::Global => self.gen_load_global_identifier(id, dest),
            VMLocation::Scope { .. } => unimplemented!("bytecode for loading scope variables"),
        }
    }

    fn gen_load_global_identifier(
        &mut self,
        id: &ast::Identifier,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let name_value = InternedStrings::get_str(self.cx, &id.name).as_flat();
        let constant_index = self.constant_table_builder.add_string(name_value)?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .load_global_instruction(dest, ConstantIndex::new(constant_index));

        Ok(dest)
    }

    fn gen_store_identifier(&mut self, id: &ast::Identifier, value: GenRegister) -> EmitResult<()> {
        if id.scope.is_none() {
            // TODO: Generate dynamic lookup from current scope if in eval or with
            return self.gen_store_global_identifier(id, value);
        }

        let scope = id.scope.unwrap();
        let binding = scope.as_ref().get_binding(&id.name);

        match binding.vm_location().unwrap() {
            // Variables in arguments and registers are be encoded directly as register operands,
            // so simply move to the correct register.
            VMLocation::Argument(index) => {
                let arg_reg = Register::argument(index);
                self.write_mov_instruction(arg_reg, value);
            }
            VMLocation::LocalRegister(index) => {
                let local_reg = Register::local(index);
                self.write_mov_instruction(local_reg, value);
            }
            // Globals must be stored with name appearing in the constant table
            VMLocation::Global => self.gen_store_global_identifier(id, value)?,
            VMLocation::Scope { .. } => unimplemented!("bytecode for storing scope variables"),
        }

        Ok(())
    }

    fn gen_store_global_identifier(
        &mut self,
        id: &ast::Identifier,
        value: GenRegister,
    ) -> EmitResult<()> {
        let name_value = InternedStrings::get_str(self.cx, &id.name).as_flat();
        let constant_index = self.constant_table_builder.add_string(name_value)?;
        self.writer
            .store_global_instruction(value, ConstantIndex::new(constant_index));

        Ok(())
    }

    /// Find the best expression destination in which to place the result of an expression that will
    /// be stored at the given identifier's location.
    ///
    /// This allows expressions to be stored directly into their destination register, avoiding an
    /// unnecessary mov instruction.
    fn expr_dest_for_id(&mut self, id: &ast::Identifier) -> ExprDest {
        // Unresolved variables can be stored from any register
        if id.scope.is_none() {
            return ExprDest::Any;
        }

        let scope = id.scope.unwrap();
        let binding = scope.as_ref().get_binding(&id.name);

        match binding.vm_location().unwrap() {
            // Variables in arguments and registers can be encoded directly as register operands.
            // Make sure to load to a new temporary if necessary.
            VMLocation::Argument(index) => ExprDest::Fixed(Register::argument(index)),
            VMLocation::LocalRegister(index) => ExprDest::Fixed(Register::local(index)),
            // Global variables can be stored from any register
            VMLocation::Global => ExprDest::Any,
            VMLocation::Scope { .. } => unimplemented!("bytecode for loading scope variables"),
        }
    }

    /// Allocate the destination register for an expression, which may be a new temporary or a fixed
    /// register that was specified.
    fn allocate_destination(&mut self, dest: ExprDest) -> EmitResult<GenRegister> {
        match dest {
            ExprDest::Any | ExprDest::NewTemporary => self.register_allocator.allocate(),
            ExprDest::Fixed(dest) => Ok(dest),
        }
    }

    /// Move a src register to the expression destination if necessary. Releases the source register
    /// if it was moved.
    fn gen_mov_reg_to_dest(&mut self, src: GenRegister, dest: ExprDest) -> EmitResult<GenRegister> {
        match dest {
            ExprDest::Any => Ok(src),
            ExprDest::NewTemporary => {
                self.register_allocator.release(src);
                let dest = self.register_allocator.allocate()?;
                self.write_mov_instruction(dest, src);
                Ok(dest)
            }
            ExprDest::Fixed(dest) => {
                self.register_allocator.release(src);
                self.write_mov_instruction(dest, src);
                Ok(dest)
            }
        }
    }

    fn gen_ensure_reg_is_temporary(&mut self, reg: GenRegister) -> EmitResult<GenRegister> {
        if self.register_allocator.is_temporary_register(reg) {
            Ok(reg)
        } else {
            let dest = self.register_allocator.allocate()?;
            self.write_mov_instruction(dest, reg);
            Ok(dest)
        }
    }

    fn gen_null_literal_expression(&mut self, dest: ExprDest) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        self.writer.load_null_instruction(dest);
        Ok(dest)
    }

    fn gen_number_literal(&mut self, value: f64, dest: ExprDest) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        // Smis are inlined as immediate while all other numbers are stored in the constant table
        let number = Value::number(value);
        if number.is_smi() {
            self.writer
                .load_immediate_instruction(dest, SInt::new(number.as_smi()));
        } else {
            let constant_index = self.constant_table_builder.add_double(number.as_double())?;
            self.writer
                .load_constant_instruction(dest, ConstantIndex::new(constant_index));
        }

        Ok(dest)
    }

    fn gen_boolean_literal_expression(
        &mut self,
        expr: &ast::BooleanLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        if expr.value {
            self.writer.load_true_instruction(dest);
        } else {
            self.writer.load_false_instruction(dest);
        }

        Ok(dest)
    }

    fn gen_string_literal_expression(
        &mut self,
        expr: &ast::StringLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        // All string literals are loaded from the constant table
        let constant_index = self.add_wtf8_string_constant(&expr.value)?;
        self.writer.load_constant_instruction(dest, constant_index);

        Ok(dest)
    }

    fn gen_bigint_literal_expression(
        &mut self,
        lit: &ast::BigIntLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        // BigInts are stored in the constant table, but not deduped
        let bigint_value = BigIntValue::new(self.cx, lit.value.clone()).to_handle();
        let constant_index = self
            .constant_table_builder
            .add_heap_object(bigint_value.cast())?;
        self.writer
            .load_constant_instruction(dest, ConstantIndex::new(constant_index));

        Ok(dest)
    }

    fn gen_regexp_literal_expression(
        &mut self,
        lit: &ast::RegExpLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Can use source directly as "escaped" pattern source string
        let source = InternedStrings::get_wtf8_str(self.cx, &lit.pattern);

        // Compile regexp and store compiled regexp in constant table
        let compiled_regexp = compile_regexp(self.cx, &lit.regexp, source);
        let compiled_regexp_index = self
            .constant_table_builder
            .add_heap_object(compiled_regexp.cast())?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .new_regexp_instruction(dest, ConstantIndex::new(compiled_regexp_index));

        Ok(dest)
    }

    fn gen_this_expression(&mut self, dest: ExprDest) -> EmitResult<GenRegister> {
        self.gen_mov_reg_to_dest(Register::this(), dest)
    }

    fn gen_unary_minus_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // A unary minus on a number literal is inlined as a negative number literal
        if let ast::Expression::Number(number_expr) = expr.argument.as_ref() {
            return self.gen_number_literal(-number_expr.value, dest);
        }

        // Otherwise generate a negate instruction
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.neg_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_unary_plus_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // A unary plus on a number literal is inlined as that number literal
        if let ast::Expression::Number(number_expr) = expr.argument.as_ref() {
            return self.gen_number_literal(number_expr.value, dest);
        }

        // Otherwise generate a ToNumber instruction
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.to_number_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_logical_not_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.log_not_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_bitwise_not_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.bit_not_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_typeof_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.type_of_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_void_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Void expressions are evaluated for side effects only, so simply evaluate the argument
        // and return undefined.
        let result = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(result);

        let dest = self.allocate_destination(dest)?;
        self.writer.load_undefined_instruction(dest);

        Ok(dest)
    }

    fn gen_binary_expression(
        &mut self,
        expr: &ast::BinaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let left = self.gen_expression(&expr.left)?;
        let right = self.gen_expression(&expr.right)?;

        self.register_allocator.release(right);
        self.register_allocator.release(left);
        let dest = self.allocate_destination(dest)?;

        match expr.operator {
            ast::BinaryOperator::Add => self.writer.add_instruction(dest, left, right),
            ast::BinaryOperator::Subtract => self.writer.sub_instruction(dest, left, right),
            ast::BinaryOperator::Multiply => self.writer.mul_instruction(dest, left, right),
            ast::BinaryOperator::Divide => self.writer.div_instruction(dest, left, right),
            ast::BinaryOperator::Remainder => self.writer.rem_instruction(dest, left, right),
            ast::BinaryOperator::Exponent => self.writer.exp_instruction(dest, left, right),
            ast::BinaryOperator::EqEq => self.writer.loose_equal_instruction(dest, left, right),
            ast::BinaryOperator::NotEq => {
                self.writer.loose_not_equal_instruction(dest, left, right)
            }
            ast::BinaryOperator::EqEqEq => self.writer.strict_equal_instruction(dest, left, right),
            ast::BinaryOperator::NotEqEq => {
                self.writer.strict_not_equal_instruction(dest, left, right)
            }
            ast::BinaryOperator::LessThan => self.writer.less_than_instruction(dest, left, right),
            ast::BinaryOperator::LessThanOrEqual => self
                .writer
                .less_than_or_equal_instruction(dest, left, right),
            ast::BinaryOperator::GreaterThan => {
                self.writer.greater_than_instruction(dest, left, right)
            }
            ast::BinaryOperator::GreaterThanOrEqual => self
                .writer
                .greater_than_or_equal_instruction(dest, left, right),
            ast::BinaryOperator::And => self.writer.bit_and_instruction(dest, left, right),
            ast::BinaryOperator::Or => self.writer.bit_or_instruction(dest, left, right),
            ast::BinaryOperator::Xor => self.writer.bit_xor_instruction(dest, left, right),
            ast::BinaryOperator::ShiftLeft => self.writer.shift_left_instruction(dest, left, right),
            ast::BinaryOperator::ShiftRightArithmetic => self
                .writer
                .shift_right_arithmetic_instruction(dest, left, right),
            ast::BinaryOperator::ShiftRightLogical => self
                .writer
                .shift_right_logical_instruction(dest, left, right),
            ast::BinaryOperator::In => self.writer.in_instruction(dest, right, left),
            ast::BinaryOperator::InPrivate => {
                unimplemented!("bytecode for private `in` expression")
            }
            ast::BinaryOperator::InstanceOf => {
                self.writer.instance_of_instruction(dest, left, right)
            }
        }

        Ok(dest)
    }

    fn gen_logical_expression(
        &mut self,
        expr: &ast::LogicalExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        let join_block = self.new_block();

        // If the destination is not a temporary register then we must write intermediate values
        // to a temporary register, then move that to final destionation at the end. This avoids
        // clobbering the non-temporary register if the expression throws.
        let needs_temporary_dest = !self.register_allocator.is_temporary_register(dest);
        let temporary_dest = if needs_temporary_dest {
            self.register_allocator.allocate()?
        } else {
            dest
        };

        // Generate left expression, storing in (possibly temporary) dest
        self.gen_expression_with_dest(&expr.left, ExprDest::Fixed(temporary_dest))?;

        // Write the appropriate conditional jump for each operator
        match expr.operator {
            ast::LogicalOperator::And => {
                self.write_jump_false_for_expression(&expr.left, temporary_dest, join_block)?
            }
            ast::LogicalOperator::Or => {
                self.write_jump_true_for_expression(&expr.left, temporary_dest, join_block)?;
            }
            ast::LogicalOperator::NullishCoalesce => {
                self.write_jump_not_nullish_instruction(temporary_dest, join_block)?
            }
        }

        // Generate right expression, storing in (possibly temporary) dest
        self.gen_expression_with_dest(&expr.right, ExprDest::Fixed(temporary_dest))?;

        self.start_block(join_block);

        // Move from temporary dest to final dest if necessary
        if needs_temporary_dest {
            self.write_mov_instruction(dest, temporary_dest);
            self.register_allocator.release(temporary_dest);
        }

        Ok(dest)
    }

    fn gen_conditional_expression(
        &mut self,
        expr: &ast::ConditionalExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let join_block = self.new_block();
        let altern_block = self.new_block();

        // Emit test expression and proceed to the correct block
        let test = self.gen_expression(&expr.test)?;
        self.write_jump_false_for_expression(&expr.test, test, altern_block)?;

        // Both branches place their result in the same destination register
        self.register_allocator.release(test);
        let dest = self.allocate_destination(dest)?;

        self.gen_expression_with_dest(&expr.conseq, ExprDest::Fixed(dest))?;
        self.write_jump_instruction(join_block)?;

        self.start_block(altern_block);
        self.gen_expression_with_dest(&expr.altern, ExprDest::Fixed(dest))?;

        self.start_block(join_block);

        Ok(dest)
    }

    fn gen_call_expression(
        &mut self,
        expr: &ast::CallExpression,
        dest: ExprDest,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<GenRegister> {
        // Calls on a property accesses must pass the object's value as the this value
        let (callee, this_value) = match expr.callee.as_ref() {
            // If the callee is a member expression, generate the object expression first and use
            // as the receiver.
            ast::Expression::Member(member_expr) => {
                // Dummy value that will be overwritten when generating member expression
                let mut call_receiver =
                    CallReceiver { receiver: Register::this(), is_chain: false };

                let callee = self.gen_member_expression(
                    member_expr,
                    ExprDest::Any,
                    Some(&mut call_receiver),
                    optional_nullish_block,
                )?;

                (callee, Some(call_receiver.receiver))
            }
            // If the callee is a chain member expression there will either be a receiver or the
            // chain expression short circuits and the callee is undefined.
            ast::Expression::Chain(chain_expr)
                if matches!(chain_expr.expression.as_ref(), ast::Expression::Member(_)) =>
            {
                // Dummy receiver that will be overwritten when generating member expression
                let mut call_receiver = CallReceiver { receiver: Register::this(), is_chain: true };

                let callee =
                    self.gen_chain_expression(chain_expr, ExprDest::Any, Some(&mut call_receiver))?;

                (callee, Some(call_receiver.receiver))
            }
            // Otherwise there is no receiver
            _ => {
                let callee = self.gen_maybe_chain_part_expression(
                    &expr.callee,
                    ExprDest::Any,
                    None,
                    optional_nullish_block,
                )?;
                (callee, None)
            }
        };

        // If in an optional chain, jump to the nullish block if the calle is nullish
        if expr.is_optional {
            self.write_jump_nullish_instruction(callee, optional_nullish_block.unwrap())?;
        }

        // Generate arguments into a contiguous argc + argv slice
        let (argv, argc) = self.gen_call_arguments(&expr.arguments, callee)?;

        // Release all remaining call registers
        self.register_allocator.release(callee);
        if let Some(this_value) = this_value {
            self.register_allocator.release(this_value);
        }

        let dest = self.allocate_destination(dest)?;

        if let Some(this_value) = this_value {
            self.writer
                .call_with_receiver_instruction(dest, callee, this_value, argv, argc);
        } else {
            self.writer.call_instruction(dest, callee, argv, argc);
        }

        Ok(dest)
    }

    /// Generate the bytecode for all call arguments in a contiguous range, returning the
    /// (argv, argc) pair representing the first register and the number of registers.
    ///
    /// Also takes a default argv register to use if there are no arguments.
    ///
    /// Releases all registers in the argv + argc slice before returning.
    fn gen_call_arguments(
        &mut self,
        arguments: &[ast::CallArgument],
        default_argv: GenRegister,
    ) -> EmitResult<(GenRegister, GenUInt)> {
        if arguments
            .iter()
            .any(|arg| matches!(arg, ast::CallArgument::Spread(_)))
        {
            unimplemented!("bytecode for spread arguments");
        }

        // Generate code for each argument, loading each into a new temporary register forming a
        // contiguous range of registers.
        let mut arg_regs = Vec::with_capacity(arguments.len());
        for argument in arguments {
            match argument {
                ast::CallArgument::Expression(expr) => {
                    let arg_reg = self.gen_expression_with_dest(expr, ExprDest::NewTemporary)?;
                    arg_regs.push(arg_reg);
                }
                ast::CallArgument::Spread(_) => unimplemented!("bytecode for spread arguments"),
            }
        }

        // Find first argument register, or use callee register as a placeholder if there are no
        // arguments (meaning the choice of argv register is arbitrary).
        let argv = if arg_regs.is_empty() {
            default_argv
        } else {
            arg_regs[0]
        };

        // Check that the argument registers are contiguous. This is guaranteed by the register
        // allocation strategy.
        debug_assert!(Self::are_registers_contiguous(&arg_regs));

        // Allocate a range of registers for the arguments
        let argc =
            GenUInt::try_from_unsigned(arguments.len()).ok_or(EmitError::TooManyCallArguments)?;

        // Release all call registers
        for arg_reg in arg_regs.iter().rev() {
            self.register_allocator.release(*arg_reg);
        }

        Ok((argv, argc))
    }

    fn are_registers_contiguous(registers: &[GenRegister]) -> bool {
        if registers.is_empty() {
            return true;
        }

        let first_index = registers[0].local_index();
        for (i, reg) in registers[1..].iter().enumerate() {
            if reg.local_index() != first_index + i + 1 {
                return false;
            }
        }

        true
    }

    fn gen_new_expresssion(
        &mut self,
        expr: &ast::NewExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let callee = self.gen_expression(&expr.callee)?;

        // Generate arguments into a contiguous argc + argv slice
        let (argv, argc) = self.gen_call_arguments(&expr.arguments, callee)?;

        // Release remaining call registers before allocating dest
        self.register_allocator.release(callee);
        let dest = self.allocate_destination(dest)?;

        // For new expressions, new.target is set to the callee
        self.writer
            .construct_instruction(dest, callee, callee, argv, argc);

        Ok(dest)
    }

    fn gen_sequence_expression(
        &mut self,
        expr: &ast::SequenceExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // All expressions except the last are evaluated for side effects only
        for i in 0..expr.expressions.len() - 1 {
            let result = self.gen_expression(&expr.expressions[i])?;
            self.register_allocator.release(result);
        }

        // Value of the last expression is value of the entire sequence expression
        self.gen_expression_with_dest(expr.expressions.last().unwrap(), dest)
    }

    fn gen_array_literal(
        &mut self,
        expr: &ast::ArrayExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let array = self.allocate_destination(dest)?;
        self.writer.new_array_instruction(array);

        // Fast path for empty arrays
        if expr.elements.is_empty() {
            return Ok(array);
        }

        // Keep an index register that is incremented for each element (if there is a next element)
        let num_elements = expr.elements.len();
        let index = self.register_allocator.allocate()?;
        self.writer.load_immediate_instruction(index, SInt::new(0));

        for (i, element) in expr.elements.iter().enumerate() {
            match element {
                ast::ArrayElement::Expression(expr) => {
                    let value = self.gen_expression(expr)?;
                    self.register_allocator.release(value);

                    self.writer.set_property_instruction(array, index, value);

                    if i != num_elements - 1 {
                        self.writer.inc_instruction(index);
                    }
                }
                ast::ArrayElement::Hole => {
                    // Holes are represented as storing the empty value
                    let value = self.register_allocator.allocate()?;
                    self.writer.load_empty_instruction(value);
                    self.register_allocator.release(value);

                    self.writer.set_property_instruction(array, index, value);

                    if i != num_elements - 1 {
                        self.writer.inc_instruction(index);
                    }
                }
                ast::ArrayElement::Spread(_) => {
                    unimplemented!("bytecode for array spread elements")
                }
            }
        }

        self.register_allocator.release(index);

        Ok(array)
    }

    fn gen_object_literal(
        &mut self,
        expr: &ast::ObjectExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let object = self.allocate_destination(dest)?;
        self.writer.new_object_instruction(object);

        for property in &expr.properties {
            if let ast::PropertyKind::Spread(_) = property.kind {
                unimplemented!("bytecode for object spread elements")
            }

            enum Property<'a> {
                Computed(GenRegister),
                Named {
                    constant_index: GenConstantIndex,
                    name: AnyStr<'a>,
                    is_proto: bool,
                },
            }

            // Evaluate property name
            let key = if property.is_computed {
                Property::Computed(self.gen_expression(&property.key)?)
            } else {
                match property.key.as_ref() {
                    ast::Expression::Id(id) => {
                        let constant_index = self.add_string_constant(&id.name)?;
                        let name = AnyStr::from_id(id);
                        let is_proto = id.name == "__proto__";

                        Property::Named { constant_index, name, is_proto }
                    }
                    ast::Expression::String(string) => {
                        let constant_index = self.add_wtf8_string_constant(&string.value)?;
                        let name = AnyStr::Wtf8(&string.value);
                        let is_proto = string.value == "__proto__";

                        Property::Named { constant_index, name, is_proto }
                    }
                    ast::Expression::Number(_) | ast::Expression::BigInt(_) => {
                        Property::Computed(self.gen_expression(&property.key)?)
                    }
                    _ => unreachable!("invalid property key"),
                }
            };

            let mut needs_name = false;

            let value = if property.value.is_none() {
                // Identifier shorthand properties
                let value_id = property.key.as_ref().to_id();
                self.gen_load_identifier(value_id, ExprDest::Any)?
            } else if property.is_method {
                // Method properties
                unimplemented!("bytecode for object literal method properties")
            } else if let Property::Named { is_proto: true, .. } = &key {
                unimplemented!("bytecode for __proto__ property");
            } else {
                // Regular key-value properties. Value is statically evaluated as named if the key
                // is statically known, otherwise the DefinePropertyInstruction will handle setting
                // the name of the value at runtime if necessary.
                match &key {
                    Property::Named { name, .. } => self.gen_named_expression(
                        *name,
                        property.value.as_ref().unwrap(),
                        ExprDest::Any,
                    )?,
                    Property::Computed(_) => {
                        let value_expr = property.value.as_ref().unwrap();
                        needs_name = Self::expression_needs_name(value_expr);

                        self.gen_expression_with_dest(value_expr, ExprDest::Any)?
                    }
                }
            };

            self.register_allocator.release(value);

            // Emit the correct define property instruction depending on whether the property is
            // a statically known string.
            match key {
                Property::Computed(key) => {
                    let needs_name_flag = if needs_name {
                        UInt::new(1)
                    } else {
                        UInt::new(0)
                    };

                    self.writer
                        .define_property_instruction(object, key, value, needs_name_flag);
                    self.register_allocator.release(key);
                }
                Property::Named { constant_index, .. } => {
                    self.writer
                        .define_named_property_instruction(object, constant_index, value);
                }
            }
        }

        Ok(object)
    }

    /// Generate a member expression. Write the object's register to the `call_receiver` register if
    /// one is provided, if evaluating a callee. Otherwise release the object's register.
    fn gen_member_expression(
        &mut self,
        expr: &ast::MemberExpression,
        dest: ExprDest,
        mut call_receiver: Option<&mut CallReceiver>,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<GenRegister> {
        let mut object = self.gen_maybe_chain_part_expression(
            &expr.object,
            ExprDest::Any,
            None,
            optional_nullish_block,
        )?;

        // Save the object register in the CallReceiver if one is provided
        if let Some(call_receiver) = call_receiver.as_deref_mut() {
            // A chain expression could have short circuited and will have undefined written to the
            // reciever register. Make sure that the receiver is a temporary register to avoid
            // overwriting a fixed register.
            if call_receiver.is_chain {
                object = self.gen_ensure_reg_is_temporary(object)?;
            }

            call_receiver.receiver = object;
        }

        // If in an optional chain, check for nullish and jump to the nullish block if necessary
        if expr.is_optional {
            self.write_jump_nullish_instruction(object, optional_nullish_block.unwrap())?;
        }

        let result = self.gen_member_property(
            expr,
            object,
            dest,
            /* release_object */ call_receiver.is_none(),
        )?;

        Ok(result)
    }

    fn gen_member_property(
        &mut self,
        expr: &ast::MemberExpression,
        object: GenRegister,
        dest: ExprDest,
        release_object: bool,
    ) -> EmitResult<GenRegister> {
        if expr.is_private {
            unimplemented!("bytecode for private member expressions");
        }

        let dest = if expr.is_computed {
            let key = self.gen_expression(&expr.property)?;

            self.register_allocator.release(key);
            if release_object {
                self.register_allocator.release(object);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer.get_property_instruction(dest, object, key);

            dest
        } else {
            // Must be a named access
            let name = expr.property.to_id();
            let name_constant_index = self.add_string_constant(&name.name)?;

            if release_object {
                self.register_allocator.release(object);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer
                .get_named_property_instruction(dest, object, name_constant_index);

            dest
        };

        Ok(dest)
    }

    /// Generate an optional expression, writing the result to dest.
    ///
    /// Takes an optional `call_receiver` which is where the outermost expression's object register
    /// will be written if it is a member expression, if evaluating a callee.
    fn gen_chain_expression(
        &mut self,
        expr: &ast::ChainExpression,
        dest: ExprDest,
        mut call_receiver: Option<&mut CallReceiver>,
    ) -> EmitResult<GenRegister> {
        let join_block = self.new_block();
        let nullish_block = self.new_block();

        // Generate the inner expression, writing to destination register if successful
        let result = self.gen_maybe_chain_part_expression(
            &expr.expression,
            dest,
            call_receiver.as_deref_mut(),
            Some(nullish_block),
        )?;

        self.write_jump_instruction(join_block)?;

        // If the inner expression ever short circuits due to a nullish value, write undefined to
        // the destination register.
        self.start_block(nullish_block);
        self.writer.load_undefined_instruction(result);

        // If there is a receiver ref, meaning this chain is followed by a call, then write to the
        // receiver register so that it has a valid value to be passed to the call.
        if let Some(call_receiver) = call_receiver {
            self.writer
                .load_undefined_instruction(call_receiver.receiver);
        }

        self.start_block(join_block);
        Ok(result)
    }

    /// Generate an expression that may be part of an optional chain, propagating the optional
    /// chaining nullish block to expressions that may short circuit.
    ///
    /// Takes an optional `call_receiver` which is where the outermost expression's object register
    /// will be written if it is a member expression, if evaluating a callee.
    fn gen_maybe_chain_part_expression(
        &mut self,
        expr: &ast::Expression,
        dest: ExprDest,
        call_receiver: Option<&mut CallReceiver>,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<GenRegister> {
        match expr {
            ast::Expression::Member(member_expr) => {
                self.gen_member_expression(member_expr, dest, call_receiver, optional_nullish_block)
            }
            ast::Expression::Call(call_expr) => {
                self.gen_call_expression(call_expr, dest, optional_nullish_block)
            }
            _ => self.gen_expression_with_dest(expr, dest),
        }
    }

    fn gen_assignment_expression(
        &mut self,
        expr: &ast::AssignmentExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match expr.left.as_ref() {
            ast::Pattern::Id(id) => {
                // Right side expression is only named (meaning name is used for anonymous functions
                // and classes) if it is a simple id assignment and id is not parenthesized.
                let is_non_parenthesized_id_predicate = || {
                    id.loc.start == expr.loc.start
                        && expr.operator == ast::AssignmentOperator::Equals
                };

                let stored_value_dest = self.expr_dest_for_id(id);

                let stored_value = if expr.operator == ast::AssignmentOperator::Equals {
                    // For simple assignments, right hand side is placed directly in the dest
                    // register.
                    self.gen_named_expression_if(
                        AnyStr::from_id(id),
                        &expr.right,
                        stored_value_dest,
                        is_non_parenthesized_id_predicate,
                    )?
                } else {
                    // For operator assignments the old value and right value are loaded to
                    // temporary registers, with only the result placed in the dest.
                    let old_value = self.gen_load_identifier(id, ExprDest::Any)?;
                    let right_value = self.gen_named_expression_if(
                        AnyStr::from_id(id),
                        &expr.right,
                        ExprDest::Any,
                        is_non_parenthesized_id_predicate,
                    )?;

                    self.register_allocator.release(right_value);
                    self.register_allocator.release(old_value);
                    let stored_value = self.allocate_destination(stored_value_dest)?;

                    self.gen_assignment_operator(
                        expr.operator,
                        stored_value,
                        old_value,
                        right_value,
                    );

                    stored_value
                };

                self.gen_store_identifier(id, stored_value)?;
                self.gen_mov_reg_to_dest(stored_value, dest)
            }
            ast::Pattern::Reference(ast::Expression::Member(member)) => {
                if member.is_private {
                    unimplemented!("bytecode for assigning private members");
                }

                enum Property {
                    Computed(GenRegister),
                    Named(GenConstantIndex),
                }

                // The right value should be placed in the destination register, since the right
                // value is returned as the value of the entire assignment expression.
                let dest = self.allocate_destination(dest)?;

                let object = self.gen_expression(&member.object)?;

                // Emit the property itself, which may be a computed expression or literal name
                let property = if member.is_computed {
                    let key = self.gen_expression(&member.property)?;
                    Property::Computed(key)
                } else {
                    // Must be a named access
                    let name = member.property.to_id();
                    let name_constant_index = self.add_string_constant(&name.name)?;
                    Property::Named(name_constant_index)
                };

                if expr.operator == ast::AssignmentOperator::Equals {
                    // For simple assignments, right hand side is placed directly in the dest
                    // register.
                    self.gen_expression_with_dest(&expr.right, ExprDest::Fixed(dest))?;
                } else {
                    // For operator assignments the old value is placed in the dest register, then
                    // overwritten with the result of the operator.
                    match property {
                        Property::Computed(key) => {
                            self.writer.get_property_instruction(dest, object, key)
                        }
                        Property::Named(name_constant_index) => self
                            .writer
                            .get_named_property_instruction(dest, object, name_constant_index),
                    }

                    let right_value = self.gen_expression(&expr.right)?;

                    self.gen_assignment_operator(expr.operator, dest, dest, right_value);

                    self.register_allocator.release(right_value);
                }

                // Write the value back to the property
                match property {
                    Property::Computed(key) => {
                        self.writer.set_property_instruction(object, key, dest);
                        self.register_allocator.release(key);
                    }
                    Property::Named(name_constant_index) => {
                        self.writer.set_named_property_instruction(
                            object,
                            name_constant_index,
                            dest,
                        );
                    }
                }

                self.register_allocator.release(object);

                Ok(dest)
            }
            ast::Pattern::Array(_) | ast::Pattern::Object(_) => {
                unimplemented!("bytecode for assignment expression destructuring")
            }
            ast::Pattern::Reference(_) | ast::Pattern::Assign(_) => {
                unreachable!("invalid assigment left hand side")
            }
        }
    }

    fn gen_assignment_operator(
        &mut self,
        operator: ast::AssignmentOperator,
        dest: GenRegister,
        left: GenRegister,
        right: GenRegister,
    ) {
        match operator {
            ast::AssignmentOperator::Add => self.writer.add_instruction(dest, left, right),
            ast::AssignmentOperator::Subtract => self.writer.sub_instruction(dest, left, right),
            ast::AssignmentOperator::Multiply => self.writer.mul_instruction(dest, left, right),
            ast::AssignmentOperator::Divide => self.writer.div_instruction(dest, left, right),
            ast::AssignmentOperator::Remainder => self.writer.rem_instruction(dest, left, right),
            ast::AssignmentOperator::Exponent => self.writer.exp_instruction(dest, left, right),
            ast::AssignmentOperator::And => self.writer.bit_and_instruction(dest, left, right),
            ast::AssignmentOperator::Or => self.writer.bit_or_instruction(dest, left, right),
            ast::AssignmentOperator::Xor => self.writer.bit_xor_instruction(dest, left, right),
            ast::AssignmentOperator::ShiftLeft => {
                self.writer.shift_left_instruction(dest, left, right)
            }
            ast::AssignmentOperator::ShiftRightArithmetic => self
                .writer
                .shift_right_arithmetic_instruction(dest, left, right),
            ast::AssignmentOperator::ShiftRightLogical => self
                .writer
                .shift_right_logical_instruction(dest, left, right),
            ast::AssignmentOperator::LogicalAnd => {
                unimplemented!("bytecode for logical and assignment")
            }
            ast::AssignmentOperator::LogicalOr => {
                unimplemented!("bytecode for logical or assignment")
            }
            ast::AssignmentOperator::NullishCoalesce => {
                unimplemented!("bytecode for nullish coalescing assignment")
            }
            ast::AssignmentOperator::Equals => unreachable!("bytecode for simple assignment"),
        }
    }

    fn gen_update_expression(
        &mut self,
        expr: &ast::UpdateExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        if let ast::Expression::Member(member) = expr.argument.as_ref() {
            if member.is_private {
                unimplemented!("bytecode for updating a private member");
            }

            enum Property {
                Computed(GenRegister),
                Named(GenConstantIndex),
            }

            let return_value = self.allocate_destination(dest)?;
            let object = self.gen_expression(&member.object)?;

            // Prefix operations can return the result of the conversion and inc/dec so they don't
            // need a scratch register. Postfix operations must save the old value as the return
            // value so they need a separate scratch register.
            let scratch_value = if expr.is_prefix {
                return_value
            } else {
                self.register_allocator.allocate()?
            };

            // Load the property to the scratch register
            let property = if member.is_computed {
                let key = self.gen_expression(&member.property)?;
                self.writer
                    .get_property_instruction(scratch_value, object, key);

                Property::Computed(key)
            } else {
                // Must be a named access
                let name = member.property.to_id();
                let name_constant_index = self.add_string_constant(&name.name)?;
                self.writer.get_named_property_instruction(
                    scratch_value,
                    object,
                    name_constant_index,
                );

                Property::Named(name_constant_index)
            };

            // Perform the converstion and inc/dec operation in place on the scratch register
            self.writer
                .to_numeric_instruction(scratch_value, scratch_value);

            // Postfix operations save the old value to be returned later
            if !expr.is_prefix {
                self.write_mov_instruction(return_value, scratch_value);
            }

            self.write_inc_or_dec(expr.operator, scratch_value);

            // Then write scratch register back to the property
            match property {
                Property::Computed(key) => {
                    self.writer
                        .set_property_instruction(object, key, scratch_value);
                    self.register_allocator.release(key);
                }
                Property::Named(name_constant_index) => {
                    self.writer.set_named_property_instruction(
                        object,
                        name_constant_index,
                        scratch_value,
                    );
                }
            }

            // Release the scratch register (if separate from the return register)
            if !expr.is_prefix {
                self.register_allocator.release(scratch_value);
            }
            self.register_allocator.release(object);

            Ok(return_value)
        } else {
            // Otherwise must be an id assignment
            let id = expr.argument.to_id();

            if expr.is_prefix {
                // Prefix operations return the modified value so we can perform operations in place
                let old_value = self.gen_load_identifier(id, ExprDest::Any)?;

                if self.register_allocator.is_temporary_register(old_value) {
                    // If the target value is in a temporary register then we can place the numeric
                    // value directly in the return register and perform operations in place.
                    self.register_allocator.release(old_value);
                    let dest = self.allocate_destination(dest)?;

                    self.writer.to_numeric_instruction(dest, old_value);
                    self.write_inc_or_dec(expr.operator, dest);
                    self.gen_store_identifier(id, dest)?;

                    Ok(dest)
                } else {
                    // If the target value is in a param or non-temporary local register then can
                    // perform all operations in place.
                    self.writer.to_numeric_instruction(old_value, old_value);
                    self.write_inc_or_dec(expr.operator, old_value);
                    self.gen_mov_reg_to_dest(old_value, dest)
                }
            } else {
                // Postfix operations return the old value, so we must make sure it is saved and
                // not clobbered.
                let dest = self.allocate_destination(dest)?;
                let old_value = self.gen_load_identifier(id, ExprDest::Any)?;
                self.writer.to_numeric_instruction(old_value, old_value);

                // If id is at a fixed register which matches the destination then writing the
                // modified value to the id's location would clobber the old value. But in this case
                // the desired behavior is to not actually increment/decrement the value. We just
                // need to perform tje in-place numeric conversion.
                if let ExprDest::Fixed(fixed_id_reg) = self.expr_dest_for_id(id) {
                    if fixed_id_reg == dest {
                        return Ok(dest);
                    }
                }

                // Save the old value to be returned later
                self.write_mov_instruction(dest, old_value);

                // Otherwise we are guaranteed that writing the modified value to the id's location
                // will not clobber the old value. Perform the inc/dec at the id's location.
                self.write_inc_or_dec(expr.operator, old_value);

                self.register_allocator.release(old_value);
                self.gen_store_identifier(id, old_value)?;

                Ok(dest)
            }
        }
    }

    fn write_inc_or_dec(&mut self, operator: ast::UpdateOperator, value: GenRegister) {
        match operator {
            ast::UpdateOperator::Increment => self.writer.inc_instruction(value),
            ast::UpdateOperator::Decrement => self.writer.dec_instruction(value),
        }
    }

    fn gen_named_expression(
        &mut self,
        name: AnyStr,
        expr: &ast::Expression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        self.gen_named_expression_if(name, expr, dest, || true)
    }

    fn gen_named_expression_if(
        &mut self,
        name: AnyStr,
        expr: &ast::Expression,
        dest: ExprDest,
        if_predicate: impl Fn() -> bool,
    ) -> EmitResult<GenRegister> {
        if !if_predicate() {
            return self.gen_expression_with_dest(expr, dest);
        }

        match expr {
            ast::Expression::Function(func) if func.id.is_none() => {
                self.gen_function_expression(func, Some(name.to_owned()), dest)
            }
            ast::Expression::ArrowFunction(func) => {
                self.gen_arrow_function_expression(func, Some(name.to_owned()), dest)
            }
            _ => self.gen_expression_with_dest(expr, dest),
        }
    }

    fn expression_needs_name(expr: &ast::Expression) -> bool {
        match expr {
            ast::Expression::Function(func) if func.id.is_none() => true,
            ast::Expression::ArrowFunction(_) => true,
            ast::Expression::Class(class) if class.id.is_none() => true,
            _ => false,
        }
    }

    fn gen_function_expression(
        &mut self,
        func: &ast::Function,
        name: Option<Wtf8String>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let pending_node = PendingFunctionNode::Expression { node: AstPtr::from_ref(func), name };
        let func_constant_index = self.enqueue_function_to_generate(pending_node)?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .new_closure_instruction(dest, ConstantIndex::new(func_constant_index));

        Ok(dest)
    }

    fn gen_arrow_function_expression(
        &mut self,
        func: &ast::Function,
        name: Option<Wtf8String>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let pending_node = PendingFunctionNode::Arrow { node: AstPtr::from_ref(func), name };
        let func_constant_index = self.enqueue_function_to_generate(pending_node)?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .new_closure_instruction(dest, ConstantIndex::new(func_constant_index));

        Ok(dest)
    }

    fn gen_variable_declaration(
        &mut self,
        var_decl: &ast::VariableDeclaration,
    ) -> EmitResult<StmtCompletion> {
        if var_decl.kind != ast::VarKind::Var {
            unimplemented!("bytecode for const and let declarations");
        }

        for decl in &var_decl.declarations {
            if let Some(init) = decl.init.as_deref() {
                match decl.id.as_ref() {
                    ast::Pattern::Id(id) => {
                        let init_value_dest = self.expr_dest_for_id(id);
                        let init_value =
                            self.gen_named_expression(AnyStr::from_id(id), init, init_value_dest)?;

                        self.gen_store_identifier(id, init_value)?;
                        self.register_allocator.release(init_value);
                    }
                    _ => unimplemented!("bytecode for variable destructuring patterns"),
                }
            }
        }

        Ok(StmtCompletion::Normal)
    }

    fn gen_function_declaraton(&mut self, func_decl: &ast::Function) -> EmitResult<StmtCompletion> {
        let func_constant_index = self.enqueue_function_to_generate(
            PendingFunctionNode::Declaration(AstPtr::from_ref(func_decl)),
        )?;

        // Create a new closure, directly into binding location if possible
        let closure_dest = if let Some(id) = func_decl.id.as_ref() {
            self.expr_dest_for_id(id)
        } else {
            ExprDest::Any
        };
        let closure_reg = self.allocate_destination(closure_dest)?;

        self.writer
            .new_closure_instruction(closure_reg, ConstantIndex::new(func_constant_index));

        // And store at the binding's location
        if let Some(id) = func_decl.id.as_ref() {
            self.gen_store_identifier(id, closure_reg)?;
        }

        self.register_allocator.release(closure_reg);

        Ok(StmtCompletion::Normal)
    }

    fn gen_expression_statement(
        &mut self,
        stmt: &ast::ExpressionStatement,
    ) -> EmitResult<StmtCompletion> {
        let result = self.gen_expression(&stmt.expr)?;
        self.register_allocator.release(result);
        Ok(StmtCompletion::Normal)
    }

    fn gen_block_statement(&mut self, stmt: &ast::Block) -> EmitResult<StmtCompletion> {
        self.gen_statement_list(&stmt.body)
    }

    fn gen_statement_list(&mut self, stmts: &[ast::Statement]) -> EmitResult<StmtCompletion> {
        for stmt in stmts {
            let completion = self.gen_statement(stmt)?;

            // An abrupt completion signals the end of the statement list
            if completion.is_abrupt() {
                return Ok(completion);
            }
        }

        Ok(StmtCompletion::Normal)
    }

    fn gen_if_statement(&mut self, stmt: &ast::IfStatement) -> EmitResult<StmtCompletion> {
        let condition = self.gen_expression(&stmt.test)?;
        self.register_allocator.release(condition);

        if stmt.altern.is_none() {
            let join_block = self.new_block();

            // If there is no alternative, branch between consequent and join block
            self.write_jump_false_for_expression(&stmt.test, condition, join_block)?;

            self.gen_statement(&stmt.conseq)?;
            self.start_block(join_block);

            // If there is no alternative then the statement always can complete normally, with
            // execution continuing afterwards.
            Ok(StmtCompletion::Normal)
        } else {
            let altern_block = self.new_block();
            let join_block = self.new_block();

            // If there is an alternative, branch between consequent and alternative blocks before
            // joining at the join block.
            self.write_jump_false_for_expression(&stmt.test, condition, altern_block)?;

            let conseq_completion = self.gen_statement(&stmt.conseq)?;
            if !conseq_completion.is_abrupt() {
                self.write_jump_instruction(join_block)?;
            }

            self.start_block(altern_block);
            let altern_completion = self.gen_statement(&stmt.altern.as_ref().unwrap())?;

            self.start_block(join_block);

            Ok(conseq_completion.combine(altern_completion))
        }
    }

    /// Return whether this expression is guaranteed to evaluate to a boolean. Used to elide
    /// unnecessary ToBoolean conversions.
    fn evaluates_to_boolean(&mut self, expr: &ast::Expression) -> bool {
        match expr {
            ast::Expression::Boolean(_) => true,
            ast::Expression::Unary(unary) => unary.operator == ast::UnaryOperator::LogicalNot,
            ast::Expression::Binary(binary) => match binary.operator {
                ast::BinaryOperator::EqEq
                | ast::BinaryOperator::NotEq
                | ast::BinaryOperator::EqEqEq
                | ast::BinaryOperator::NotEqEq
                | ast::BinaryOperator::LessThan
                | ast::BinaryOperator::LessThanOrEqual
                | ast::BinaryOperator::GreaterThan
                | ast::BinaryOperator::GreaterThanOrEqual => true,
                _ => false,
            },
            ast::Expression::Logical(logical) => match logical.operator {
                ast::LogicalOperator::And | ast::LogicalOperator::Or => {
                    self.evaluates_to_boolean(&logical.left)
                        && self.evaluates_to_boolean(&logical.right)
                }
                ast::LogicalOperator::NullishCoalesce => false,
            },
            _ => false,
        }
    }

    fn gen_for_statement(
        &mut self,
        stmt: &ast::ForStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, true));

        let loop_start_block = self.new_block();
        let update_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Evaluate the init expression once before loop starts
        match stmt.init.as_deref() {
            Some(ast::ForInit::VarDecl(var_decl)) => {
                self.gen_variable_declaration(var_decl)?;
            }
            Some(ast::ForInit::Expression(expr)) => {
                let result = self.gen_expression(expr)?;
                self.register_allocator.release(result);
            }
            None => {}
        }

        self.start_block(loop_start_block);

        // Evaluate the test expression and either continue to body or break out of loop
        if let Some(test_expr) = stmt.test.as_deref() {
            let test = self.gen_expression(test_expr)?;
            self.register_allocator.release(test);

            self.write_jump_false_for_expression(test_expr, test, join_block)?;
        }

        // Evaluate the loop body
        self.gen_statement(&stmt.body)?;

        // Evaluate the update expression and return to the beginning of the loop. Always emitted
        // even if loop body has an abrupt completion since there may be a continue.
        self.start_block(update_block);
        if let Some(update_expr) = stmt.update.as_deref() {
            let update = self.gen_expression(update_expr)?;
            self.register_allocator.release(update);
        }

        self.write_jump_instruction(loop_start_block)?;

        self.start_block(join_block);

        // Normal completion since there is always the test false path that skips the loop entirely
        Ok(StmtCompletion::Normal)
    }

    fn gen_while_statement(
        &mut self,
        stmt: &ast::WhileStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, true));

        let loop_start_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Evaluate test and either continue to body or break out of loop
        self.start_block(loop_start_block);
        let test = self.gen_expression(&stmt.test)?;
        self.register_allocator.release(test);

        self.write_jump_false_for_expression(&stmt.test, test, join_block)?;

        let body_completion = self.gen_statement(&stmt.body)?;

        // Always jump back to the condition at the start of the loop
        if !body_completion.is_abrupt() {
            self.write_jump_instruction(loop_start_block)?;
        }

        self.start_block(join_block);

        // Normal completion since there is always the test false path that skips the loop entirely
        Ok(StmtCompletion::Normal)
    }

    fn gen_do_while_statement(
        &mut self,
        stmt: &ast::DoWhileStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, true));

        let loop_start_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Execute the body at the start of the loop
        self.start_block(loop_start_block);
        let body_completion = self.gen_statement(&stmt.body)?;

        // Then evaluate test and either break out of loop or continue to next iteration
        if !body_completion.is_abrupt() {
            let test = self.gen_expression(&stmt.test)?;
            self.register_allocator.release(test);
            self.write_jump_true_for_expression(&stmt.test, test, loop_start_block)?;
        }

        self.start_block(join_block);

        // Normal completion since even with an abnormal body there might be a continue statement
        // that proceeds past the loop.
        Ok(StmtCompletion::Normal)
    }

    fn gen_try_statement(&mut self, stmt: &ast::TryStatement) -> EmitResult<StmtCompletion> {
        // Generate the body of the try statement, marking the range of instructions that should
        // be covered by the body handler.
        let body_handler_start = self.writer.current_offset();
        let body_completion = self.gen_block_statement(&stmt.block)?;
        let body_handler_end = self.writer.current_offset();

        let join_block = self.new_block();
        let catch_block = stmt.handler.as_ref().map(|_| self.new_block());
        let finally_block = stmt.finalizer.as_ref().map(|_| self.new_block());

        // Body continues to finalizer if one exists, otherwise it continues to the join block.
        // Only need to write a jump if catch is present, otherwise will directly continue to
        // finally block.
        if !body_completion.is_abrupt() && catch_block.is_some() {
            let after_body_block = finally_block.unwrap_or(join_block);
            self.write_jump_instruction(after_body_block)?;
        }

        let mut body_handler = ExceptionHandlerBuilder::new(body_handler_start, body_handler_end);
        let mut catch_handler = None;
        let mut result_completion = body_completion;

        // Emit the catch clause
        if let Some(catch_clause) = &stmt.handler {
            body_handler.handler = self.writer.current_offset();

            // If there is a catch parameter, mark the register in the handler
            if let Some(param) = catch_clause.param.as_deref() {
                if param.is_id() {
                    let id = param.to_id();
                    let scope = id.scope.unwrap();
                    let binding = scope.as_ref().get_binding(&id.name);

                    match binding.vm_location().unwrap() {
                        VMLocation::LocalRegister(index) => {
                            body_handler.error_register = Some(Register::local(index))
                        }
                        VMLocation::Scope { .. } => {
                            unimplemented!("bytecode for captured catch parameters")
                        }
                        _ => unreachable!("catch parameters must be in a register or VM scope"),
                    }
                } else {
                    unimplemented!("bytecode for catch parameter destructuring");
                }
            }

            // Emit the catch block's body. No need to write a jump from catch to next block, since
            // either the finally or join block will be emitted directly after the catch.
            self.start_block(catch_block.unwrap());

            let catch_handler_start = self.writer.current_offset();
            let catch_completion = self.gen_block_statement(&catch_clause.body)?;
            let catch_handler_end = self.writer.current_offset();

            // If there is a finally block then the catch block must be wrapped in an exception
            // handler.
            if stmt.finalizer.is_some() {
                let handler = ExceptionHandlerBuilder::new(catch_handler_start, catch_handler_end);
                catch_handler = Some(handler)
            } else {
                // Without a finally block, result completion is abrupt if both body and catch
                // blocks have an abrupt completion.
                result_completion = result_completion.combine(catch_completion);
            }
        }

        // Emit the finally block
        if let Some(finally) = &stmt.finalizer {
            // The finally block is the target handler for either the body or the catch block if
            // one is present.
            if let Some(catch_handler) = &mut catch_handler {
                catch_handler.handler = self.writer.current_offset();
            } else {
                body_handler.handler = self.writer.current_offset();
            }

            // Emit the finally block's body. No need to write a jump from finally to join block
            // since it immediately follows.
            self.start_block(finally_block.unwrap());
            let finally_completion = self.gen_block_statement(finally)?;

            // Finally completion used as result completion since all paths go through finally block
            result_completion = finally_completion;
        }

        if stmt.finalizer.is_some() {
            // TODO: Finally blocks must allow multiple entry points and successors, as they may
            // be visited from the try body, catch body, or any abrupt completions of either.
            unimplemented!("bytecode for finally blocks");
        }

        // Save the exception handlers
        self.exception_handler_builder.add(body_handler);
        if let Some(catch_handler) = catch_handler {
            self.exception_handler_builder.add(catch_handler);
        }

        self.start_block(join_block);
        Ok(result_completion)
    }

    fn gen_throw_statement(&mut self, stmt: &ast::ThrowStatement) -> EmitResult<StmtCompletion> {
        let error = self.gen_expression(&stmt.argument)?;
        self.writer.throw_instruction(error);
        self.register_allocator.release(error);

        Ok(StmtCompletion::Abrupt)
    }

    fn gen_return_statement(&mut self, stmt: &ast::ReturnStatement) -> EmitResult<StmtCompletion> {
        if stmt.argument.is_none() {
            self.gen_return_undefined()?;
            return Ok(StmtCompletion::Abrupt);
        }

        let return_arg = self.gen_expression(stmt.argument.as_ref().unwrap())?;
        self.writer.ret_instruction(return_arg);
        self.register_allocator.release(return_arg);

        Ok(StmtCompletion::Abrupt)
    }

    fn gen_return_undefined(&mut self) -> EmitResult<()> {
        let return_arg = self.register_allocator.allocate()?;
        self.writer.load_undefined_instruction(return_arg);
        self.writer.ret_instruction(return_arg);
        self.register_allocator.release(return_arg);

        Ok(())
    }

    fn gen_labeled_statement(
        &mut self,
        stmt: &ast::LabeledStatement,
    ) -> EmitResult<StmtCompletion> {
        // Find the innermost labeled statement
        let mut inner_stmt = stmt;
        while let ast::Statement::Labeled(labeled_stmt) = inner_stmt.body.as_ref() {
            inner_stmt = labeled_stmt;
        }

        // All nested labels share the same id
        let label_id = inner_stmt.label.id;

        let completion = match inner_stmt.body.as_ref() {
            ast::Statement::For(stmt) => {
                let jump_targets = self.push_jump_statement_target(Some(label_id), true);
                self.gen_for_statement(stmt, Some(jump_targets))
            }
            ast::Statement::ForEach(_) => unimplemented!("bytecode for for each statements"),
            ast::Statement::While(stmt) => {
                let jump_targets = self.push_jump_statement_target(Some(label_id), true);
                self.gen_while_statement(stmt, Some(jump_targets))
            }
            ast::Statement::DoWhile(stmt) => {
                let jump_targets = self.push_jump_statement_target(Some(label_id), true);
                self.gen_do_while_statement(stmt, Some(jump_targets))
            }
            stmt => {
                // Do not generate a continue block for labeled non-loop statements
                let jump_targets = self.push_jump_statement_target(Some(label_id), false);
                let completion = self.gen_statement(stmt);

                // The break target is after the labeled statement
                self.start_block(jump_targets.break_block);

                completion
            }
        };

        self.pop_jump_statement_target();

        completion
    }

    fn push_jump_statement_target(
        &mut self,
        label_id: Option<ast::LabelId>,
        create_continue_block: bool,
    ) -> JumpStatementTarget {
        let break_block = self.new_block();
        let continue_block = if create_continue_block {
            self.new_block()
        } else {
            0
        };

        self.jump_statement_target_stack.push(JumpStatementTarget {
            label_id,
            break_block,
            continue_block,
        });

        self.jump_statement_target_stack.last().unwrap().clone()
    }

    fn pop_jump_statement_target(&mut self) {
        self.jump_statement_target_stack.pop();
    }

    /// Lookup the innermost jump statement target, or the target that matches the label id if one
    /// is provided.
    fn lookup_jump_statement_target(&self, label: Option<&ast::Label>) -> &JumpStatementTarget {
        if let Some(label) = label {
            self.jump_statement_target_stack
                .iter()
                .rev()
                .find(|target| target.label_id == Some(label.id))
                .unwrap()
        } else {
            self.jump_statement_target_stack.last().unwrap()
        }
    }

    fn gen_break_statement(&mut self, stmt: &ast::BreakStatement) -> EmitResult<StmtCompletion> {
        let target = self.lookup_jump_statement_target(stmt.label.as_ref());
        self.write_jump_instruction(target.break_block)?;
        Ok(StmtCompletion::Abrupt)
    }

    fn gen_continue_statement(
        &mut self,
        stmt: &ast::ContinueStatement,
    ) -> EmitResult<StmtCompletion> {
        let target = self.lookup_jump_statement_target(stmt.label.as_ref());
        self.write_jump_instruction(target.continue_block)?;
        Ok(StmtCompletion::Abrupt)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EmitError {
    ConstantTableTooLarge,
    TooManyFunctionParameters,
    TooManyCallArguments,
    TooManyRegisters,
}

impl Error for EmitError {}

impl fmt::Display for EmitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EmitError::ConstantTableTooLarge => write!(f, "Constant table too large"),
            EmitError::TooManyFunctionParameters => write!(f, "Too many function parameters"),
            EmitError::TooManyCallArguments => write!(f, "Too many call arguments"),
            EmitError::TooManyRegisters => write!(f, "Too many registers"),
        }
    }
}

pub type EmitResult<T> = Result<T, EmitError>;

impl<T: Escapable> Escapable for EmitResult<T> {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        match self {
            Ok(ok) => Ok(ok.escape(cx)),
            Err(err) => Err(*err),
        }
    }
}

type BlockId = usize;

struct ForwardJumpInfo {
    /// Offset of the jump instruction (including prefixes) from the start of the bytecode.
    jump_offset: usize,
    /// Width reserved in the constant table.
    constant_table_width: WidthEnum,
}

// Type aliases for operands referenced in the generator. These operands have the max width, and
// will be narrowed when possible when writing individual instructions.
pub type GenRegister = Register<ExtraWide>;
type GenUInt = UInt<ExtraWide>;
type GenSInt = SInt<ExtraWide>;
type GenConstantIndex = ConstantIndex<ExtraWide>;

enum PendingFunctionNode {
    Declaration(AstPtr<ast::Function>),
    Expression { node: AstPtr<ast::Function>, name: Option<Wtf8String> },
    Arrow { node: AstPtr<ast::Function>, name: Option<Wtf8String> },
}

impl PendingFunctionNode {
    fn ast_ptr(&self) -> AstPtr<ast::Function> {
        match self {
            PendingFunctionNode::Declaration(node)
            | PendingFunctionNode::Expression { node, .. }
            | PendingFunctionNode::Arrow { node, .. } => *node,
        }
    }

    fn is_constructor(&self) -> bool {
        match self {
            PendingFunctionNode::Declaration(_) | PendingFunctionNode::Expression { .. } => true,
            PendingFunctionNode::Arrow { .. } => false,
        }
    }

    /// Named given to this unnamed function (an anonymous expression or arrow function)
    fn name(self) -> Option<Wtf8String> {
        match self {
            PendingFunctionNode::Declaration(_) => None,
            PendingFunctionNode::Expression { name, .. }
            | PendingFunctionNode::Arrow { name, .. } => name,
        }
    }
}

/// Collection of functions that still need to be generated, along with their index in the
/// function's constant table.
type PendingFunctions = Vec<(PendingFunctionNode, ConstantTableIndex)>;

/// A function that still needs to be generated, along with the information needed to patch it's
/// creation into the parent function.
struct PatchablePendingFunction {
    /// This pending function's AST node
    func_node: PendingFunctionNode,
    /// The already generated parent function which creates this function
    parent_function: Handle<BytecodeFunction>,
    /// The index into the parent function's constant table that needs to be patched with the
    /// function once it is created.
    constant_index: ConstantTableIndex,
}

pub struct EmitFunctionResult {
    pub bytecode_function: Handle<BytecodeFunction>,
    pending_functions: PendingFunctions,
}

/// The destination register for an expression's value.
enum ExprDest {
    /// Value could be in any register, including locals and arguments.
    Any,
    /// Value must be placed in a newly allocated temporary register.
    NewTemporary,
    /// Value must be placed in a specific register.
    Fixed(GenRegister),
}

pub enum JumpOperand {
    RelativeOffset(GenSInt),
    ConstantIndex(GenConstantIndex),
}

/// Information about where jump statements (break and continue) should jump to.
#[derive(Clone)]
struct JumpStatementTarget {
    /// Block that breaks jump to.
    break_block: BlockId,
    /// Block that continues jump to.
    continue_block: BlockId,
    /// Label id if this is a labeled statement, otherwise this is an unlabeled loop.
    label_id: Option<ast::LabelId>,
}

struct CallReceiver {
    /// Reference to be filled with the receiver for a call.
    receiver: GenRegister,
    /// Whether the receiver is within an optional chain.
    is_chain: bool,
}

/// A reference to a string that may be either wtf8 or utf8.
#[derive(Copy, Clone)]
enum AnyStr<'a> {
    Wtf8(&'a Wtf8String),
    Str(&'a str),
}

impl AnyStr<'_> {
    fn from_id(id: &ast::Identifier) -> AnyStr {
        AnyStr::Str(id.name.as_str())
    }

    fn to_owned(&self) -> Wtf8String {
        match self {
            AnyStr::Wtf8(wtf8) => (*wtf8).clone(),
            AnyStr::Str(str) => Wtf8String::from_str(str),
        }
    }
}

#[derive(PartialEq)]
enum StmtCompletion {
    /// Continue normally to the next statement in at least one path through this function.
    Normal,
    /// Do not continue to the next statement in any paths through this statement, e.g. due to a
    /// return, throw, break, continue, etc.
    Abrupt,
}

impl StmtCompletion {
    fn is_abrupt(&self) -> bool {
        *self == StmtCompletion::Abrupt
    }

    /// When two completions are joined, the result is only abrupt if all paths are abrupt.
    fn combine(&self, other: StmtCompletion) -> StmtCompletion {
        match (self, other) {
            (StmtCompletion::Abrupt, StmtCompletion::Abrupt) => StmtCompletion::Abrupt,
            _ => StmtCompletion::Normal,
        }
    }
}
