use std::{collections::HashSet, fs, ops::Range};

use crate::{
    common::graphviz::{DotGraphBuilder, DotTextAlign, DOTFILE_EXTENSION},
    runtime::{
        bytecode::{
            function::BytecodeFunction,
            instruction::{get_jump_offset, Instruction, InstructionIterator},
        },
        debug_print::{DebugPrint, DebugPrintMode},
        Context, HeapPtr,
    },
};

/// If the option is set, generate a Graphviz DOT file for a function's bytecode and write to disk.
pub fn save_bytecode_dotfile_if_needed(context: Context, func: HeapPtr<BytecodeFunction>) {
    let output_directory = match context.options.bytecode_dotfile_directory.clone() {
        Some(dir) => dir,
        None => return,
    };

    let name = function_name(func);

    let output_path = context.debug_file_name_reserver.reserve_unique_path(
        &output_directory,
        &name,
        DOTFILE_EXTENSION,
    );

    let dot_graph = func.to_dot_graph();
    let dot_file = dot_graph.to_string();

    if let Err(error) =
        fs::create_dir_all(&output_directory).and_then(|()| fs::write(&output_path, dot_file))
    {
        eprintln!("Failed to write bytecode dotfile {}: {error}", output_path.display());
    }
}

/// Render a Graphviz DOT representation of a function's bytecode.
pub fn bytecode_function_to_dot_graph(func: HeapPtr<BytecodeFunction>) -> DotGraphBuilder {
    let mut graph = DotGraphBuilder::new("bytecode");

    add_graph_title(&mut graph, func);
    add_default_attributes(&mut graph);
    add_start_node(&mut graph, func);
    add_exception_handler_nodes(&mut graph, func);
    add_nodes_and_edges(&mut graph, func);

    graph
}

/// Each basic block's starting offset can be used as its unique id.
fn block_to_node_id(block_start_offset: usize) -> String {
    block_start_offset.to_string()
}

fn function_name(func: HeapPtr<BytecodeFunction>) -> String {
    match func.name() {
        Some(name) => name.format().unwrap_or_default(),
        None => "<anonymous>".to_owned(),
    }
}

fn add_graph_title(graph: &mut DotGraphBuilder, func: HeapPtr<BytecodeFunction>) {
    let constant_table_string = func
        .constant_table_ptr()
        .map(|table| table.debug_print(DebugPrintMode::Verbose))
        .unwrap_or_default();

    let graph_title = format!(
        "BytecodeFunction: {}\nParameters: {}, Registers: {}\n{}\n",
        function_name(func),
        func.num_parameters(),
        func.num_registers(),
        constant_table_string,
    );

    graph
        .set_graph_attribute("label", &graph_title)
        .set_graph_attribute("fontname", "monospace")
        .set_graph_label_text_align(DotTextAlign::Left);
}

/// Start node points to the first block. Note that offset may not be zero due to width prefixes.
fn add_start_node(graph: &mut DotGraphBuilder, func: HeapPtr<BytecodeFunction>) {
    let (_, start_offset) = InstructionIterator::new(func.bytecode()).next().unwrap();

    graph.add_node("start").attribute("label", "start");
    graph.add_edge("start", &block_to_node_id(start_offset));
}

/// Each exception handler has an entrypoint node pointing to its handler.
fn add_exception_handler_nodes(graph: &mut DotGraphBuilder, func: HeapPtr<BytecodeFunction>) {
    let Some(exception_handlers) = func.exception_handlers_ptr() else {
        return;
    };

    for (i, handler) in exception_handlers.iter().enumerate() {
        let handler_node_id = &format!("exception_handler_{i}");
        let handler_node_label =
            &format!("exception handler ({}-{})", handler.start(), handler.end());

        graph
            .add_node(handler_node_id)
            .attribute("label", handler_node_label);
        graph.add_edge(handler_node_id, &block_to_node_id(handler.handler()));
    }
}

fn add_default_attributes(graph: &mut DotGraphBuilder) {
    graph.set_graph_attribute("labelloc", "t");
    graph.set_graph_attribute("labeljust", "l");
    graph.set_node_default_attribute("shape", "box");
    graph.set_node_default_attribute("fontname", "monospace");
    graph.set_edge_default_attribute("fontname", "monospace");
}

/// The absolute offset that a jump instruction at `offset` transfers control to.
fn get_absolute_jump_target_offset(
    func: HeapPtr<BytecodeFunction>,
    instr: &dyn Instruction,
    offset: usize,
) -> Option<usize> {
    let jump_offset = get_jump_offset(func, instr)?;
    Some(offset.strict_add_signed(jump_offset))
}

/// Determine boundaries of all basic blocks in the bytecode.
///
/// Must reconstruct these boundaries by detecting all entrypoints and jump instruction targets.
/// Return a vec containing the offsets of the [start, end) of every basic block in the bytecode.
fn build_basic_block_bounds<'a>(
    func: HeapPtr<BytecodeFunction>,
    bytecode: &'a [u8],
    instructions: &[(&'a dyn Instruction, usize)],
) -> Vec<usize> {
    // Determine the offsets that begin a basic block
    let mut block_bounds = HashSet::new();

    // Note that start of the first block may not be zero due to width prefixes. Last block will
    // end at the end of the bytecode stream.
    block_bounds.insert(instructions[0].1);
    block_bounds.insert(bytecode.len());

    // All exception handler entrypoints begin a basic block
    if let Some(exception_handlers) = func.exception_handlers_ptr() {
        for handler in exception_handlers.iter() {
            block_bounds.insert(handler.handler());
        }
    }

    for (i, &(instr, offset)) in instructions.iter().enumerate() {
        if let Some(jump_target_offset) = get_absolute_jump_target_offset(func, instr, offset) {
            // Jump target is the start of a basic block
            block_bounds.insert(jump_target_offset);

            // The instruction following a terminator (if any) begins a new basic block
            if let Some((_, next_offset)) = instructions.get(i + 1) {
                block_bounds.insert(*next_offset);
            }
        }
    }

    // Sort deduped basic block boundaries to construct the final list
    let mut sorted_block_bounds = block_bounds.into_iter().collect::<Vec<_>>();
    sorted_block_bounds.sort();

    sorted_block_bounds
}

/// Iterate over all basic blocks in a function's bytecode.
///
/// Call `f` for each basic block, passing the block's offset range and the instructions in the
/// block paired with their absolute offsets.
fn iter_basic_blocks<'a>(
    func: HeapPtr<BytecodeFunction>,
    bytecode: &'a [u8],
    mut f: impl FnMut(Range<usize>, &[(&'a dyn Instruction, usize)]),
) {
    let instructions = InstructionIterator::new(bytecode).collect::<Vec<_>>();
    let block_bounds = build_basic_block_bounds(func, bytecode, &instructions);

    // Walk the instructions once, slicing out the instructions that fall within each block
    let mut index = 0;
    for bounds in block_bounds.windows(2) {
        let bounds = bounds[0]..bounds[1];

        let block_start_index = index;
        while index < instructions.len() && instructions[index].1 < bounds.end {
            index += 1;
        }

        let block = &instructions[block_start_index..index];
        if !block.is_empty() {
            f(bounds, block);
        }
    }
}

fn add_nodes_and_edges(graph: &mut DotGraphBuilder, func: HeapPtr<BytecodeFunction>) {
    let bytecode = func.bytecode();

    iter_basic_blocks(func, bytecode, |block_bounds, instructions| {
        let node_id = block_to_node_id(block_bounds.start);
        graph.add_node(&node_id);

        // Accumulate text for all instructions in the block for the node label
        let mut node_label = String::new();
        for &(instruction, offset) in instructions {
            node_label.push_str(&format!("{offset}: {instruction}\n"));
        }

        // The last instruction determines all control flow edges out of the block
        let &(terminator, terminator_offset) = instructions.last().unwrap();

        // Fallthrough block only exists if this is not the last block in the bytecode
        let fallthrough_node_id = if block_bounds.end != func.bytecode().len() {
            Some(block_to_node_id(block_bounds.end))
        } else {
            None
        };

        add_node_edges(graph, func, terminator, terminator_offset, &node_id, fallthrough_node_id);

        graph
            .get_node(&node_id)
            .attribute("label", &node_label)
            .text_align(DotTextAlign::Left);
    });
}

fn add_node_edges(
    graph: &mut DotGraphBuilder,
    func: HeapPtr<BytecodeFunction>,
    terminator_instr: &dyn Instruction,
    terminator_offset: usize,
    node_id: &str,
    fallthrough_node_id: Option<String>,
) {
    let opcode = terminator_instr.opcode();

    if opcode.is_unconditional_jump() {
        // Unconditional jump only has edge to target block
        let target_offset =
            get_absolute_jump_target_offset(func, terminator_instr, terminator_offset).unwrap();
        let target_node_id = block_to_node_id(target_offset);

        graph.add_edge(node_id, &target_node_id);
    } else if opcode.is_conditional_jump() {
        // A conditional jump either jumps to its target block or falls through to the next block
        let target_offset =
            get_absolute_jump_target_offset(func, terminator_instr, terminator_offset).unwrap();
        let target_node_id = block_to_node_id(target_offset);

        graph
            .add_edge(node_id, &target_node_id)
            .attribute("label", "T");
        graph
            .add_edge(node_id, &fallthrough_node_id.unwrap())
            .attribute("label", "F");
    } else {
        // No control flow instruction means the block was split at a jump target, so fall through
        // to the next block (unless this is the lsat block in the bytecode).

        if let Some(fallthrough_node_id) = fallthrough_node_id {
            graph.add_edge(node_id, &fallthrough_node_id);
        }
    }
}
