use std::{
    fmt::{self, Debug},
    hash,
    ptr::{self, NonNull},
    rc::Rc,
};

use bitflags::bitflags;
use num_bigint::BigInt;

use crate::{
    js::{common::wtf_8::Wtf8String, runtime::completion::EvalResult},
    maybe,
};

use super::{
    loc::{Loc, EMPTY_LOC},
    regexp::RegExp,
    scope_tree::{
        AstScopeNode, Binding, HOME_OBJECT_BINDING_NAME, STATIC_HOME_OBJECT_BINDING_NAME,
    },
    source::Source,
};

pub type P<T> = Box<T>;

pub fn p<T>(node: T) -> P<T> {
    Box::new(node)
}

/// Reference to AST node without lifetime constraints. Only valid to use while AST is still live.
pub struct AstPtr<T> {
    ptr: NonNull<T>,
}

impl<T> AstPtr<T> {
    pub fn uninit() -> AstPtr<T> {
        AstPtr { ptr: NonNull::dangling() }
    }

    pub fn from_ref(value: &T) -> AstPtr<T> {
        let ptr = unsafe { NonNull::new_unchecked(value as *const _ as *mut T) };
        AstPtr { ptr }
    }
}

impl<T> std::convert::AsRef<T> for AstPtr<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T> std::convert::AsMut<T> for AstPtr<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T> Clone for AstPtr<T> {
    fn clone(&self) -> AstPtr<T> {
        *self
    }
}

impl<T> Copy for AstPtr<T> {}

impl<T> PartialEq for AstPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Eq for AstPtr<T> {}

impl<T> hash::Hash for AstPtr<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T> Debug for AstPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AstPtr({:p})", self.ptr)
    }
}

pub struct Program {
    pub loc: Loc,
    pub toplevels: Vec<Toplevel>,
    pub kind: ProgramKind,
    pub source: Rc<Source>,
    // Whether the function is in strict mode, which could be inherited from surrounding context
    // (e.g. in a direct eval or module)
    pub is_strict_mode: bool,
    // Whether the program has a "use strict" directive
    pub has_use_strict_directive: bool,

    pub scope: AstPtr<AstScopeNode>,
}

impl Program {
    pub fn new(
        loc: Loc,
        toplevels: Vec<Toplevel>,
        kind: ProgramKind,
        source: Rc<Source>,
        scope: AstPtr<AstScopeNode>,
        is_strict_mode: bool,
        has_use_strict_directive: bool,
    ) -> Program {
        Program {
            loc,
            toplevels,
            kind,
            source,
            is_strict_mode,
            has_use_strict_directive,
            scope,
        }
    }
}

#[derive(PartialEq)]
pub enum ProgramKind {
    Script,
    Module,
}

pub enum Toplevel {
    Statement(Statement),
    Import(ImportDeclaration),
    ExportDefault(ExportDefaultDeclaration),
    ExportNamed(ExportNamedDeclaration),
    ExportAll(ExportAllDeclaration),
}

pub struct Identifier {
    pub loc: Loc,
    pub name: String,

    /// Reference to the scope that contains the binding for this identifier, or tagged as
    /// unresolved if the scope could not be statically determined.
    ///
    /// For defs this is set during parsing. For uses this is set during analysis.
    pub scope: TaggedResolvedScope,
}

impl Identifier {
    pub fn new(loc: Loc, name: String) -> Identifier {
        Identifier { loc, name, scope: TaggedResolvedScope::unresolved_global() }
    }

    pub fn get_binding(&self) -> &Binding {
        self.scope.unwrap_resolved().get_binding(&self.name)
    }

    pub fn get_private_name_binding(&self) -> &Binding {
        let private_name = format!("#{}", self.name);
        self.scope.unwrap_resolved().get_binding(&private_name)
    }
}

/// Reference to a scope node without lifetime constraints. Only valid to use while scope tree is
/// still live.
///
/// Reference is represented as a tagged pointer, with tags indicating whether the scope was
/// resolved and if it needs to be looked up dynamically or directly from global scope.
#[derive(Clone, Copy)]
pub struct TaggedResolvedScope {
    ptr: *mut u8,
}

pub enum ResolvedScope {
    /// Scope was not resolved, and name must be looked up in global scope.
    UnresolvedGlobal,
    /// Scope was not resolved, and name must be looked up dynamically in scope chain.
    UnresolvedDynamic,
    /// Scope was resolved to a specific scope node.
    Resolved,
}

impl TaggedResolvedScope {
    pub const fn unresolved_global() -> TaggedResolvedScope {
        TaggedResolvedScope { ptr: ptr::null_mut() }
    }

    pub const fn unresolved_dynamic() -> TaggedResolvedScope {
        TaggedResolvedScope { ptr: 1 as *mut u8 }
    }

    pub const fn resolved(scope: AstPtr<AstScopeNode>) -> TaggedResolvedScope {
        TaggedResolvedScope { ptr: scope.ptr.as_ptr() as *mut u8 }
    }

    pub fn kind(&self) -> ResolvedScope {
        if self.ptr.is_null() {
            ResolvedScope::UnresolvedGlobal
        } else if self.ptr as usize == 1 {
            ResolvedScope::UnresolvedDynamic
        } else {
            ResolvedScope::Resolved
        }
    }

    pub fn unwrap_resolved(&self) -> &AstScopeNode {
        if self.is_unresolved() {
            panic!("Expected resolved scope")
        }

        unsafe { &*(self.ptr as *mut AstScopeNode) }
    }

    pub fn unwrap_resolved_mut(&mut self) -> &mut AstScopeNode {
        if self.is_unresolved() {
            panic!("Expected resolved scope")
        }

        unsafe { &mut *(self.ptr as *mut AstScopeNode) }
    }

    pub fn is_resolved(&self) -> bool {
        matches!(self.kind(), ResolvedScope::Resolved)
    }

    pub fn is_unresolved(&self) -> bool {
        matches!(self.kind(), ResolvedScope::UnresolvedGlobal | ResolvedScope::UnresolvedDynamic)
    }
}

pub enum Statement {
    VarDecl(VariableDeclaration),
    FuncDecl(P<Function>),
    ClassDecl(P<Class>),
    Expr(ExpressionStatement),
    Block(Block),
    If(IfStatement),
    Switch(SwitchStatement),
    For(ForStatement),
    ForEach(ForEachStatement),
    While(WhileStatement),
    DoWhile(DoWhileStatement),
    With(WithStatement),
    Try(TryStatement),
    Throw(ThrowStatement),
    Return(ReturnStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Labeled(LabeledStatement),
    Empty(Loc),
    Debugger(Loc),
}

#[derive(PartialEq)]
pub enum VarKind {
    Var,
    Let,
    Const,
}

pub struct VariableDeclaration {
    pub loc: Loc,
    pub kind: VarKind,
    pub declarations: Vec<VariableDeclarator>,
}

pub struct VariableDeclarator {
    pub loc: Loc,
    pub id: P<Pattern>,
    pub init: Option<P<OuterExpression>>,
    /// Whether an assignment expression appears in the pattern.
    pub id_has_assign_expr: bool,
}

impl VariableDeclarator {
    pub fn new(loc: Loc, id: P<Pattern>, init: Option<P<OuterExpression>>) -> VariableDeclarator {
        VariableDeclarator { loc, id, init, id_has_assign_expr: false }
    }

    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        self.id.iter_bound_names(f)
    }
}

/// Functions can be uniquely determined by their starting source position.
pub type FunctionId = usize;

bitflags! {
    #[derive(Clone, Copy)]
    pub struct FunctionFlags: u16 {
        const IS_ASYNC = 1 << 0;
        const IS_GENERATOR = 1 << 1;
        const IS_ARROW = 1 << 2;
        const HAS_SIMPLE_PARAMETER_LIST = 1 << 3;
        const HAS_PARAMETER_EXPRESSIONS = 1 << 4;
        const HAS_DUPLICATE_PARAMETERS = 1 << 5;
        /// False only if we can statically prove that the arguments object is not needed. If true
        /// true the arguments object may be needed.
        const IS_ARGUMENTS_OBJECT_NEEDED = 1 << 6;
        /// False only if we can statically prove that new.target is not needed. If true new.target
        /// must be created as it may be needed.
        const IS_NEW_TARGET_NEEDED = 1 << 7;
        /// Whether the function is in strict mode, which could be inherited from surrounding context
        const IS_STRICT_MODE = 1 << 8;
        /// Whether the function has a "use strict" directive
        const HAS_USE_STRICT_DIRECTIVE = 1 << 9;
    }
}

pub struct Function {
    pub loc: Loc,
    pub id: Option<P<Identifier>>,
    pub params: Vec<FunctionParam>,
    pub body: P<FunctionBody>,
    pub flags: FunctionFlags,

    /// Scope node for the function, containing function parameters and the body.
    pub scope: AstPtr<AstScopeNode>,
}

impl Function {
    pub fn new_uninit() -> Function {
        Function {
            // Default values that will be overwritten by `init`
            loc: EMPTY_LOC,
            id: None,
            params: vec![],
            body: p(FunctionBody::Block(FunctionBlockBody {
                loc: EMPTY_LOC,
                body: vec![],
                scope: None,
            })),
            flags: FunctionFlags::empty(),
            scope: AstPtr::uninit(),
        }
    }

    pub fn init(
        &mut self,
        loc: Loc,
        id: Option<P<Identifier>>,
        params: Vec<FunctionParam>,
        body: P<FunctionBody>,
        flags: FunctionFlags,
        scope: AstPtr<AstScopeNode>,
    ) {
        self.loc = loc;
        self.id = id;
        self.params = params;
        self.body = body;
        self.flags = flags;
        self.scope = scope;
    }

    pub fn new(
        loc: Loc,
        id: Option<P<Identifier>>,
        params: Vec<FunctionParam>,
        body: P<FunctionBody>,
        flags: FunctionFlags,
        scope: AstPtr<AstScopeNode>,
    ) -> Function {
        let mut func = Function::new_uninit();
        func.init(loc, id, params, body, flags, scope);
        func
    }

    pub fn is_async(&self) -> bool {
        self.flags.contains(FunctionFlags::IS_ASYNC)
    }

    pub fn is_generator(&self) -> bool {
        self.flags.contains(FunctionFlags::IS_GENERATOR)
    }

    pub fn is_arrow(&self) -> bool {
        self.flags.contains(FunctionFlags::IS_ARROW)
    }

    pub fn has_simple_parameter_list(&self) -> bool {
        self.flags
            .contains(FunctionFlags::HAS_SIMPLE_PARAMETER_LIST)
    }

    pub fn has_duplicate_parameters(&self) -> bool {
        self.flags.contains(FunctionFlags::HAS_DUPLICATE_PARAMETERS)
    }

    pub fn is_arguments_object_needed(&self) -> bool {
        self.flags
            .contains(FunctionFlags::IS_ARGUMENTS_OBJECT_NEEDED)
    }

    pub fn set_is_arguments_object_needed(&mut self, is_needed: bool) {
        self.flags
            .set(FunctionFlags::IS_ARGUMENTS_OBJECT_NEEDED, is_needed);
    }

    pub fn is_new_target_needed(&self) -> bool {
        self.flags.contains(FunctionFlags::IS_NEW_TARGET_NEEDED)
    }

    pub fn set_is_new_target_needed(&mut self, is_needed: bool) {
        self.flags
            .set(FunctionFlags::IS_NEW_TARGET_NEEDED, is_needed);
    }

    pub fn is_strict_mode(&self) -> bool {
        self.flags.contains(FunctionFlags::IS_STRICT_MODE)
    }

    pub fn has_use_strict_directive(&self) -> bool {
        self.flags.contains(FunctionFlags::HAS_USE_STRICT_DIRECTIVE)
    }

    /// Whether this function needs a mapped arguments object. If false and this function needs
    /// an arguments object, it uses an unmapped arguments object.
    pub fn needs_mapped_arguments_object(&self) -> bool {
        self.is_arguments_object_needed()
            && !self.is_strict_mode()
            && self.has_simple_parameter_list()
    }
}

pub enum FunctionParam {
    Pattern {
        pattern: Pattern,
        /// Whether the pattern has an assignment expression.
        has_assign_expr: bool,
    },
    Rest {
        rest: RestElement,
        /// Whether the rest element has an assignment expression.
        has_assign_expr: bool,
    },
}

impl FunctionParam {
    pub fn new_pattern(pattern: Pattern) -> FunctionParam {
        FunctionParam::Pattern { pattern, has_assign_expr: false }
    }

    pub fn new_rest(rest: RestElement) -> FunctionParam {
        FunctionParam::Rest { rest, has_assign_expr: false }
    }

    pub fn has_assign_expr(&self) -> bool {
        match self {
            FunctionParam::Pattern { has_assign_expr, .. } => *has_assign_expr,
            FunctionParam::Rest { has_assign_expr, .. } => *has_assign_expr,
        }
    }

    pub fn iter_patterns<'a, F: FnMut(&'a Pattern)>(&'a self, f: &mut F) {
        match &self {
            FunctionParam::Pattern { pattern, .. } => pattern.iter_patterns(f),
            FunctionParam::Rest { rest: RestElement { argument, .. }, .. } => {
                argument.iter_patterns(f)
            }
        }
    }
}

pub enum FunctionBody {
    Block(FunctionBlockBody),
    Expression(OuterExpression),
}

impl FunctionBody {
    pub fn unwrap_block(&self) -> &FunctionBlockBody {
        match self {
            FunctionBody::Block(block) => block,
            _ => panic!("Expected block body"),
        }
    }
}

pub struct FunctionBlockBody {
    pub loc: Loc,
    pub body: Vec<Statement>,

    /// Scope node for the function body, not including parameters. Only present if the function has
    /// parameter expressions, otherwise only the function's scope node is needed.
    pub scope: Option<AstPtr<AstScopeNode>>,
}

pub struct Class {
    pub loc: Loc,
    pub id: Option<P<Identifier>>,
    pub super_class: Option<P<OuterExpression>>,
    pub body: Vec<ClassElement>,

    pub constructor: Option<AstPtr<ClassMethod>>,

    /// Scope node for the class body
    pub scope: AstPtr<AstScopeNode>,

    /// Scope node for the instance fields initializer. Only present if the class has instance
    /// fields.
    pub fields_initializer_scope: Option<AstPtr<AstScopeNode>>,

    /// Scope node for the static initializer, including static fields and initializer blocks.
    /// Only present if the class has static fields or static initializer blocks.
    pub static_initializer_scope: Option<AstPtr<AstScopeNode>>,
}

impl Class {
    pub fn new(
        loc: Loc,
        id: Option<P<Identifier>>,
        super_class: Option<P<OuterExpression>>,
        body: Vec<ClassElement>,
        scope: AstPtr<AstScopeNode>,
        fields_initializer_scope: Option<AstPtr<AstScopeNode>>,
        static_initializer_scope: Option<AstPtr<AstScopeNode>>,
    ) -> Class {
        Class {
            loc,
            id,
            super_class,
            body,
            constructor: None,
            scope,
            fields_initializer_scope,
            static_initializer_scope,
        }
    }
}

pub enum ClassElement {
    Method(ClassMethod),
    Property(ClassProperty),
}

pub struct ClassMethod {
    pub loc: Loc,
    pub key: P<OuterExpression>,
    pub value: P<Function>,
    pub kind: ClassMethodKind,
    pub is_computed: bool,
    pub is_static: bool,
    pub is_private: bool,
    /// Whether this is the first part of a private accessor pair, where the second part appears
    /// laster in the class. Set during analysis.
    pub is_private_pair_start: bool,
}

impl ClassMethod {
    pub fn new(
        loc: Loc,
        key: P<OuterExpression>,
        value: P<Function>,
        kind: ClassMethodKind,
        is_computed: bool,
        is_static: bool,
        is_private: bool,
    ) -> ClassMethod {
        ClassMethod {
            loc,
            key,
            value,
            kind,
            is_computed,
            is_static,
            is_private,
            is_private_pair_start: false,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum ClassMethodKind {
    Method,
    Constructor,
    Get,
    Set,
    // Static initializer blocks are represented as methods
    StaticInitializer,
}

pub struct ClassProperty {
    pub loc: Loc,
    pub key: P<OuterExpression>,
    pub value: Option<P<OuterExpression>>,
    pub is_computed: bool,
    pub is_static: bool,
    pub is_private: bool,
}

pub struct ExpressionStatement {
    pub loc: Loc,
    pub expr: P<OuterExpression>,
}

pub struct Block {
    pub loc: Loc,
    pub body: Vec<Statement>,

    /// Block scope node for the block.
    pub scope: AstPtr<AstScopeNode>,
}

pub struct IfStatement {
    pub loc: Loc,
    pub test: P<OuterExpression>,
    pub conseq: P<Statement>,
    pub altern: Option<P<Statement>>,
}

pub struct SwitchStatement {
    pub loc: Loc,
    pub discriminant: P<OuterExpression>,
    pub cases: Vec<SwitchCase>,

    /// Block scope node for the switch statement body.
    pub scope: AstPtr<AstScopeNode>,
}

pub struct SwitchCase {
    pub loc: Loc,
    pub test: Option<P<OuterExpression>>,
    pub body: Vec<Statement>,
}

pub struct ForStatement {
    pub loc: Loc,
    pub init: Option<P<ForInit>>,
    pub test: Option<P<OuterExpression>>,
    pub update: Option<P<OuterExpression>>,
    pub body: P<Statement>,

    /// Block scope node that contains the for statement variable declarations and the body.
    pub scope: AstPtr<AstScopeNode>,
}

pub enum ForInit {
    Expression(OuterExpression),
    VarDecl(VariableDeclaration),
}

pub struct ForEachStatement {
    pub loc: Loc,
    pub kind: ForEachKind,
    pub left: P<ForEachInit>,
    pub right: P<OuterExpression>,
    pub body: P<Statement>,
    pub is_await: bool,

    /// Block scope node that contains the for statement variable declarations and the body.
    pub scope: AstPtr<AstScopeNode>,
}

#[derive(PartialEq)]
pub enum ForEachKind {
    In,
    Of,
}

pub enum ForEachInit {
    VarDecl(VariableDeclaration),
    Pattern {
        pattern: Pattern,
        /// Whether an assignment expression appears in the pattern.
        has_assign_expr: bool,
    },
}

impl ForEachInit {
    pub fn new_pattern(pattern: Pattern) -> ForEachInit {
        ForEachInit::Pattern { pattern, has_assign_expr: false }
    }

    pub fn pattern(&self) -> &Pattern {
        match self {
            ForEachInit::VarDecl(decl) => &decl.declarations[0].id,
            ForEachInit::Pattern { pattern, .. } => pattern,
        }
    }

    pub fn is_decl(&self) -> bool {
        matches!(self, ForEachInit::VarDecl(_))
    }

    pub fn has_assign_expr(&self) -> bool {
        match self {
            ForEachInit::Pattern { has_assign_expr, .. } => *has_assign_expr,
            // Not relevant for var decls
            ForEachInit::VarDecl(_) => false,
        }
    }
}

pub struct WhileStatement {
    pub loc: Loc,
    pub test: P<OuterExpression>,
    pub body: P<Statement>,
}

pub struct DoWhileStatement {
    pub loc: Loc,
    pub test: P<OuterExpression>,
    pub body: P<Statement>,
}

pub struct WithStatement {
    pub loc: Loc,
    pub object: P<OuterExpression>,
    pub body: P<Statement>,

    /// Scope node for the with statement body
    pub scope: AstPtr<AstScopeNode>,
}

pub struct TryStatement {
    pub loc: Loc,
    pub block: P<Block>,
    pub handler: Option<P<CatchClause>>,
    pub finalizer: Option<P<Block>>,
}

pub struct CatchClause {
    pub loc: Loc,
    pub param: Option<P<Pattern>>,
    pub body: P<Block>,
    /// Whether the parameter is a pattern with an assignment expression.
    pub param_has_assign_expr: bool,

    /// Block scope node that contains the catch clause parameter binding and body.
    pub scope: AstPtr<AstScopeNode>,
}

impl CatchClause {
    pub fn new(
        loc: Loc,
        param: Option<P<Pattern>>,
        body: P<Block>,
        scope: AstPtr<AstScopeNode>,
    ) -> CatchClause {
        CatchClause { loc, param, body, param_has_assign_expr: false, scope }
    }
}

pub struct ThrowStatement {
    pub loc: Loc,
    pub argument: P<OuterExpression>,
}

pub struct ReturnStatement {
    pub loc: Loc,
    pub argument: Option<P<OuterExpression>>,

    /// Reference to the scope that contains the binding for `this`. Similar to the scope in the
    /// `ThisExpression` node, but only set if this return is in a derived constructor (meaning the
    /// derived constructor's `this` may be implicitly returned).
    pub this_scope: Option<AstPtr<AstScopeNode>>,
}

impl ReturnStatement {
    pub fn new(loc: Loc, argument: Option<P<OuterExpression>>) -> ReturnStatement {
        ReturnStatement { loc, argument, this_scope: None }
    }
}

pub struct BreakStatement {
    pub loc: Loc,
    pub label: Option<Label>,
}

pub struct ContinueStatement {
    pub loc: Loc,
    pub label: Option<Label>,
}

pub struct LabeledStatement {
    pub loc: Loc,
    pub label: P<Label>,
    pub body: P<Statement>,
}

pub type LabelId = u16;

pub struct Label {
    pub loc: Loc,
    pub name: String,
    pub id: LabelId,
}

impl Label {
    pub fn new(loc: Loc, name: String) -> Label {
        Label { loc, name, id: 0 }
    }
}

/// An entire expression held by a statement. Contains additional metadata about the expression.
///
/// Only expressions may hold Expressions directly. All other nodes must hold an OuterExpression
/// so that metadata can be associated with the entire wrapped expression.
pub struct OuterExpression {
    /// The entire expression that is being wrapped.
    pub expr: Expression,
    /// Whether an assignment expression appears in the expression.
    pub has_assign_expr: bool,
}

pub enum Expression {
    Id(Identifier),
    Null(Loc),
    Boolean(BooleanLiteral),
    Number(NumberLiteral),
    String(StringLiteral),
    BigInt(BigIntLiteral),
    RegExp(RegExpLiteral),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Logical(LogicalExpression),
    Assign(AssignmentExpression),
    Update(UpdateExpression),
    Member(MemberExpression),
    Chain(ChainExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
    New(NewExpression),
    Sequence(SequenceExpression),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Function(P<Function>),
    ArrowFunction(P<Function>),
    Class(P<Class>),
    This(ThisExpression),
    Await(AwaitExpression),
    Yield(YieldExpression),
    SuperMember(SuperMemberExpression),
    SuperCall(P<SuperCallExpression>),
    Template(TemplateLiteral),
    TaggedTemplate(TaggedTemplateExpression),
    MetaProperty(MetaProperty),
    Import(ImportExpression),
}

impl Expression {
    pub fn to_id(&self) -> &Identifier {
        match self {
            Expression::Id(id) => id,
            _ => panic!("Expected identifier expression"),
        }
    }

    pub fn to_id_mut(&mut self) -> &mut Identifier {
        match self {
            Expression::Id(id) => id,
            _ => panic!("Expected identifier expression"),
        }
    }
}

pub struct BooleanLiteral {
    pub loc: Loc,
    pub value: bool,
}

pub struct NumberLiteral {
    pub loc: Loc,
    pub value: f64,
}

pub struct StringLiteral {
    pub loc: Loc,
    pub value: Wtf8String,
}

pub struct BigIntLiteral {
    pub loc: Loc,
    pub value: BigInt,
}

pub struct RegExpLiteral {
    pub loc: Loc,
    pub raw: P<Wtf8String>,
    pub pattern: P<Wtf8String>,
    pub flags: P<Wtf8String>,
    pub regexp: P<RegExp>,
}

#[derive(PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    TypeOf,
    Void,
    Delete,
}

pub struct UnaryExpression {
    pub loc: Loc,
    pub operator: UnaryOperator,
    pub argument: P<Expression>,
}

#[derive(PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Exponent,
    EqEq,
    EqEqEq,
    NotEq,
    NotEqEq,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRightArithmetic,
    ShiftRightLogical,
    // All in expressions with a non-private name LHS
    In,
    // Only for private name LHS. Left expr will always be an identifier.
    InPrivate,
    InstanceOf,
}

pub struct BinaryExpression {
    pub loc: Loc,
    pub operator: BinaryOperator,
    pub left: P<Expression>,
    pub right: P<Expression>,
}

#[derive(PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    NullishCoalesce,
}

pub struct LogicalExpression {
    pub loc: Loc,
    pub operator: LogicalOperator,
    pub left: P<Expression>,
    pub right: P<Expression>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum AssignmentOperator {
    Equals,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Exponent,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRightArithmetic,
    ShiftRightLogical,
    LogicalAnd,
    LogicalOr,
    NullishCoalesce,
}

impl AssignmentOperator {
    pub fn is_logical(&self) -> bool {
        matches!(
            self,
            AssignmentOperator::LogicalAnd
                | AssignmentOperator::LogicalOr
                | AssignmentOperator::NullishCoalesce
        )
    }
}

pub struct AssignmentExpression {
    pub loc: Loc,
    pub operator: AssignmentOperator,
    pub left: P<Pattern>,
    pub right: P<Expression>,
    // Needed for reparsing into a pattern
    pub is_parenthesized: bool,
}

#[derive(Copy, Clone)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}

pub struct UpdateExpression {
    pub loc: Loc,
    pub operator: UpdateOperator,
    pub argument: P<Expression>,
    pub is_prefix: bool,
}

pub struct MemberExpression {
    pub loc: Loc,
    pub object: P<Expression>,
    pub property: P<Expression>,
    pub is_computed: bool,
    pub is_optional: bool,
    pub is_private: bool,
}

pub struct ChainExpression {
    pub loc: Loc,
    pub expression: P<Expression>,
}

pub struct ConditionalExpression {
    pub loc: Loc,
    pub test: P<Expression>,
    pub altern: P<Expression>,
    pub conseq: P<Expression>,
}

pub struct CallExpression {
    pub loc: Loc,
    pub callee: P<Expression>,
    pub arguments: Vec<CallArgument>,
    pub is_optional: bool,

    // The reamining fields are only set if the call expression is potentially a direct eval.
    // These fields are set during analysis.
    /// Whether the function is potentially a direct eval and inside a non-arrow function, meaning a
    /// new.target expression is allowed.
    pub maybe_eval_in_function: bool,
    /// Whether the function is potentially a direct eval and inside a method, meaning a super
    /// member expression is allowed.
    pub maybe_eval_in_method: bool,
    /// Whether the function is potentially a direct eval and inside a static method or class field,
    /// meaning a super member expression refers to the static home object.
    pub maybe_eval_in_static: bool,
    /// Whether the function is potentially a direct eval and inside a derived constructor,
    /// meaning a super constructor call is allowed.
    pub maybe_eval_in_derived_constructor: bool,
    /// Whether the function is potentially a direct eval and inside a class static block
    /// initializer, meaning the await token is not allowed.
    pub maybe_eval_in_static_initializer: bool,
    /// Whether the function is potentially a direct eval and inside a class field initializer,
    /// meaning accessing the arguments binding is not allowed.
    pub maybe_eval_in_class_field_initializer: bool,
}

impl CallExpression {
    pub fn new(
        loc: Loc,
        callee: P<Expression>,
        arguments: Vec<CallArgument>,
        is_optional: bool,
    ) -> CallExpression {
        CallExpression {
            loc,
            callee,
            arguments,
            is_optional,
            maybe_eval_in_function: false,
            maybe_eval_in_method: false,
            maybe_eval_in_static: false,
            maybe_eval_in_derived_constructor: false,
            maybe_eval_in_static_initializer: false,
            maybe_eval_in_class_field_initializer: false,
        }
    }
}

pub enum CallArgument {
    Expression(Expression),
    Spread(SpreadElement),
}

pub struct NewExpression {
    pub loc: Loc,
    pub callee: P<Expression>,
    pub arguments: Vec<CallArgument>,
}

pub struct SequenceExpression {
    pub loc: Loc,
    pub expressions: Vec<Expression>,
}

pub struct ArrayExpression {
    pub loc: Loc,
    pub elements: Vec<ArrayElement>,
    // Needed for reparsing into a pattern
    pub is_parenthesized: bool,
}

pub enum ArrayElement {
    Expression(Expression),
    Spread(SpreadElement),
    Hole,
}

pub struct SpreadElement {
    pub loc: Loc,
    pub argument: P<Expression>,
    pub has_trailing_comma: bool,
}

pub struct ObjectExpression {
    pub loc: Loc,
    pub properties: Vec<Property>,
    // Needed for reparsing into a pattern
    pub is_parenthesized: bool,

    /// Scope node for the object body, will only contain the home object.
    pub scope: AstPtr<AstScopeNode>,
}

pub struct Property {
    pub loc: Loc,
    pub key: P<Expression>,
    pub value: Option<P<Expression>>,
    pub is_computed: bool,
    pub is_method: bool,
    pub kind: PropertyKind,
}

pub enum PropertyKind {
    Init,
    Get,
    Set,
    // For spread properties the key is the argument and all other fields are ignored. The single
    // bool argument is whether the spread property is followed by a comma.
    Spread(bool),
    // A pattern initializer that is not valid in a final AST, but can be reparsed into an object
    // pattern property with an initializer. The single expression argument is the initializer. If
    // a PatternInitializer is found during analysis the analyzer will error.
    PatternInitializer(P<Expression>),
}

pub struct ThisExpression {
    pub loc: Loc,
    /// Reference to the scope that contains the binding for `this`, which may be a parent scope if
    /// `this` is captured by an arrow function.
    ///
    /// Starts out uninitialized during parsing and is set during analysis. Only set if resolved to
    /// a captured `this` binding, or if referring to `this` of a derived constructor.
    pub scope: Option<AstPtr<AstScopeNode>>,
}

pub struct AwaitExpression {
    pub loc: Loc,
    pub argument: P<Expression>,
}

pub struct YieldExpression {
    pub loc: Loc,
    pub argument: Option<P<Expression>>,
    pub is_delegate: bool,
}

pub struct SuperMemberExpression {
    pub loc: Loc,
    pub super_: Loc,
    pub property: P<Expression>,
    pub is_computed: bool,

    /// Whether this super member expression is in a static method. Set during analysis.
    pub is_static: bool,

    /// Reference to the scope that contains the binding for `this`. Treated the same as the scope
    /// in the `ThisExpression` node.
    pub this_scope: Option<AstPtr<AstScopeNode>>,

    /// Reference to the scope that contains the binding for the home object referenced by this
    /// super expression, or tagged as unresolved dynamic if the scope could not be statically
    /// determined.
    pub home_object_scope: TaggedResolvedScope,
}

impl SuperMemberExpression {
    pub fn new(loc: Loc, super_: Loc, property: P<Expression>, is_computed: bool) -> Self {
        Self {
            loc,
            super_,
            property,
            is_computed,
            is_static: false,
            this_scope: None,
            home_object_scope: TaggedResolvedScope::unresolved_global(),
        }
    }

    pub fn home_object_name(&self) -> &'static str {
        if self.is_static {
            STATIC_HOME_OBJECT_BINDING_NAME
        } else {
            HOME_OBJECT_BINDING_NAME
        }
    }
}

pub struct SuperCallExpression {
    pub loc: Loc,
    pub super_: Loc,
    pub arguments: Vec<CallArgument>,

    /// Reference to the function scope that contains the binding for the containing derived
    /// constructor, or tagged as unresolved dynamic if the scope could not be statically determined.
    pub constructor_scope: TaggedResolvedScope,

    /// Reference to the function scope that contains the binding for this new.target, or tagged
    /// as unresolved dynamic if the scope could not be statically determined.
    pub new_target_scope: TaggedResolvedScope,

    /// Reference to the scope that contains the binding for `this`. Similar to the scope in the
    /// `ThisExpression` node, but is always set since it refers to a derived constructor's `this`.
    pub this_scope: AstPtr<AstScopeNode>,
}

impl SuperCallExpression {
    pub fn new(loc: Loc, super_loc: Loc, arguments: Vec<CallArgument>) -> Self {
        Self {
            loc,
            super_: super_loc,
            arguments,
            constructor_scope: TaggedResolvedScope::unresolved_global(),
            new_target_scope: TaggedResolvedScope::unresolved_global(),
            this_scope: AstPtr::uninit(),
        }
    }
}

pub struct TemplateLiteral {
    pub loc: Loc,
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}

pub struct TemplateElement {
    pub loc: Loc,
    pub raw: Wtf8String,
    /// Guaranteed to exist for template literals. Tagged templates allow this to be None.
    pub cooked: Option<Wtf8String>,
}

pub struct TaggedTemplateExpression {
    pub loc: Loc,
    pub tag: P<Expression>,
    pub quasi: P<TemplateLiteral>,
}

pub struct MetaProperty {
    pub loc: Loc,
    pub kind: MetaPropertyKind,
}

impl MetaProperty {
    pub fn new_target(loc: Loc) -> MetaProperty {
        MetaProperty {
            loc,
            kind: MetaPropertyKind::NewTarget { scope: TaggedResolvedScope::unresolved_global() },
        }
    }
}

#[derive(Clone, Copy)]
pub enum MetaPropertyKind {
    NewTarget {
        /// Reference to the function scope that contains the binding for this new.target, or tagged
        /// as unresolved dynamic if the scope could not be statically determined.
        scope: TaggedResolvedScope,
    },
    ImportMeta,
}

pub struct ImportExpression {
    pub loc: Loc,
    pub source: P<Expression>,
}

pub enum Pattern {
    Id(Identifier),
    Array(ArrayPattern),
    Object(ObjectPattern),
    Assign(AssignmentPattern),
    Member(MemberExpression),
    SuperMember(SuperMemberExpression),
}

impl Pattern {
    pub fn to_id(&self) -> &Identifier {
        match self {
            Pattern::Id(id) => id,
            _ => panic!("Expected identifier pattern"),
        }
    }

    pub fn is_id(&self) -> bool {
        matches!(self, Pattern::Id(_))
    }

    pub fn iter_patterns<'a, F: FnMut(&'a Pattern)>(&'a self, f: &mut F) {
        f(self);

        match &self {
            Pattern::Id(_) => {}
            Pattern::Array(patt) => {
                for element in &patt.elements {
                    match element {
                        ArrayPatternElement::Pattern(pattern) => pattern.iter_patterns(f),
                        ArrayPatternElement::Rest(rest) => rest.argument.iter_patterns(f),
                        ArrayPatternElement::Hole => {}
                    }
                }
            }
            Pattern::Object(patt) => {
                for prop in &patt.properties {
                    prop.value.iter_patterns(f)
                }
            }
            Pattern::Assign(patt) => patt.left.iter_patterns(f),
            Pattern::Member(_) | Pattern::SuperMember(_) => {}
        }
    }

    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        match &self {
            Pattern::Id(id) => f(id),
            Pattern::Array(patt) => patt.iter_bound_names(f),
            Pattern::Object(patt) => patt.iter_bound_names(f),
            Pattern::Assign(patt) => patt.iter_bound_names(f),
            Pattern::Member(_) | Pattern::SuperMember(_) => ().into(),
        }
    }
}

pub struct ArrayPattern {
    pub loc: Loc,
    pub elements: Vec<ArrayPatternElement>,
}

pub enum ArrayPatternElement {
    Pattern(Pattern),
    Rest(RestElement),
    Hole,
}

impl ArrayPattern {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        for element in &self.elements {
            match element {
                ArrayPatternElement::Pattern(pattern) => maybe!(pattern.iter_bound_names(f)),
                ArrayPatternElement::Rest(RestElement { argument, .. }) => {
                    maybe!(argument.iter_bound_names(f))
                }
                ArrayPatternElement::Hole => {}
            }
        }

        ().into()
    }
}

pub struct RestElement {
    pub loc: Loc,
    pub argument: P<Pattern>,
}

pub struct ObjectPattern {
    pub loc: Loc,
    pub properties: Vec<ObjectPatternProperty>,
}

impl ObjectPattern {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        for prop in &self.properties {
            maybe!(prop.value.iter_bound_names(f))
        }

        ().into()
    }
}

pub struct ObjectPatternProperty {
    pub loc: Loc,
    pub key: Option<P<Expression>>,
    pub value: P<Pattern>,
    pub is_computed: bool,
    // For rest properties the value is the argument and must be an id. All other fields are ignored.
    pub is_rest: bool,
}

pub struct AssignmentPattern {
    pub loc: Loc,
    pub left: P<Pattern>,
    pub right: P<Expression>,
}

impl AssignmentPattern {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        self.left.iter_bound_names(f)
    }
}

pub struct ImportDeclaration {
    pub loc: Loc,
    pub specifiers: Vec<ImportSpecifier>,
    pub source: P<StringLiteral>,
}

pub enum ImportSpecifier {
    Default(ImportDefaultSpecifier),
    Named(ImportNamedSpecifier),
    Namespace(ImportNamespaceSpecifier),
}

pub struct ImportDefaultSpecifier {
    pub loc: Loc,
    pub local: P<Identifier>,
}

pub struct ImportNamedSpecifier {
    pub loc: Loc,
    pub imported: Option<P<ModuleName>>,
    pub local: P<Identifier>,
}

pub struct ImportNamespaceSpecifier {
    pub loc: Loc,
    pub local: P<Identifier>,
}

pub struct ExportNamedDeclaration {
    pub loc: Loc,
    // Must be variable declaration, function declaration, or class declaration
    pub declaration: Option<P<Statement>>,
    pub specifiers: Vec<ExportSpecifier>,
    pub source: Option<P<StringLiteral>>,
}

impl ExportNamedDeclaration {
    pub fn iter_declaration_ids(&self, f: &mut impl FnMut(&Identifier)) {
        if let Some(declaration) = &self.declaration {
            match declaration.as_ref() {
                Statement::VarDecl(VariableDeclaration { declarations, .. }) => {
                    for decl in declarations {
                        let _ = decl.iter_bound_names(&mut |id| {
                            f(id);
                            ().into()
                        });
                    }
                }
                Statement::FuncDecl(func) => {
                    if let Some(id) = &func.id {
                        f(id);
                    }
                }
                Statement::ClassDecl(class) => {
                    if let Some(id) = &class.id {
                        f(id);
                    }
                }
                _ => {}
            }
        }
    }
}

pub struct ExportSpecifier {
    pub loc: Loc,
    pub local: P<Identifier>,
    pub exported: Option<P<ModuleName>>,
}

pub struct ExportDefaultDeclaration {
    pub loc: Loc,
    pub declaration: ExportDefaultKind,
}

impl ExportDefaultDeclaration {
    pub fn id(&self) -> Option<&Identifier> {
        match &self.declaration {
            ExportDefaultKind::Function(func) => func.id.as_deref(),
            ExportDefaultKind::Class(class) => class.id.as_deref(),
            ExportDefaultKind::Expression(_) => None,
        }
    }
}

pub enum ExportDefaultKind {
    Function(P<Function>),
    Class(P<Class>),
    Expression(P<OuterExpression>),
}

pub struct ExportAllDeclaration {
    pub loc: Loc,
    pub exported: Option<P<ModuleName>>,
    pub source: P<StringLiteral>,
}

pub enum ModuleName {
    Id(Identifier),
    String(StringLiteral),
}
