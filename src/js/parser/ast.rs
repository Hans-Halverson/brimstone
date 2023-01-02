use crate::{js::runtime::completion::EvalResult, maybe};

use super::loc::Loc;

pub type P<T> = Box<T>;

/// Reference to AST node without lifetime constraints. Only valid to use while AST is still live.
pub struct AstPtr<T: ?Sized> {
    ptr: *const T,
}

impl<T: ?Sized> AstPtr<T> {
    pub fn from_ref(value: &T) -> AstPtr<T> {
        AstPtr { ptr: value }
    }

    pub fn as_ref(&self) -> &T {
        unsafe { &*self.ptr }
    }

    pub fn as_mut(&self) -> &mut T {
        unsafe { &mut *self.ptr.cast_mut() }
    }
}

// An element of 8.1.7 VarScopedDeclarations
pub enum VarDecl {
    Func(AstPtr<Function>),
    Var(AstPtr<VariableDeclaration>),
}

// An element of 8.1.5 LexicallyScopedDeclarations
pub enum LexDecl {
    Func(AstPtr<Function>),
    Var(AstPtr<VariableDeclaration>),
}

impl VarDecl {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        match &self {
            VarDecl::Func(func) => f(&func.as_ref().id.as_deref().unwrap()),
            VarDecl::Var(var_decl) => var_decl.as_ref().iter_bound_names(f),
        }
    }
}

impl LexDecl {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        match &self {
            LexDecl::Func(func) => f(&func.as_ref().id.as_deref().unwrap()),
            LexDecl::Var(var_decl) => var_decl.as_ref().iter_bound_names(f),
        }
    }
}

pub trait WithDecls {
    fn var_decls(&self) -> &[VarDecl];

    fn lex_decls(&self) -> &[LexDecl];

    fn add_var_decl(&mut self, var_decl: VarDecl);

    fn add_lex_decl(&mut self, lex_decl: LexDecl);
}

pub struct Program {
    pub loc: Loc,
    pub toplevels: Vec<Toplevel>,

    pub var_decls: Vec<VarDecl>,
    pub lex_decls: Vec<LexDecl>,
}

impl Program {
    pub fn new(loc: Loc, toplevels: Vec<Toplevel>) -> Program {
        Program {
            loc,
            toplevels,
            var_decls: vec![],
            lex_decls: vec![],
        }
    }
}

impl WithDecls for Program {
    fn var_decls(&self) -> &[VarDecl] {
        &self.var_decls
    }

    fn lex_decls(&self) -> &[LexDecl] {
        &self.lex_decls
    }

    fn add_var_decl(&mut self, var_decl: VarDecl) {
        self.var_decls.push(var_decl)
    }

    fn add_lex_decl(&mut self, lex_decl: LexDecl) {
        self.lex_decls.push(lex_decl)
    }
}

pub enum Toplevel {
    Statement(Statement),
}

pub struct Identifier {
    pub loc: Loc,
    pub name: String,
}

pub enum Statement {
    VarDecl(VariableDeclaration),
    FuncDecl(Function),
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

impl VariableDeclaration {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        for decl in &self.declarations {
            maybe!(decl.iter_bound_names(f))
        }

        ().into()
    }
}

pub struct VariableDeclarator {
    pub loc: Loc,
    pub id: P<Pattern>,
    pub init: Option<P<Expression>>,
}

impl VariableDeclarator {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        self.id.iter_bound_names(f)
    }
}

pub struct Function {
    pub loc: Loc,
    pub id: Option<P<Identifier>>,
    pub params: Vec<Pattern>,
    pub body: P<FunctionBody>,
    pub is_async: bool,
    pub is_generator: bool,

    pub var_decls: Vec<VarDecl>,
    pub lex_decls: Vec<LexDecl>,
    pub has_simple_parameter_list: bool,
    pub has_parameter_expressions: bool,
    pub has_duplicate_parameters: bool,
    // False only if we can statically prove that the arguments object is not needed. If true the
    // arguments object may be needed.
    pub is_arguments_object_needed: bool,
}

impl Function {
    pub fn new(
        loc: Loc,
        id: Option<P<Identifier>>,
        params: Vec<Pattern>,
        body: P<FunctionBody>,
        is_async: bool,
        is_generator: bool,
    ) -> Function {
        Function {
            loc,
            id,
            params,
            body,
            is_async,
            is_generator,
            var_decls: vec![],
            lex_decls: vec![],
            has_simple_parameter_list: false,
            has_parameter_expressions: false,
            has_duplicate_parameters: false,
            is_arguments_object_needed: true,
        }
    }
}

impl WithDecls for Function {
    fn var_decls(&self) -> &[VarDecl] {
        &self.var_decls
    }

    fn lex_decls(&self) -> &[LexDecl] {
        &self.lex_decls
    }

    fn add_var_decl(&mut self, var_decl: VarDecl) {
        self.var_decls.push(var_decl)
    }

    fn add_lex_decl(&mut self, lex_decl: LexDecl) {
        self.lex_decls.push(lex_decl)
    }
}

pub enum FunctionBody {
    Block(Block),
    Expression(Expression),
}

pub struct ExpressionStatement {
    pub loc: Loc,
    pub expr: P<Expression>,
}

pub struct Block {
    pub loc: Loc,
    pub body: Vec<Statement>,

    pub lex_decls: Vec<LexDecl>,
}

impl Block {
    pub fn new(loc: Loc, body: Vec<Statement>) -> Block {
        Block {
            loc,
            body,
            lex_decls: vec![],
        }
    }
}

impl WithDecls for Block {
    fn var_decls(&self) -> &[VarDecl] {
        panic!("Blocks do not have var decls")
    }

    fn lex_decls(&self) -> &[LexDecl] {
        &self.lex_decls
    }

    fn add_var_decl(&mut self, _: VarDecl) {
        panic!("Blocks do not have var decls")
    }

    fn add_lex_decl(&mut self, lex_decl: LexDecl) {
        self.lex_decls.push(lex_decl)
    }
}

pub struct IfStatement {
    pub loc: Loc,
    pub test: P<Expression>,
    pub conseq: P<Statement>,
    pub altern: Option<P<Statement>>,
}

pub struct SwitchStatement {
    pub loc: Loc,
    pub discriminant: P<Expression>,
    pub cases: Vec<SwitchCase>,

    pub lex_decls: Vec<LexDecl>,
}

impl SwitchStatement {
    pub fn new(loc: Loc, discriminant: P<Expression>, cases: Vec<SwitchCase>) -> SwitchStatement {
        SwitchStatement {
            loc,
            discriminant,
            cases,
            lex_decls: vec![],
        }
    }
}

impl WithDecls for SwitchStatement {
    fn var_decls(&self) -> &[VarDecl] {
        panic!("SwitchStatements do not have var decls")
    }

    fn lex_decls(&self) -> &[LexDecl] {
        &self.lex_decls
    }

    fn add_var_decl(&mut self, _: VarDecl) {
        panic!("SwitchStatements do not have var decls")
    }

    fn add_lex_decl(&mut self, lex_decl: LexDecl) {
        self.lex_decls.push(lex_decl)
    }
}

pub struct SwitchCase {
    pub loc: Loc,
    pub test: Option<P<Expression>>,
    pub body: Vec<Statement>,
}

pub struct ForStatement {
    pub loc: Loc,
    pub init: Option<P<ForInit>>,
    pub test: Option<P<Expression>>,
    pub update: Option<P<Expression>>,
    pub body: P<Statement>,
}

pub enum ForInit {
    Expression(Expression),
    VarDecl(VariableDeclaration),
}

pub struct ForEachStatement {
    pub loc: Loc,
    pub kind: ForEachKind,
    pub left: P<ForEachInit>,
    pub right: P<Expression>,
    pub body: P<Statement>,
    pub is_await: bool,
}

pub enum ForEachKind {
    In,
    Of,
}

pub enum ForEachInit {
    VarDecl(VariableDeclaration),
    Pattern(Pattern),
}

pub struct WhileStatement {
    pub loc: Loc,
    pub test: P<Expression>,
    pub body: P<Statement>,
}

pub struct DoWhileStatement {
    pub loc: Loc,
    pub test: P<Expression>,
    pub body: P<Statement>,
}

pub struct WithStatement {
    pub loc: Loc,
    pub object: P<Expression>,
    pub body: P<Statement>,
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
}

pub struct ThrowStatement {
    pub loc: Loc,
    pub argument: P<Expression>,
}

pub struct ReturnStatement {
    pub loc: Loc,
    pub argument: Option<P<Expression>>,
}

pub struct BreakStatement {
    pub loc: Loc,
    pub label: Option<P<Identifier>>,
}

pub struct ContinueStatement {
    pub loc: Loc,
    pub label: Option<P<Identifier>>,
}

pub struct LabeledStatement {
    pub loc: Loc,
    pub label: P<Identifier>,
    pub body: P<Statement>,
}

pub enum Expression {
    Id(Identifier),
    Null(Loc),
    Boolean(BooleanLiteral),
    Number(NumberLiteral),
    String(StringLiteral),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Logical(LogicalExpression),
    Assign(AssignmentExpression),
    Update(UpdateExpression),
    Member(MemberExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
    New(NewExpression),
    Sequence(SequenceExpression),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Function(Function),
    ArrowFunction(Function),
    This(Loc),
    Await(AwaitExpression),
    Yield(YieldExpression),
    // TODO: TemplateLiteral
    // TODO: TaggedTemplateExpression
    // TODO: ClassExpression
    // TODO: MetaProperty
    // TODO: ImportExpression
    // TODO: ChainExpression
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
    pub value: String,
}

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
    In,
    InstanceOf,
}

pub struct BinaryExpression {
    pub loc: Loc,
    pub operator: BinaryOperator,
    pub left: P<Expression>,
    pub right: P<Expression>,
}

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
}

pub struct AssignmentExpression {
    pub loc: Loc,
    pub operator: AssignmentOperator,
    pub left: P<Expression>,
    pub right: P<Expression>,
}

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
    pub arguments: Vec<Expression>,
    pub is_optional: bool,
}

pub struct NewExpression {
    pub loc: Loc,
    pub callee: P<Expression>,
    pub arguments: Vec<Expression>,
}

pub struct SequenceExpression {
    pub loc: Loc,
    pub expressions: Vec<Expression>,
}

pub struct ArrayExpression {
    pub loc: Loc,
    pub elements: Vec<Option<Expression>>,
}

pub struct ObjectExpression {
    pub loc: Loc,
    pub properties: Vec<Property>,
}

pub struct Property {
    pub loc: Loc,
    pub key: P<Expression>,
    pub value: Option<P<Expression>>,
    pub is_computed: bool,
    pub is_method: bool,
    pub kind: PropertyKind,
}

#[derive(Clone, Copy)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

pub struct AwaitExpression {
    pub loc: Loc,
    pub argument: P<Expression>,
}

pub struct YieldExpression {
    pub loc: Loc,
    pub argument: Option<P<Expression>>,
    pub delegate: bool,
}

pub enum Pattern {
    Id(Identifier),
    Array(ArrayPattern),
    Object(ObjectPattern),
    Assign(AssignmentPattern),
}

impl Pattern {
    pub fn iter_patterns<'a, F: FnMut(&'a Pattern)>(&'a self, f: &mut F) {
        f(self);

        match &self {
            Pattern::Id(_) => {}
            Pattern::Array(patt) => {
                for element in &patt.elements {
                    if let Some(element) = element {
                        element.iter_patterns(f)
                    }
                }
            }
            Pattern::Object(patt) => {
                for prop in &patt.properties {
                    prop.value.iter_patterns(f)
                }
            }
            Pattern::Assign(patt) => patt.left.iter_patterns(f),
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
        }
    }
}

pub struct ArrayPattern {
    pub loc: Loc,
    pub elements: Vec<Option<Pattern>>,
}

impl ArrayPattern {
    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        for element in &self.elements {
            if let Some(element) = element {
                maybe!(element.iter_bound_names(f))
            }
        }

        ().into()
    }
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
