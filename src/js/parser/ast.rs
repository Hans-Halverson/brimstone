use crate::{js::runtime::completion::AbstractResult, maybe_};

use super::loc::Loc;

pub type P<T> = Box<T>;

pub type AstId = usize;

/// Reference to AST node without lifetime constraints. Only valid to use while AST is still live.
pub struct AstPtr<T> {
    ptr: *const T,
}

impl<T> AstPtr<T> {
    pub fn from_ref(value: &T) -> AstPtr<T> {
        AstPtr { ptr: value }
    }

    pub fn as_ref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

pub struct Program {
    pub loc: Loc,
    pub ast_id: AstId,
    pub toplevels: Vec<Toplevel>,
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
    pub fn iter_bound_names<F: FnMut(&Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        for decl in &self.declarations {
            maybe_!(decl.iter_bound_names(f))
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
    pub fn iter_bound_names<F: FnMut(&Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        self.id.iter_bound_names(f)
    }
}

pub struct Function {
    pub loc: Loc,
    pub ast_id: AstId,
    pub id: Option<P<Identifier>>,
    pub params: Vec<Pattern>,
    pub body: P<FunctionBody>,
    pub is_async: bool,
    pub is_generator: bool,
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
    pub ast_id: AstId,
    pub body: Vec<Statement>,
}

pub struct IfStatement {
    pub loc: Loc,
    pub test: P<Expression>,
    pub conseq: P<Statement>,
    pub altern: Option<P<Statement>>,
}

pub struct SwitchStatement {
    pub loc: Loc,
    pub ast_id: AstId,
    pub discriminant: P<Expression>,
    pub cases: Vec<SwitchCase>,
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
    pub fn iter_bound_names<F: FnMut(&Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
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
    pub fn iter_bound_names<F: FnMut(&Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        for element in &self.elements {
            if let Some(element) = element {
                maybe_!(element.iter_bound_names(f))
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
    pub fn iter_bound_names<F: FnMut(&Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        for prop in &self.properties {
            if !prop.is_computed {
                maybe_!(prop.value.iter_bound_names(f))
            }
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
    pub fn iter_bound_names<F: FnMut(&Identifier) -> AbstractResult<()>>(
        &self,
        f: &mut F,
    ) -> AbstractResult<()> {
        self.left.iter_bound_names(f)
    }
}
