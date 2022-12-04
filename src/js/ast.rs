use super::loc::Loc;

pub type P<T> = Box<T>;

pub struct Program {
    pub loc: Loc,
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
    pub init: Option<P<Expression>>,
}

pub struct Function {
    pub loc: Loc,
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
}
