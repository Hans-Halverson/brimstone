use std::hash;

use num_bigint::BigInt;

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

impl<T: ?Sized> Clone for AstPtr<T> {
    fn clone(&self) -> AstPtr<T> {
        AstPtr { ptr: self.ptr }
    }
}

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

// An element of 8.1.7 VarScopedDeclarations
pub enum VarDecl {
    Func(AstPtr<Function>),
    Var(AstPtr<VariableDeclaration>),
}

// An element of 8.1.5 LexicallyScopedDeclarations
pub enum LexDecl {
    Func(AstPtr<Function>),
    Var(AstPtr<VariableDeclaration>),
    Class(AstPtr<Class>),
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
            LexDecl::Class(class) => class.as_ref().id.as_deref().map(f).unwrap_or(().into()),
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
    pub kind: ProgramKind,
    // Whether the function is in strict mode, which could be inherited from surrounding context
    // (e.g. in a direct eval or module)
    pub is_strict_mode: bool,
    // Whether the program has a "use strict" directive
    pub has_use_strict_directive: bool,

    pub var_decls: Vec<VarDecl>,
    pub lex_decls: Vec<LexDecl>,
}

impl Program {
    pub fn new(
        loc: Loc,
        toplevels: Vec<Toplevel>,
        kind: ProgramKind,
        is_strict_mode: bool,
        has_use_strict_directive: bool,
    ) -> Program {
        Program {
            loc,
            toplevels,
            kind,
            is_strict_mode,
            has_use_strict_directive,
            var_decls: vec![],
            lex_decls: vec![],
        }
    }
}

#[derive(PartialEq)]
pub enum ProgramKind {
    Script,
    Module,
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
    Import(ImportDeclaration),
    ExportDefault(ExportDefaultDeclaration),
    ExportNamed(ExportNamedDeclaration),
    ExportAll(ExportAllDeclaration),
}

pub struct Identifier {
    pub loc: Loc,
    pub name: String,
}

pub enum Statement {
    VarDecl(VariableDeclaration),
    FuncDecl(Function),
    ClassDecl(Class),
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
    pub params: Vec<FunctionParam>,
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

    // Whether the function has a "use strict" directive
    pub has_use_strict_directive: bool,
    // Whether the function is in strict mode, which could be inherited from surrounding context
    pub is_strict_mode: bool,
}

impl Function {
    pub fn new(
        loc: Loc,
        id: Option<P<Identifier>>,
        params: Vec<FunctionParam>,
        body: P<FunctionBody>,
        is_async: bool,
        is_generator: bool,
        is_strict_mode: bool,
        has_use_strict_directive: bool,
    ) -> Function {
        Function {
            loc,
            id,
            params,
            body,
            is_async,
            is_generator,
            is_strict_mode,
            var_decls: vec![],
            lex_decls: vec![],
            has_simple_parameter_list: false,
            has_parameter_expressions: false,
            has_duplicate_parameters: false,
            is_arguments_object_needed: false,
            has_use_strict_directive,
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

pub enum FunctionParam {
    Pattern(Pattern),
    Rest(RestElement),
}

impl FunctionParam {
    pub fn iter_patterns<'a, F: FnMut(&'a Pattern)>(&'a self, f: &mut F) {
        match &self {
            FunctionParam::Pattern(pattern) => pattern.iter_patterns(f),
            FunctionParam::Rest(RestElement { argument, .. }) => argument.iter_patterns(f),
        }
    }

    pub fn iter_bound_names<'a, F: FnMut(&'a Identifier) -> EvalResult<()>>(
        &'a self,
        f: &mut F,
    ) -> EvalResult<()> {
        match &self {
            FunctionParam::Pattern(pattern) => pattern.iter_bound_names(f),
            FunctionParam::Rest(RestElement { argument, .. }) => argument.iter_bound_names(f),
        }
    }
}

pub enum FunctionBody {
    Block(Block),
    Expression(Expression),
}

pub struct Class {
    pub loc: Loc,
    pub id: Option<P<Identifier>>,
    pub super_class: Option<P<Expression>>,
    pub body: Vec<ClassElement>,

    pub constructor: Option<AstPtr<ClassMethod>>,
}

impl Class {
    pub fn new(
        loc: Loc,
        id: Option<P<Identifier>>,
        super_class: Option<P<Expression>>,
        body: Vec<ClassElement>,
    ) -> Class {
        Class { loc, id, super_class, body, constructor: None }
    }
}

pub enum ClassElement {
    Method(ClassMethod),
    Property(ClassProperty),
}

pub struct ClassMethod {
    pub loc: Loc,
    pub key: P<Expression>,
    pub value: P<Function>,
    pub kind: ClassMethodKind,
    pub is_computed: bool,
    pub is_static: bool,
    pub is_private: bool,
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
    pub key: P<Expression>,
    pub value: Option<P<Expression>>,
    pub is_computed: bool,
    pub is_static: bool,
    pub is_private: bool,
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
        Block { loc, body, lex_decls: vec![] }
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
        SwitchStatement { loc, discriminant, cases, lex_decls: vec![] }
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

#[derive(PartialEq)]
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
    pub label: Option<Label>,
}

pub struct ContinueStatement {
    pub loc: Loc,
    pub label: Option<Label>,
}

pub struct LabeledStatement {
    pub loc: Loc,
    pub label: Label,
    pub body: P<Statement>,
}

pub type LabelId = u16;

pub struct Label {
    pub label: P<Identifier>,
    pub id: LabelId,
}

impl Label {
    pub fn new(label: P<Identifier>) -> Label {
        Label { label, id: 0 }
    }
}

pub enum Expression {
    Id(Identifier),
    Null(Loc),
    Boolean(BooleanLiteral),
    Number(NumberLiteral),
    String(StringLiteral),
    BigInt(BigIntLiteral),
    Regexp(RegexpLiteral),
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
    Class(Class),
    This(Loc),
    Await(AwaitExpression),
    Yield(YieldExpression),
    SuperMember(SuperMemberExpression),
    SuperCall(SuperCallExpression),
    Template(TemplateLiteral),
    TaggedTemplate(TaggedTemplateExpression),
    MetaProperty(MetaProperty),
    Import(ImportExpression),
    // TODO: ChainExpression
}

impl Expression {
    pub fn to_id(&self) -> &Identifier {
        match self {
            Expression::Id(id) => id,
            _ => panic!("Expected identifier expression"),
        }
    }

    pub fn into_id(self) -> Identifier {
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
    pub value: String,
}

pub struct BigIntLiteral {
    pub loc: Loc,
    pub value: BigInt,
}

pub struct RegexpLiteral {
    pub loc: Loc,
    pub raw: String,
    pub pattern: String,
    pub flags: String,
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

#[derive(PartialEq)]
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

pub struct AssignmentExpression {
    pub loc: Loc,
    pub operator: AssignmentOperator,
    pub left: P<Pattern>,
    pub right: P<Expression>,
    // Needed for reparsing into a pattern
    pub is_parenthesized: bool,
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
    pub is_private: bool,
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
}

pub struct SuperCallExpression {
    pub loc: Loc,
    pub super_: Loc,
    pub arguments: Vec<CallArgument>,
}

pub struct TemplateLiteral {
    pub loc: Loc,
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}

pub struct TemplateElement {
    pub loc: Loc,
    pub raw: String,
    pub cooked: String,
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

pub enum MetaPropertyKind {
    NewTarget,
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
    // An expression that evaluates to a reference. Can only be a MemberExpression or
    // SuperMemberExpression.
    Reference(Expression),
}

impl Pattern {
    pub fn to_id(&self) -> &Identifier {
        match self {
            Pattern::Id(id) => id,
            _ => panic!("Expected identifier pattern"),
        }
    }

    pub fn is_id(&self) -> bool {
        match self {
            Pattern::Id(_) => true,
            _ => false,
        }
    }

    pub fn to_assign(&self) -> &AssignmentPattern {
        match self {
            Pattern::Assign(assign) => assign,
            _ => panic!("Expected assignment pattern"),
        }
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
            Pattern::Reference(_) => {}
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
            Pattern::Reference(_) => ().into(),
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

pub struct ExportSpecifier {
    pub loc: Loc,
    pub local: P<ModuleName>,
    pub exported: Option<P<ModuleName>>,
}

pub struct ExportDefaultDeclaration {
    pub loc: Loc,
    // Must be function declaration, class declaration, or expression statement
    pub declaration: P<Statement>,
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
