use std::{
    fmt::{self, Debug},
    hash,
    ptr::NonNull,
    rc::Rc,
};

use num_bigint::BigInt;

use crate::{
    js::{common::wtf_8::Wtf8String, runtime::completion::EvalResult},
    maybe,
};

use super::{
    loc::{Loc, EMPTY_LOC},
    regexp::RegExp,
    scope_tree::{AstScopeNode, Binding},
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

    pub fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }

    pub fn as_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T> Clone for AstPtr<T> {
    fn clone(&self) -> AstPtr<T> {
        AstPtr { ptr: self.ptr }
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
    /// Reference to the scope that contains the binding for this identifier. None if this
    /// identifier could not be statically resolved.
    ///
    /// For defs this is set during parsing. For uses this is set during analysis.
    pub scope: Option<AstPtr<AstScopeNode>>,
}

impl Identifier {
    pub fn new(loc: Loc, name: String) -> Identifier {
        Identifier { loc, name, scope: None }
    }

    pub fn get_binding(&self) -> &Binding {
        self.scope
            .as_ref()
            .unwrap()
            .as_ref()
            .get_binding(&self.name)
    }
}

pub enum Statement {
    VarDecl(VariableDeclaration),
    FuncDecl(P<Function>),
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

/// Functions can be uniquely determined by their starting source position.
pub type FunctionId = usize;

pub struct Function {
    pub loc: Loc,
    pub id: Option<P<Identifier>>,
    pub params: Vec<FunctionParam>,
    pub body: P<FunctionBody>,
    pub is_async: bool,
    pub is_generator: bool,

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
            body: p(FunctionBody::Block(FunctionBlockBody { loc: EMPTY_LOC, body: vec![] })),
            is_async: false,
            is_generator: false,
            is_strict_mode: false,
            has_use_strict_directive: false,
            scope: AstPtr::uninit(),
            // Initial values that will not be overwritten by `init`
            has_simple_parameter_list: false,
            has_parameter_expressions: false,
            has_duplicate_parameters: false,
            is_arguments_object_needed: false,
        }
    }

    pub fn init(
        &mut self,
        loc: Loc,
        id: Option<P<Identifier>>,
        params: Vec<FunctionParam>,
        body: P<FunctionBody>,
        is_async: bool,
        is_generator: bool,
        is_strict_mode: bool,
        has_use_strict_directive: bool,
        scope: AstPtr<AstScopeNode>,
    ) {
        self.loc = loc;
        self.id = id;
        self.params = params;
        self.body = body;
        self.is_async = is_async;
        self.is_generator = is_generator;
        self.is_strict_mode = is_strict_mode;
        self.has_use_strict_directive = has_use_strict_directive;
        self.scope = scope;
    }

    pub fn new(
        loc: Loc,
        id: Option<P<Identifier>>,
        params: Vec<FunctionParam>,
        body: P<FunctionBody>,
        is_async: bool,
        is_generator: bool,
        is_strict_mode: bool,
        has_use_strict_directive: bool,
        scope: AstPtr<AstScopeNode>,
    ) -> Function {
        let mut func = Function::new_uninit();
        func.init(
            loc,
            id,
            params,
            body,
            is_async,
            is_generator,
            is_strict_mode,
            has_use_strict_directive,
            scope,
        );
        func
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
    Block(FunctionBlockBody),
    Expression(Expression),
}

pub struct FunctionBlockBody {
    pub loc: Loc,
    pub body: Vec<Statement>,
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

    /// Block scope node for the block.
    pub scope: AstPtr<AstScopeNode>,
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

    /// Block scope node for the switch statement body.
    pub scope: AstPtr<AstScopeNode>,
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

    /// Block scope node that contains the for statement variable declarations and the body.
    pub scope: AstPtr<AstScopeNode>,
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
    Pattern(Pattern),
}

impl ForEachInit {
    pub fn pattern(&self) -> &Pattern {
        match self {
            ForEachInit::VarDecl(decl) => &decl.declarations[0].id,
            ForEachInit::Pattern(pattern) => pattern,
        }
    }
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
    /// Body scope node contains the catch clause parameter binding.
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
}

impl Expression {
    pub fn to_id(&self) -> &Identifier {
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

#[derive(Clone, Copy)]
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
    pub local: P<Identifier>,
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
    Id(ModuleNameIdentifier),
    String(StringLiteral),
}

pub struct ModuleNameIdentifier {
    pub loc: Loc,
    pub name: String,
}
