use std::{
    collections::hash_map::RandomState,
    fmt::{self, Debug},
    hash,
    marker::PhantomData,
    ptr::{self, NonNull},
};

use bitflags::bitflags;
use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashSet};
use indexmap_allocator_api::IndexMap;
use num_bigint::{BigInt, Sign};

use crate::js::{
    common::{
        alloc,
        wtf_8::{Wtf8Str, Wtf8String},
    },
    runtime::eval_result::EvalResult,
};

use super::{
    loc::{Loc, Pos, EMPTY_LOC},
    regexp::RegExp,
    scope_tree::{
        AstScopeNode, Binding, HOME_OBJECT_BINDING_NAME, STATIC_HOME_OBJECT_BINDING_NAME,
    },
};

pub type AstAlloc<'a> = &'a Bump;

pub type AstBox<'a, T> = alloc::Box<T, AstAlloc<'a>>;

pub type AstVec<'a, T> = alloc::Vec<T, AstAlloc<'a>>;

pub type AstStr<'a> = &'a Wtf8Str;

pub type AstString<'a> = Wtf8String<AstAlloc<'a>>;

pub type AstHashSet<'a, T> = HashSet<T, DefaultHashBuilder, AstAlloc<'a>>;

pub type AstIndexMap<'a, K, V> = IndexMap<K, V, RandomState, AstAlloc<'a>>;

pub type P<'a, T> = AstBox<'a, T>;

impl<'a> AstString<'a> {
    /// Convert `AstString` to an `AstStr` that has the same lifetime as the underlying arena.
    pub fn as_arena_str(&self) -> AstStr<'a> {
        unsafe { std::mem::transmute(<AstString<'a> as std::ops::Deref>::deref(self)) }
    }
}

impl<'a, T> AstPtr<T> {
    /// Convert `AstPtr` to a reference with the same lifetime as the underlying arena.
    pub fn as_arena_ref(&self) -> &'a T {
        unsafe { self.ptr.as_ref() }
    }
}

pub fn p<'a, T>(node: T) -> P<'a, T> {
    AstBox::new_in(node)
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

pub struct Program<'a> {
    pub loc: Loc,
    pub toplevels: AstVec<'a, Toplevel<'a>>,
    pub kind: ProgramKind,
    /// Whether the function is in strict mode, which could be inherited from surrounding context
    /// (e.g. in a direct eval or module)
    pub is_strict_mode: bool,
    /// Whether the program has a "use strict" directive
    pub has_use_strict_directive: bool,
    /// Whether this is a module with top level await. Set during analysis.
    pub has_top_level_await: bool,

    pub scope: AstPtr<AstScopeNode<'a>>,
}

impl<'a> Program<'a> {
    pub fn new(
        loc: Loc,
        toplevels: AstVec<'a, Toplevel<'a>>,
        kind: ProgramKind,
        scope: AstPtr<AstScopeNode<'a>>,
        is_strict_mode: bool,
        has_use_strict_directive: bool,
    ) -> Program<'a> {
        Program {
            loc,
            toplevels,
            kind,
            is_strict_mode,
            has_use_strict_directive,
            has_top_level_await: false,
            scope,
        }
    }
}

#[derive(PartialEq)]
pub enum ProgramKind {
    Script,
    Module,
}

pub enum Toplevel<'a> {
    Statement(Statement<'a>),
    Import(ImportDeclaration<'a>),
    ExportDefault(ExportDefaultDeclaration<'a>),
    ExportNamed(ExportNamedDeclaration<'a>),
    ExportAll(ExportAllDeclaration<'a>),
}

pub struct Identifier<'a> {
    pub loc: Loc,
    pub name: AstString<'a>,

    /// Reference to the scope that contains the binding for this identifier, or tagged as
    /// unresolved if the scope could not be statically determined.
    ///
    /// For defs this is set during parsing. For uses this is set during analysis.
    pub scope: TaggedResolvedScope<'a>,
}

impl<'a> Identifier<'a> {
    pub fn new(loc: Loc, name: AstString) -> Identifier {
        Identifier { loc, name, scope: TaggedResolvedScope::unresolved_global() }
    }

    pub fn get_binding(&self) -> &Binding {
        self.scope.unwrap_resolved().get_binding(&self.name)
    }

    pub fn get_private_name_binding(&self) -> &Binding {
        let private_name = AstString::from_string(format!("#{}", self.name));
        self.scope.unwrap_resolved().get_binding(&private_name)
    }
}

/// Reference to a scope node without lifetime constraints. Only valid to use while scope tree is
/// still live.
///
/// Reference is represented as a tagged pointer, with tags indicating whether the scope was
/// resolved and if it needs to be looked up dynamically or directly from global scope.
#[derive(Clone, Copy)]
pub struct TaggedResolvedScope<'a> {
    ptr: *mut u8,
    data: PhantomData<AstScopeNode<'a>>,
}

pub enum ResolvedScope {
    /// Scope was not resolved, and name must be looked up in global scope.
    UnresolvedGlobal,
    /// Scope was not resolved, and name must be looked up dynamically in scope chain.
    UnresolvedDynamic,
    /// Scope was resolved to a specific scope node.
    Resolved,
}

impl<'a> TaggedResolvedScope<'a> {
    pub const fn unresolved_global() -> TaggedResolvedScope<'static> {
        TaggedResolvedScope { ptr: ptr::null_mut(), data: PhantomData }
    }

    pub const fn unresolved_dynamic() -> TaggedResolvedScope<'static> {
        TaggedResolvedScope { ptr: 1 as *mut u8, data: PhantomData }
    }

    pub const fn resolved(scope: AstPtr<AstScopeNode>) -> TaggedResolvedScope {
        TaggedResolvedScope { ptr: scope.ptr.as_ptr() as *mut u8, data: PhantomData }
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

    pub fn unwrap_resolved(&self) -> &AstScopeNode<'a> {
        if self.is_unresolved() {
            panic!("Expected resolved scope")
        }

        unsafe { &*(self.ptr as *mut AstScopeNode) }
    }

    pub fn unwrap_resolved_mut(&mut self) -> &mut AstScopeNode<'a> {
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

pub enum Statement<'a> {
    VarDecl(VariableDeclaration<'a>),
    FuncDecl(P<'a, Function<'a>>),
    ClassDecl(P<'a, Class<'a>>),
    Expr(ExpressionStatement<'a>),
    Block(Block<'a>),
    If(IfStatement<'a>),
    Switch(SwitchStatement<'a>),
    For(ForStatement<'a>),
    ForEach(ForEachStatement<'a>),
    While(WhileStatement<'a>),
    DoWhile(DoWhileStatement<'a>),
    With(WithStatement<'a>),
    Try(TryStatement<'a>),
    Throw(ThrowStatement<'a>),
    Return(ReturnStatement<'a>),
    Break(BreakStatement<'a>),
    Continue(ContinueStatement<'a>),
    Labeled(LabeledStatement<'a>),
    Empty(Loc),
    Debugger(Loc),
}

#[derive(PartialEq)]
pub enum VarKind {
    Var,
    Let,
    Const,
}

pub struct VariableDeclaration<'a> {
    pub loc: Loc,
    pub kind: VarKind,
    pub declarations: AstVec<'a, VariableDeclarator<'a>>,
}

pub struct VariableDeclarator<'a> {
    pub loc: Loc,
    pub id: P<'a, Pattern<'a>>,
    pub init: Option<P<'a, OuterExpression<'a>>>,
    /// Whether an assignment expression appears in the pattern.
    pub id_has_assign_expr: bool,
}

impl<'a> VariableDeclarator<'a> {
    pub fn new(
        loc: Loc,
        id: P<'a, Pattern<'a>>,
        init: Option<P<'a, OuterExpression<'a>>>,
    ) -> VariableDeclarator<'a> {
        VariableDeclarator { loc, id, init, id_has_assign_expr: false }
    }

    pub fn iter_bound_names<F: FnMut(&'a Identifier<'a>) -> EvalResult<()>>(
        &self,
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

pub struct Function<'a> {
    pub loc: Loc,
    pub id: Option<P<'a, Identifier<'a>>>,
    pub params: AstVec<'a, FunctionParam<'a>>,
    pub body: P<'a, FunctionBody<'a>>,
    pub flags: FunctionFlags,

    /// Scope node for the function, containing function parameters and the body.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

impl<'a> Function<'a> {
    pub fn new_uninit() -> Function<'a> {
        Function {
            // Default values that will be overwritten by `init`
            loc: EMPTY_LOC,
            id: None,
            params: alloc::vec![],
            body: p(FunctionBody::Block(FunctionBlockBody {
                loc: EMPTY_LOC,
                body: alloc::vec![],
                scope: None,
            })),
            flags: FunctionFlags::empty(),
            scope: AstPtr::uninit(),
        }
    }

    pub fn init(
        &mut self,
        loc: Loc,
        id: Option<P<'a, Identifier<'a>>>,
        params: AstVec<'a, FunctionParam<'a>>,
        body: P<'a, FunctionBody<'a>>,
        flags: FunctionFlags,
        scope: AstPtr<AstScopeNode<'a>>,
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
        id: Option<P<'a, Identifier<'a>>>,
        params: AstVec<'a, FunctionParam<'a>>,
        body: P<'a, FunctionBody<'a>>,
        flags: FunctionFlags,
        scope: AstPtr<AstScopeNode<'a>>,
    ) -> Function<'a> {
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

pub enum FunctionParam<'a> {
    Pattern {
        pattern: Pattern<'a>,
        /// Whether the pattern has an assignment expression.
        has_assign_expr: bool,
    },
    Rest {
        rest: RestElement<'a>,
        /// Whether the rest element has an assignment expression.
        has_assign_expr: bool,
    },
}

impl FunctionParam<'_> {
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

    pub fn iter_patterns<'a, F: FnMut(&'a Pattern)>(&self, f: &mut F) {
        match &self {
            FunctionParam::Pattern { pattern, .. } => pattern.iter_patterns(f),
            FunctionParam::Rest { rest: RestElement { argument, .. }, .. } => {
                argument.iter_patterns(f)
            }
        }
    }
}

pub enum FunctionBody<'a> {
    Block(FunctionBlockBody<'a>),
    Expression(OuterExpression<'a>),
}

impl<'a> FunctionBody<'a> {
    pub fn unwrap_block(&self) -> &FunctionBlockBody {
        match self {
            FunctionBody::Block(block) => block,
            _ => panic!("Expected block body"),
        }
    }
}

pub struct FunctionBlockBody<'a> {
    pub loc: Loc,
    pub body: AstVec<'a, Statement<'a>>,

    /// Scope node for the function body, not including parameters. Only present if the function has
    /// parameter expressions, otherwise only the function's scope node is needed.
    pub scope: Option<AstPtr<AstScopeNode<'a>>>,
}

pub struct Class<'a> {
    pub loc: Loc,
    pub id: Option<P<'a, Identifier<'a>>>,
    pub super_class: Option<P<'a, OuterExpression<'a>>>,
    pub body: AstVec<'a, ClassElement<'a>>,

    pub constructor: Option<AstPtr<ClassMethod<'a>>>,

    /// Scope node for the class body
    pub scope: AstPtr<AstScopeNode<'a>>,

    /// Scope node for the instance fields initializer. Only present if the class has instance
    /// fields.
    pub fields_initializer_scope: Option<AstPtr<AstScopeNode<'a>>>,

    /// Scope node for the static initializer, including static fields and initializer blocks.
    /// Only present if the class has static fields or static initializer blocks.
    pub static_initializer_scope: Option<AstPtr<AstScopeNode<'a>>>,
}

impl<'a> Class<'a> {
    pub fn new(
        loc: Loc,
        id: Option<P<'a, Identifier<'a>>>,
        super_class: Option<P<'a, OuterExpression<'a>>>,
        body: AstVec<'a, ClassElement<'a>>,
        scope: AstPtr<AstScopeNode<'a>>,
        fields_initializer_scope: Option<AstPtr<AstScopeNode<'a>>>,
        static_initializer_scope: Option<AstPtr<AstScopeNode<'a>>>,
    ) -> Class<'a> {
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

pub enum ClassElement<'a> {
    Method(ClassMethod<'a>),
    Property(ClassProperty<'a>),
}

pub struct ClassMethod<'a> {
    pub loc: Loc,
    pub key: P<'a, OuterExpression<'a>>,
    pub value: P<'a, Function<'a>>,
    pub kind: ClassMethodKind,
    pub is_computed: bool,
    pub is_static: bool,
    pub is_private: bool,
    /// Whether this is the first part of a private accessor pair, where the second part appears
    /// laster in the class. Set during analysis.
    pub is_private_pair_start: bool,
}

impl<'a> ClassMethod<'a> {
    pub fn new(
        loc: Loc,
        key: P<'a, OuterExpression<'a>>,
        value: P<'a, Function<'a>>,
        kind: ClassMethodKind,
        is_computed: bool,
        is_static: bool,
        is_private: bool,
    ) -> ClassMethod<'a> {
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

pub struct ClassProperty<'a> {
    pub loc: Loc,
    pub key: P<'a, OuterExpression<'a>>,
    pub value: Option<P<'a, OuterExpression<'a>>>,
    pub is_computed: bool,
    pub is_static: bool,
    pub is_private: bool,
}

pub struct ExpressionStatement<'a> {
    pub loc: Loc,
    pub expr: P<'a, OuterExpression<'a>>,
}

pub struct Block<'a> {
    pub loc: Loc,
    pub body: AstVec<'a, Statement<'a>>,

    /// Block scope node for the block.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

pub struct IfStatement<'a> {
    pub loc: Loc,
    pub test: P<'a, OuterExpression<'a>>,
    pub conseq: P<'a, Statement<'a>>,
    pub altern: Option<P<'a, Statement<'a>>>,
}

pub struct SwitchStatement<'a> {
    pub loc: Loc,
    pub discriminant: P<'a, OuterExpression<'a>>,
    pub cases: AstVec<'a, SwitchCase<'a>>,

    /// Block scope node for the switch statement body.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

pub struct SwitchCase<'a> {
    pub loc: Loc,
    pub test: Option<P<'a, OuterExpression<'a>>>,
    pub body: AstVec<'a, Statement<'a>>,
}

pub struct ForStatement<'a> {
    pub loc: Loc,
    pub init: Option<P<'a, ForInit<'a>>>,
    pub test: Option<P<'a, OuterExpression<'a>>>,
    pub update: Option<P<'a, OuterExpression<'a>>>,
    pub body: P<'a, Statement<'a>>,

    /// Block scope node that contains the for statement variable declarations and the body.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

pub enum ForInit<'a> {
    Expression(OuterExpression<'a>),
    VarDecl(VariableDeclaration<'a>),
}

pub struct ForEachStatement<'a> {
    pub loc: Loc,
    pub kind: ForEachKind,
    pub left: P<'a, ForEachInit<'a>>,
    pub right: P<'a, OuterExpression<'a>>,
    pub body: P<'a, Statement<'a>>,
    pub is_await: bool,

    /// Source position of start of the `in` or `of` keyword.
    pub in_of_pos: Pos,

    /// Block scope node that contains the for statement variable declarations and the body.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

#[derive(PartialEq)]
pub enum ForEachKind {
    In,
    Of,
}

pub enum ForEachInit<'a> {
    VarDecl(VariableDeclaration<'a>),
    Pattern {
        pattern: Pattern<'a>,
        /// Whether an assignment expression appears in the pattern.
        has_assign_expr: bool,
    },
}

impl<'a> ForEachInit<'a> {
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

pub struct WhileStatement<'a> {
    pub loc: Loc,
    pub test: P<'a, OuterExpression<'a>>,
    pub body: P<'a, Statement<'a>>,
}

pub struct DoWhileStatement<'a> {
    pub loc: Loc,
    pub test: P<'a, OuterExpression<'a>>,
    pub body: P<'a, Statement<'a>>,
}

pub struct WithStatement<'a> {
    pub loc: Loc,
    pub object: P<'a, OuterExpression<'a>>,
    pub body: P<'a, Statement<'a>>,

    /// Scope node for the with statement body
    pub scope: AstPtr<AstScopeNode<'a>>,
}

pub struct TryStatement<'a> {
    pub loc: Loc,
    pub block: P<'a, Block<'a>>,
    pub handler: Option<P<'a, CatchClause<'a>>>,
    pub finalizer: Option<P<'a, Block<'a>>>,
}

pub struct CatchClause<'a> {
    pub loc: Loc,
    pub param: Option<P<'a, Pattern<'a>>>,
    pub body: P<'a, Block<'a>>,
    /// Whether the parameter is a pattern with an assignment expression.
    pub param_has_assign_expr: bool,

    /// Block scope node that contains the catch clause parameter binding and body.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

impl<'a> CatchClause<'a> {
    pub fn new(
        loc: Loc,
        param: Option<P<'a, Pattern<'a>>>,
        body: P<'a, Block<'a>>,
        scope: AstPtr<AstScopeNode<'a>>,
    ) -> CatchClause<'a> {
        CatchClause { loc, param, body, param_has_assign_expr: false, scope }
    }
}

pub struct ThrowStatement<'a> {
    pub loc: Loc,
    pub argument: P<'a, OuterExpression<'a>>,
}

pub struct ReturnStatement<'a> {
    pub loc: Loc,
    pub argument: Option<P<'a, OuterExpression<'a>>>,

    /// Reference to the scope that contains the binding for `this`. Similar to the scope in the
    /// `ThisExpression` node, but only set if this return is in a derived constructor (meaning the
    /// derived constructor's `this` may be implicitly returned).
    pub this_scope: Option<AstPtr<AstScopeNode<'a>>>,
}

impl<'a> ReturnStatement<'a> {
    pub fn new(loc: Loc, argument: Option<P<'a, OuterExpression<'a>>>) -> ReturnStatement<'a> {
        ReturnStatement { loc, argument, this_scope: None }
    }
}

pub struct BreakStatement<'a> {
    pub loc: Loc,
    pub label: Option<Label<'a>>,
}

pub struct ContinueStatement<'a> {
    pub loc: Loc,
    pub label: Option<Label<'a>>,
}

pub struct LabeledStatement<'a> {
    pub loc: Loc,
    pub label: P<'a, Label<'a>>,
    pub body: P<'a, Statement<'a>>,
}

pub type LabelId = u16;

pub struct Label<'a> {
    pub loc: Loc,
    pub name: AstString<'a>,
    pub id: LabelId,
}

impl<'a> Label<'a> {
    pub fn new(loc: Loc, name: AstString<'a>) -> Label<'a> {
        Label { loc, name, id: 0 }
    }
}

/// An entire expression held by a statement. Contains additional metadata about the expression.
///
/// Only expressions may hold Expressions directly. All other nodes must hold an OuterExpression
/// so that metadata can be associated with the entire wrapped expression.
pub struct OuterExpression<'a> {
    /// The entire expression that is being wrapped.
    pub expr: Expression<'a>,
    /// Whether an assignment expression appears in the expression.
    pub has_assign_expr: bool,
}

impl<'a> OuterExpression<'a> {
    /// The source position of the start of the expression.
    pub fn pos(&self) -> Pos {
        self.expr.loc().start
    }
}

pub enum Expression<'a> {
    Id(Identifier<'a>),
    Null(Loc),
    Boolean(BooleanLiteral),
    Number(NumberLiteral),
    String(StringLiteral<'a>),
    BigInt(BigIntLiteral<'a>),
    RegExp(RegExpLiteral<'a>),
    Unary(UnaryExpression<'a>),
    Binary(BinaryExpression<'a>),
    Logical(LogicalExpression<'a>),
    Assign(AssignmentExpression<'a>),
    Update(UpdateExpression<'a>),
    Member(MemberExpression<'a>),
    Chain(ChainExpression<'a>),
    Conditional(ConditionalExpression<'a>),
    Call(CallExpression<'a>),
    New(NewExpression<'a>),
    Sequence(SequenceExpression<'a>),
    Array(ArrayExpression<'a>),
    Object(ObjectExpression<'a>),
    Function(P<'a, Function<'a>>),
    ArrowFunction(P<'a, Function<'a>>),
    Class(P<'a, Class<'a>>),
    This(ThisExpression<'a>),
    Await(AwaitExpression<'a>),
    Yield(YieldExpression<'a>),
    SuperMember(SuperMemberExpression<'a>),
    SuperCall(P<'a, SuperCallExpression<'a>>),
    Template(TemplateLiteral<'a>),
    TaggedTemplate(TaggedTemplateExpression<'a>),
    MetaProperty(MetaProperty<'a>),
    Import(ImportExpression<'a>),
}

impl<'a> Expression<'a> {
    pub fn to_id(&self) -> &Identifier {
        match self {
            Expression::Id(id) => id,
            _ => panic!("Expected identifier expression"),
        }
    }

    pub fn to_id_mut(&mut self) -> &mut Identifier<'a> {
        match self {
            Expression::Id(id) => id,
            _ => panic!("Expected identifier expression"),
        }
    }

    /// The source location of the expression.
    pub fn loc(&self) -> Loc {
        match self {
            Expression::Id(id) => id.loc,
            Expression::Null(loc) => *loc,
            Expression::Boolean(lit) => lit.loc,
            Expression::Number(lit) => lit.loc,
            Expression::String(lit) => lit.loc,
            Expression::BigInt(lit) => lit.loc,
            Expression::RegExp(lit) => lit.loc,
            Expression::Unary(expr) => expr.loc,
            Expression::Binary(expr) => expr.loc,
            Expression::Logical(expr) => expr.loc,
            Expression::Assign(expr) => expr.loc,
            Expression::Update(expr) => expr.loc,
            Expression::Member(expr) => expr.loc,
            Expression::Chain(expr) => expr.loc,
            Expression::Conditional(expr) => expr.loc,
            Expression::Call(expr) => expr.loc,
            Expression::New(expr) => expr.loc,
            Expression::Sequence(expr) => expr.loc,
            Expression::Array(expr) => expr.loc,
            Expression::Object(expr) => expr.loc,
            Expression::Function(expr) => expr.loc,
            Expression::ArrowFunction(expr) => expr.loc,
            Expression::Class(expr) => expr.loc,
            Expression::This(expr) => expr.loc,
            Expression::Await(expr) => expr.loc,
            Expression::Yield(expr) => expr.loc,
            Expression::SuperMember(expr) => expr.loc,
            Expression::SuperCall(expr) => expr.loc,
            Expression::Template(expr) => expr.loc,
            Expression::TaggedTemplate(expr) => expr.loc,
            Expression::MetaProperty(expr) => expr.loc,
            Expression::Import(expr) => expr.loc,
        }
    }

    /// The source position of the start of the expression.
    pub fn pos(&self) -> Pos {
        self.loc().start
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

pub struct StringLiteral<'a> {
    pub loc: Loc,
    pub value: AstString<'a>,
}

pub struct BigIntLiteral<'a> {
    pub loc: Loc,
    /// Sign of the BigInt value.
    sign: Sign,
    /// Digits of the BigInt value.
    digits: AstVec<'a, u32>,
}

impl<'a> BigIntLiteral<'a> {
    pub fn new(loc: Loc, value: BigInt) -> BigIntLiteral<'a> {
        let (sign, unsigned) = value.into_parts();
        BigIntLiteral { loc, sign, digits: unsigned.to_u32_digits() }
    }

    /// Return a clone of the BigInt value.
    pub fn value(&self) -> BigInt {
        BigInt::from_slice(self.sign, &self.digits)
    }
}

pub struct RegExpLiteral<'a> {
    pub loc: Loc,
    pub raw: P<'a, AstString<'a>>,
    pub pattern: P<'a, AstString<'a>>,
    pub flags: P<'a, AstString<'a>>,
    pub regexp: P<'a, RegExp<'a>>,
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

pub struct UnaryExpression<'a> {
    pub loc: Loc,
    pub operator: UnaryOperator,
    pub argument: P<'a, Expression<'a>>,
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

pub struct BinaryExpression<'a> {
    pub loc: Loc,
    pub operator: BinaryOperator,
    pub left: P<'a, Expression<'a>>,
    pub right: P<'a, Expression<'a>>,

    /// Source position of the start of the operator
    pub operator_pos: Pos,
}

#[derive(PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    NullishCoalesce,
}

pub struct LogicalExpression<'a> {
    pub loc: Loc,
    pub operator: LogicalOperator,
    pub left: P<'a, Expression<'a>>,
    pub right: P<'a, Expression<'a>>,
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

pub struct AssignmentExpression<'a> {
    pub loc: Loc,
    pub operator: AssignmentOperator,
    pub left: P<'a, Pattern<'a>>,
    pub right: P<'a, Expression<'a>>,

    /// Source position of the start of the operator
    pub operator_pos: Pos,

    /// Needed for reparsing into a pattern
    pub is_parenthesized: bool,
}

#[derive(Copy, Clone)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}

pub struct UpdateExpression<'a> {
    pub loc: Loc,
    pub operator: UpdateOperator,
    pub argument: P<'a, Expression<'a>>,
    pub is_prefix: bool,
}

pub struct MemberExpression<'a> {
    pub loc: Loc,
    pub object: P<'a, Expression<'a>>,
    pub property: P<'a, Expression<'a>>,
    pub is_computed: bool,
    pub is_optional: bool,
    pub is_private: bool,

    /// Source position of the `.` or `[` token
    pub operator_pos: Pos,
}

pub struct ChainExpression<'a> {
    pub loc: Loc,
    pub expression: P<'a, Expression<'a>>,
}

pub struct ConditionalExpression<'a> {
    pub loc: Loc,
    pub test: P<'a, Expression<'a>>,
    pub altern: P<'a, Expression<'a>>,
    pub conseq: P<'a, Expression<'a>>,
}

pub struct CallExpression<'a> {
    pub loc: Loc,
    pub callee: P<'a, Expression<'a>>,
    pub arguments: AstVec<'a, CallArgument<'a>>,
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

impl<'a> CallExpression<'a> {
    pub fn new(
        loc: Loc,
        callee: P<'a, Expression<'a>>,
        arguments: AstVec<'a, CallArgument<'a>>,
        is_optional: bool,
    ) -> CallExpression<'a> {
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

pub enum CallArgument<'a> {
    Expression(Expression<'a>),
    Spread(SpreadElement<'a>),
}

pub struct NewExpression<'a> {
    pub loc: Loc,
    pub callee: P<'a, Expression<'a>>,
    pub arguments: AstVec<'a, CallArgument<'a>>,
}

pub struct SequenceExpression<'a> {
    pub loc: Loc,
    pub expressions: AstVec<'a, Expression<'a>>,
}

pub struct ArrayExpression<'a> {
    pub loc: Loc,
    pub elements: AstVec<'a, ArrayElement<'a>>,
    // Needed for reparsing into a pattern
    pub is_parenthesized: bool,
}

pub enum ArrayElement<'a> {
    Expression(Expression<'a>),
    Spread(SpreadElement<'a>),
    Hole(Pos),
}

pub struct SpreadElement<'a> {
    pub loc: Loc,
    pub argument: P<'a, Expression<'a>>,
    pub has_trailing_comma: bool,
}

pub struct ObjectExpression<'a> {
    pub loc: Loc,
    pub properties: AstVec<'a, Property<'a>>,
    // Needed for reparsing into a pattern
    pub is_parenthesized: bool,

    /// Scope node for the object body, will only contain the home object.
    pub scope: AstPtr<AstScopeNode<'a>>,
}

pub struct Property<'a> {
    pub loc: Loc,
    pub key: P<'a, Expression<'a>>,
    pub value: Option<P<'a, Expression<'a>>>,
    pub is_computed: bool,
    pub is_method: bool,
    pub kind: PropertyKind<'a>,
}

pub enum PropertyKind<'a> {
    Init,
    Get,
    Set,
    // For spread properties the key is the argument and all other fields are ignored. The single
    // bool argument is whether the spread property is followed by a comma.
    Spread(bool),
    // A pattern initializer that is not valid in a final AST, but can be reparsed into an object
    // pattern property with an initializer. The single expression argument is the initializer. If
    // a PatternInitializer is found during analysis the analyzer will error.
    PatternInitializer(P<'a, Expression<'a>>),
}

pub struct ThisExpression<'a> {
    pub loc: Loc,
    /// Reference to the scope that contains the binding for `this`, which may be a parent scope if
    /// `this` is captured by an arrow function.
    ///
    /// Starts out uninitialized during parsing and is set during analysis. Only set if resolved to
    /// a captured `this` binding, or if referring to `this` of a derived constructor.
    pub scope: Option<AstPtr<AstScopeNode<'a>>>,
}

pub struct AwaitExpression<'a> {
    pub loc: Loc,
    pub argument: P<'a, Expression<'a>>,
}

pub struct YieldExpression<'a> {
    pub loc: Loc,
    pub argument: Option<P<'a, Expression<'a>>>,
    pub is_delegate: bool,
}

pub struct SuperMemberExpression<'a> {
    pub loc: Loc,
    pub super_: Loc,
    pub property: P<'a, Expression<'a>>,
    pub is_computed: bool,

    /// Whether this super member expression is in a static method. Set during analysis.
    pub is_static: bool,

    /// Reference to the scope that contains the binding for `this`. Treated the same as the scope
    /// in the `ThisExpression` node.
    pub this_scope: Option<AstPtr<AstScopeNode<'a>>>,

    /// Reference to the scope that contains the binding for the home object referenced by this
    /// super expression, or tagged as unresolved dynamic if the scope could not be statically
    /// determined.
    pub home_object_scope: TaggedResolvedScope<'a>,

    /// Source position of the `.` or `[` token
    pub operator_pos: Pos,
}

impl<'a> SuperMemberExpression<'a> {
    pub fn new(
        loc: Loc,
        super_: Loc,
        operator_pos: Pos,
        property: P<'a, Expression<'a>>,
        is_computed: bool,
    ) -> Self {
        Self {
            loc,
            super_,
            property,
            is_computed,
            is_static: false,
            this_scope: None,
            home_object_scope: TaggedResolvedScope::unresolved_global(),
            operator_pos,
        }
    }

    pub fn home_object_name(&self) -> &'static Wtf8String {
        if self.is_static {
            &STATIC_HOME_OBJECT_BINDING_NAME
        } else {
            &HOME_OBJECT_BINDING_NAME
        }
    }
}

pub struct SuperCallExpression<'a> {
    pub loc: Loc,
    pub super_: Loc,
    pub arguments: AstVec<'a, CallArgument<'a>>,

    /// Reference to the function scope that contains the binding for the containing derived
    /// constructor, or tagged as unresolved dynamic if the scope could not be statically determined.
    pub constructor_scope: TaggedResolvedScope<'a>,

    /// Reference to the function scope that contains the binding for this new.target, or tagged
    /// as unresolved dynamic if the scope could not be statically determined.
    pub new_target_scope: TaggedResolvedScope<'a>,

    /// Reference to the scope that contains the binding for `this`. Similar to the scope in the
    /// `ThisExpression` node, but is always set since it refers to a derived constructor's `this`.
    pub this_scope: AstPtr<AstScopeNode<'a>>,
}

impl<'a> SuperCallExpression<'a> {
    pub fn new(loc: Loc, super_loc: Loc, arguments: AstVec<'a, CallArgument<'a>>) -> Self {
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

pub struct TemplateLiteral<'a> {
    pub loc: Loc,
    pub quasis: AstVec<'a, TemplateElement<'a>>,
    pub expressions: AstVec<'a, Expression<'a>>,
}

pub struct TemplateElement<'a> {
    pub loc: Loc,
    pub raw: AstString<'a>,
    /// Guaranteed to exist for template literals. Tagged templates allow this to be None.
    pub cooked: Option<AstString<'a>>,
}

pub struct TaggedTemplateExpression<'a> {
    pub loc: Loc,
    pub tag: P<'a, Expression<'a>>,
    pub quasi: P<'a, TemplateLiteral<'a>>,
}

pub struct MetaProperty<'a> {
    pub loc: Loc,
    pub kind: MetaPropertyKind<'a>,
}

impl<'a> MetaProperty<'a> {
    pub fn new_target(loc: Loc) -> MetaProperty<'a> {
        MetaProperty {
            loc,
            kind: MetaPropertyKind::NewTarget { scope: TaggedResolvedScope::unresolved_global() },
        }
    }
}

#[derive(Clone, Copy)]
pub enum MetaPropertyKind<'a> {
    NewTarget {
        /// Reference to the function scope that contains the binding for this new.target, or tagged
        /// as unresolved dynamic if the scope could not be statically determined.
        scope: TaggedResolvedScope<'a>,
    },
    ImportMeta,
}

pub struct ImportExpression<'a> {
    pub loc: Loc,
    pub source: P<'a, Expression<'a>>,
    pub options: Option<P<'a, Expression<'a>>>,
}

pub enum Pattern<'a> {
    Id(Identifier<'a>),
    Array(ArrayPattern<'a>),
    Object(ObjectPattern<'a>),
    Assign(AssignmentPattern<'a>),
    Member(MemberExpression<'a>),
    SuperMember(SuperMemberExpression<'a>),
}

impl<'a> Pattern<'a> {
    pub fn to_id(&self) -> &Identifier {
        match self {
            Pattern::Id(id) => id,
            _ => panic!("Expected identifier pattern"),
        }
    }

    pub fn is_id(&self) -> bool {
        matches!(self, Pattern::Id(_))
    }

    pub fn iter_patterns<F: FnMut(&'a Pattern<'a>)>(&self, f: &mut F) {
        f(self);

        match &self {
            Pattern::Id(_) => {}
            Pattern::Array(patt) => {
                for element in &patt.elements {
                    match element {
                        ArrayPatternElement::Pattern(pattern) => pattern.iter_patterns(f),
                        ArrayPatternElement::Rest(rest) => rest.argument.iter_patterns(f),
                        ArrayPatternElement::Hole(_) => {}
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

    pub fn iter_bound_names<F: FnMut(&'a Identifier<'a>) -> EvalResult<()>>(
        &self,
        f: &mut F,
    ) -> EvalResult<()> {
        match &self {
            Pattern::Id(id) => f(id),
            Pattern::Array(patt) => patt.iter_bound_names(f),
            Pattern::Object(patt) => patt.iter_bound_names(f),
            Pattern::Assign(patt) => patt.iter_bound_names(f),
            Pattern::Member(_) | Pattern::SuperMember(_) => Ok(()),
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            Pattern::Id(patt) => patt.loc,
            Pattern::Array(patt) => patt.loc,
            Pattern::Object(patt) => patt.loc,
            Pattern::Assign(patt) => patt.loc,
            Pattern::Member(expr) => expr.loc,
            Pattern::SuperMember(expr) => expr.loc,
        }
    }

    /// The source position of the start of the pattern.
    pub fn pos(&self) -> Pos {
        self.loc().start
    }
}

pub struct ArrayPattern<'a> {
    pub loc: Loc,
    pub elements: AstVec<'a, ArrayPatternElement<'a>>,
}

pub enum ArrayPatternElement<'a> {
    Pattern(Pattern<'a>),
    Rest(RestElement<'a>),
    Hole(Pos),
}

impl<'a> ArrayPattern<'a> {
    pub fn iter_bound_names<F: FnMut(&'a Identifier<'a>) -> EvalResult<()>>(
        &self,
        f: &mut F,
    ) -> EvalResult<()> {
        for element in &self.elements {
            match element {
                ArrayPatternElement::Pattern(pattern) => pattern.iter_bound_names(f)?,
                ArrayPatternElement::Rest(RestElement { argument, .. }) => {
                    argument.iter_bound_names(f)?
                }
                ArrayPatternElement::Hole(_) => {}
            }
        }

        Ok(())
    }
}

pub struct RestElement<'a> {
    pub loc: Loc,
    pub argument: P<'a, Pattern<'a>>,
}

pub struct ObjectPattern<'a> {
    pub loc: Loc,
    pub properties: AstVec<'a, ObjectPatternProperty<'a>>,
}

impl<'a> ObjectPattern<'a> {
    pub fn iter_bound_names<F: FnMut(&'a Identifier<'a>) -> EvalResult<()>>(
        &self,
        f: &mut F,
    ) -> EvalResult<()> {
        for prop in &self.properties {
            prop.value.iter_bound_names(f)?
        }

        Ok(())
    }
}

pub struct ObjectPatternProperty<'a> {
    pub loc: Loc,
    pub key: Option<P<'a, Expression<'a>>>,
    pub value: P<'a, Pattern<'a>>,
    pub is_computed: bool,
    // For rest properties the value is the argument and must be an id. All other fields are ignored.
    pub is_rest: bool,
}

pub struct AssignmentPattern<'a> {
    pub loc: Loc,
    pub left: P<'a, Pattern<'a>>,
    pub right: P<'a, Expression<'a>>,
}

impl<'a> AssignmentPattern<'a> {
    pub fn iter_bound_names<F: FnMut(&'a Identifier<'a>) -> EvalResult<()>>(
        &self,
        f: &mut F,
    ) -> EvalResult<()> {
        self.left.iter_bound_names(f)
    }
}

pub struct ImportDeclaration<'a> {
    pub loc: Loc,
    pub specifiers: AstVec<'a, ImportSpecifier<'a>>,
    pub source: P<'a, StringLiteral<'a>>,
    pub attributes: Option<P<'a, ImportAttributes<'a>>>,
}

pub struct ImportAttributes<'a> {
    pub attributes: AstVec<'a, ImportAttribute<'a>>,
}

pub struct ImportAttribute<'a> {
    pub loc: Loc,
    // Must be a string literal or identifier
    pub key: P<'a, Expression<'a>>,
    pub value: P<'a, StringLiteral<'a>>,
}

pub enum ImportSpecifier<'a> {
    Default(ImportDefaultSpecifier<'a>),
    Named(ImportNamedSpecifier<'a>),
    Namespace(ImportNamespaceSpecifier<'a>),
}

pub struct ImportDefaultSpecifier<'a> {
    pub loc: Loc,
    pub local: P<'a, Identifier<'a>>,
}

pub struct ImportNamedSpecifier<'a> {
    pub loc: Loc,
    pub imported: Option<P<'a, ExportName<'a>>>,
    pub local: P<'a, Identifier<'a>>,
}

pub struct ImportNamespaceSpecifier<'a> {
    pub loc: Loc,
    pub local: P<'a, Identifier<'a>>,
}

pub struct ExportNamedDeclaration<'a> {
    pub loc: Loc,
    // Must be variable declaration, function declaration, or class declaration
    pub declaration: Option<P<'a, Statement<'a>>>,
    pub specifiers: AstVec<'a, ExportSpecifier<'a>>,
    pub source: Option<P<'a, StringLiteral<'a>>>,
    pub source_attributes: Option<P<'a, ImportAttributes<'a>>>,
}

impl<'a> ExportNamedDeclaration<'a> {
    pub fn iter_declaration_ids(&self, f: &mut impl FnMut(&'a Identifier<'a>)) {
        if let Some(declaration) = self.declaration.as_deref() {
            match declaration {
                Statement::VarDecl(VariableDeclaration { declarations, .. }) => {
                    for decl in declarations {
                        let _ = decl.iter_bound_names(&mut |id| {
                            f(id);
                            Ok(())
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

pub struct ExportSpecifier<'a> {
    pub loc: Loc,
    /// Guaranteed to be an identifier if declaration is not a re-export declaration.
    pub local: P<'a, ExportName<'a>>,
    pub exported: Option<P<'a, ExportName<'a>>>,
}

pub struct ExportDefaultDeclaration<'a> {
    pub loc: Loc,
    pub declaration: ExportDefaultKind<'a>,
}

impl<'a> ExportDefaultDeclaration<'a> {
    pub fn id(&self) -> Option<&Identifier<'a>> {
        match &self.declaration {
            ExportDefaultKind::Function(func) => func.id.as_deref(),
            ExportDefaultKind::Class(class) => class.id.as_deref(),
            ExportDefaultKind::Expression(_) => None,
        }
    }
}

pub enum ExportDefaultKind<'a> {
    Function(P<'a, Function<'a>>),
    Class(P<'a, Class<'a>>),
    Expression(P<'a, OuterExpression<'a>>),
}

pub struct ExportAllDeclaration<'a> {
    pub loc: Loc,
    pub exported: Option<P<'a, ExportName<'a>>>,
    pub source: P<'a, StringLiteral<'a>>,
    pub source_attributes: Option<P<'a, ImportAttributes<'a>>>,
}

/// The name of an export that other modules must reference when importing. Must be well formed
/// unicode (which is already implicitly true for Identifiers).
pub enum ExportName<'a> {
    Id(Identifier<'a>),
    String(StringLiteral<'a>),
}

impl<'a> ExportName<'a> {
    pub fn to_id(&self) -> &Identifier {
        match self {
            ExportName::Id(id) => id,
            _ => panic!("Expected identifier export name"),
        }
    }

    pub fn to_id_mut(&mut self) -> &mut Identifier<'a> {
        match self {
            ExportName::Id(id) => id,
            _ => panic!("Expected identifier export name"),
        }
    }
}
