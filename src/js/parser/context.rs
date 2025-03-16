use std::rc::Rc;

use bumpalo::Bump;

use super::source::Source;

/// Context for a single invocation of the parser. Owns the entire AST.
pub struct ParseContext {
    /// The source file that is being parsed
    source: Rc<Source>,
    /// An arena allocator for the nodes parsed from the source
    alloc: Box<Bump>,
}

impl ParseContext {
    pub fn new(source: Rc<Source>) -> Self {
        Self { source, alloc: Box::new(Bump::new()) }
    }

    pub fn source(&self) -> &Rc<Source> {
        &self.source
    }

    pub fn alloc(&self) -> &Bump {
        &self.alloc
    }
}
