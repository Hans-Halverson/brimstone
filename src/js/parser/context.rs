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

    pub fn stats(&self) -> ParseStats {
        ParseStats {
            name: self.source.display_name().to_owned(),
            source_size: self.source.contents.len(),
            total_arena_size: self.alloc.allocated_bytes_including_metadata(),
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
pub struct ParseStats {
    /// Display name of the source file
    name: String,
    /// Size of the source file in bytes
    source_size: usize,
    /// Total size of the arena allocated for the AST in bytes. Includes arena metadata and space
    /// allocated for the arena but not yet used.
    total_arena_size: usize,
}
