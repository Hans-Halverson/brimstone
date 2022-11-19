/// Start and end positions are byte offsets in the source file.
pub type Pos = usize;

/// Half open interval describing a source location.
#[derive(Clone, Copy, Debug)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
}

pub const EMPTY_LOC: Loc = Loc { start: 0, end: 0 };
