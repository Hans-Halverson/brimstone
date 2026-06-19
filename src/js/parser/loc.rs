use std::ops::Range;

use crate::common::unicode::{CodePoint, decode_wtf8_codepoint, is_ascii};

/// Start and end positions are byte offsets in the source file.
pub type Pos = usize;

/// Half open interval describing a source location.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
}

impl Loc {
    pub fn to_range(self) -> Range<Pos> {
        self.start..self.end
    }
}

pub const EMPTY_LOC: Loc = Loc { start: 0, end: 0 };

/// Pos to use as a sentinel value when a Pos is not available.
pub const NO_POS: Pos = usize::MAX;

/// Calculate the byte offsets of the start of each line.
pub fn calculate_line_offsets(source: &[u8]) -> Vec<u32> {
    let mut line_offsets = vec![0];

    let mut pos = 0;

    loop {
        match source[pos..]
            .iter()
            .enumerate()
            .find(|(_, byte)| **byte == b'\n')
        {
            None => break,
            Some((newline_offset, _)) => {
                pos += newline_offset + 1;
                // Have already guaranteed that source is less than 2^32 bytes long
                line_offsets.push(pos as u32);
            }
        }
    }

    line_offsets
}

/// Return the line and column number to report for a Pos. Line and column number are intended for
/// display and are 1-indexed.
///
/// Column is the number of code points in the line up to the pos (not number of bytes).
pub fn find_line_col_for_pos(buf: &[u8], pos: Pos, line_offsets: &[u32]) -> (usize, usize) {
    // Binary search to find the largest line start offset that is smaller than the pos. This is
    // the line number.
    let line = find_largest_offset_less_than_or_equal(pos, line_offsets);

    let line_start = line_offsets[line] as usize;
    let line_slice = &buf[line_start..pos];
    let col = best_effort_count_code_points(line_slice);

    (line + 1, col + 1)
}

/// Best-effort count of code points in a buffer. Check validity, returning the number of valid code
/// points if invalid WTF-8 is encountered.
fn best_effort_count_code_points(buf: &[u8]) -> usize {
    let mut num_code_points = 0;
    let mut slice = buf;

    while !slice.is_empty() {
        let num_bytes = if is_ascii(slice[0] as CodePoint) {
            1
        } else {
            match decode_wtf8_codepoint(slice) {
                Ok((_, num_bytes)) => num_bytes,
                // Invalid WTF-8, return number of code points parsed so far
                Err(_) => return num_code_points,
            }
        };

        num_code_points += 1;
        slice = &slice[num_bytes..];
    }

    // Reached end of buffer, string is valid WTF-8
    num_code_points
}

fn find_largest_offset_less_than_or_equal(target: Pos, line_offsets: &[u32]) -> Pos {
    let mut lo = 0;
    let mut hi = line_offsets.len();

    while lo < hi {
        let mid = (lo + hi) / 2;
        if (line_offsets[mid] as usize) < target + 1 {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }

    lo - 1
}
