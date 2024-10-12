use std::cell::RefCell;
use std::fs::File;
use std::io::{BufReader, Read};

use crate::js::common::wtf_8::Wtf8String;

use super::loc::calculate_line_offsets;
use super::parse_error::ParseResult;
use super::{LocalizedParseError, ParseError};

pub struct Source {
    /// Path to the source file on disk.
    file_path: String,
    /// Name of the source file to use when displaying. If not set, the file path is used.
    display_name: Option<String>,
    pub contents: Wtf8String,
    line_offsets: RefCell<Option<Vec<u32>>>,
}

impl Source {
    fn new(file_path: String, display_name: Option<String>, contents: Wtf8String) -> Source {
        Source {
            file_path,
            display_name,
            contents,
            line_offsets: RefCell::new(None),
        }
    }

    pub fn new_from_file(file_path: &str) -> ParseResult<Source> {
        // Read file to string
        let file = File::open(file_path)?;
        let mut reader = BufReader::new(file);

        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;

        let wtf8_contents = Wtf8String::from_string(contents);

        // Guarantee that source size is within allowed range
        if is_source_too_large(wtf8_contents.len()) {
            return Err(LocalizedParseError::new_without_loc(ParseError::SourceTooLarge(true)));
        }

        Ok(Source::new(file_path.to_owned(), None, wtf8_contents))
    }

    pub fn new_for_eval(file_path: String, contents: Wtf8String) -> ParseResult<Source> {
        // Guarantee that source size is within allowed range
        if is_source_too_large(contents.len()) {
            return Err(LocalizedParseError::new_without_loc(ParseError::SourceTooLarge(false)));
        }

        Ok(Self::new(file_path, Some("<eval>".to_owned()), contents))
    }

    pub fn line_offsets(&self) -> &[u32] {
        unsafe {
            match *(self.line_offsets.as_ptr()) {
                Some(_) => (),
                None => {
                    let offsets = calculate_line_offsets(self.contents.as_bytes());
                    self.line_offsets.replace(Some(offsets));
                }
            }

            (*(self.line_offsets.as_ptr())).as_mut().unwrap()
        }
    }

    pub fn file_path(&self) -> &str {
        &self.file_path
    }

    pub fn display_name(&self) -> &str {
        match &self.display_name {
            Some(name) => name,
            None => &self.file_path,
        }
    }

    pub fn has_display_name(&self) -> bool {
        self.display_name.is_some()
    }
}

/// Source files are limited to 2^32 bytes (4GB) in size so that positions can be represented with
/// a u32.
fn is_source_too_large(size: usize) -> bool {
    size >= u32::MAX as usize
}
