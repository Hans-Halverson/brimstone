use std::cell::RefCell;
use std::fs::File;
use std::io::{BufReader, Read};

use crate::{
    common::{constants::MAX_STRING_LENGTH, wtf_8::Wtf8String},
    parser::{
        loc::calculate_line_offsets, parse_error::ParseResult, LocalizedParseError, ParseError,
    },
};

pub struct Source {
    /// Path to the source file on disk.
    file_path: String,
    /// Name of the source file to use when displaying. If not set, the file path is used.
    display_name: Option<String>,
    pub contents: Wtf8String,
    line_offsets: RefCell<Option<Vec<u32>>>,
}

impl Source {
    fn new(
        file_path: String,
        display_name: Option<String>,
        contents: Wtf8String,
    ) -> ParseResult<Source> {
        // Guarantee that file path and display name are within allowed string range
        if file_path.len() > MAX_STRING_LENGTH as usize {
            return Err(LocalizedParseError::new_without_loc(ParseError::StringTooLarge(
                "File path".to_string(),
            )));
        }

        if let Some(display_name) = &display_name {
            if display_name.len() > MAX_STRING_LENGTH as usize {
                return Err(LocalizedParseError::new_without_loc(ParseError::StringTooLarge(
                    "Display name".to_string(),
                )));
            }
        }

        Ok(Source {
            file_path,
            display_name,
            contents,
            line_offsets: RefCell::new(None),
        })
    }

    pub fn new_from_file(file_path: &str) -> ParseResult<Source> {
        let wtf8_contents = Self::read_file_to_wtf8_string(file_path)?;

        // Guarantee that source size is within allowed range
        if wtf8_contents.len() > MAX_STRING_LENGTH as usize {
            return Err(LocalizedParseError::new_without_loc(ParseError::StringTooLarge(
                "File".to_string(),
            )));
        }

        Self::new(file_path.to_owned(), None, wtf8_contents)
    }

    pub fn new_for_eval(file_path: String, contents: Wtf8String) -> ParseResult<Source> {
        // Guarantee that source size is within allowed range
        if contents.len() > MAX_STRING_LENGTH as usize {
            return Err(LocalizedParseError::new_without_loc(ParseError::StringTooLarge(
                "String".to_string(),
            )));
        }

        Self::new(file_path, Some("<eval>".to_owned()), contents)
    }

    pub fn new_for_string(file_path: &str, contents: Wtf8String) -> ParseResult<Source> {
        // Guarantee that source size is within allowed range
        if contents.len() > MAX_STRING_LENGTH as usize {
            return Err(LocalizedParseError::new_without_loc(ParseError::StringTooLarge(
                "String".to_string(),
            )));
        }

        Self::new(file_path.to_owned(), None, contents)
    }

    pub fn read_file_to_wtf8_string(file_path: &str) -> ParseResult<Wtf8String> {
        // Read file to string
        let file = File::open(file_path)?;
        let mut reader = BufReader::new(file);

        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;

        Ok(Wtf8String::from_string(contents))
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

    /// Get the line of the source file at the given line number. Line number is 0-indexed.
    pub fn get_line(&self, line: u32) -> String {
        let offsets = self.line_offsets();
        let start = offsets[line as usize];
        let end = if (line as usize + 1) < offsets.len() {
            // Exclude the newline character if one exists
            offsets[line as usize + 1] as usize - 1
        } else {
            self.contents.len()
        };

        let line_slice = &self.contents.as_bytes()[start as usize..end];

        String::from_utf8_lossy(line_slice).to_string()
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
