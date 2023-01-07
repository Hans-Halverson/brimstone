use std::cell::RefCell;
use std::fs::File;
use std::io::{BufReader, Read};

use super::loc::calculate_line_offsets;
use super::parser::ParseResult;

pub struct Source {
    pub file_path: String,
    pub contents: String,
    line_offsets: RefCell<Option<Vec<usize>>>,
}

impl Source {
    pub fn new_from_file(file_path: &str) -> ParseResult<Source> {
        // Read file to string
        let file = File::open(&file_path)?;
        let mut reader = BufReader::new(file);

        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;

        Ok(Source::new_from_string(file_path, contents))
    }

    pub fn new_from_string(file_path: &str, contents: String) -> Source {
        Source {
            file_path: file_path.to_owned(),
            contents,
            line_offsets: RefCell::new(None),
        }
    }

    pub fn line_offsets(&self) -> &[usize] {
        unsafe {
            match *(self.line_offsets.as_ptr()) {
                Some(_) => (),
                None => {
                    let offsets = calculate_line_offsets(&self.contents);
                    self.line_offsets.replace(Some(offsets));
                }
            }

            (*(self.line_offsets.as_ptr())).as_mut().unwrap()
        }
    }
}
