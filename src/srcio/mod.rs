use std::{fs};

pub struct CodeSrc {
    _path: String,
    src: String,
}

impl CodeSrc {
    
    pub fn new(path: &String) -> Result<CodeSrc, String> {
        Ok(Self {
            _path: path.to_owned(),
            src: Self::read_code(path)?,
        })
    }

    fn read_code(path: &String) -> Result<String, String> {
        let result = fs::read_to_string(path);
        
        // read the source
        if let Ok(src) = result {
            return Ok(src);
        }

        Err(format!("unable to fetch source code from {}: {}", path, result.err().unwrap()))
    }

    pub fn code(&self) -> &String {
        &self.src
    }
}