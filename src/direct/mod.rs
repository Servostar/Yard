use std::{collections::VecDeque};

use crate::{token::{Token}, builtin::modules::Module};

#[derive(Default)]
pub struct LangSpecs {
    /// builtin modules for extra builtin functions
    builtin_features: Vec<crate::builtin::modules::Module>,
    lang_version: u32,
    authors: Vec<String>,
    embedded_files: Vec<String>,
}

impl LangSpecs {
    
    pub fn features(&self) -> &[crate::builtin::modules::Module] {
        &self.builtin_features
    }
}

pub fn resolve_directives(tokens: &mut VecDeque<Token>) -> LangSpecs {
    let mut specs = LangSpecs::default();

    for token in tokens.iter() {
        match token {
            Token::CompilerDirective(text, _) => parse_directive(text, &mut specs),
            _ => ()
        }
    }

    // remove compiler directives from source
    tokens.retain(|token| match token {
        Token::CompilerDirective(_, _) => false,
        _ => true
    });

    specs
}

static DIRECTIVE_REGEX_SRC: &'static str = concat!(
    r"@feature\(((?:\s*[\w]+\s*,?)*)\)",
    r"|@version\(\s*([0-9]{3})\s*\)",
    r"|@author\((.*)\)",
    r"|@embed\((.*)\)"
);

lazy_static::lazy_static! {
    static ref DIRECTIVE_REGEX: regex::Regex = regex::Regex::new(DIRECTIVE_REGEX_SRC).unwrap();
}

pub fn from_list(text: &str) -> Vec<String> {
    let mut vec = vec![];

    for word in text.split(',') {
        vec.push(String::from(word.trim()))
    }

    vec
}

fn parse_directive(text: &str, specs: &mut LangSpecs) {
    
    for cap in DIRECTIVE_REGEX.captures_iter(text) {
        let mut enumerator = cap.iter().enumerate();
        loop {
            let next = enumerator.next();
            if next.is_none() {
                break;
            }

            let (i, group) = next.unwrap();

            // ignore first group as its the entire match,
            // as well as the 1st group (= comments)
            if i < 1 {
                continue;
            }

            if let Some(mat) = group {
                match i {
                    1 => {
                        specs.builtin_features.append(&mut Module::from_list(mat.as_str()));
                        return;
                    },
                    2 => {
                        specs.lang_version = mat.as_str().parse().unwrap();
                        return;
                    },
                    3 => {
                        specs.authors.push(String::from(mat.as_str()));
                        return;
                    },
                    4 => {
                        specs.embedded_files.append(&mut from_list(mat.as_str()));
                        crate::message(crate::token::MessageType::Warning, "Embed directive not working at current state");
                        return;
                    },
                    _ => crate::message(crate::token::MessageType::Warning, format!("unknown directive: `{}`", text)),
                }
                break;
            }
        }
    }
    
    crate::message(crate::token::MessageType::Warning, format!("unknown directive: `{}`", text));
}