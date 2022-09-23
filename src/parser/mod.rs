use std::collections::{VecDeque, HashSet};

use crate::token::Token;

#[derive(Eq, Hash)]
pub struct Function<'a> {
    /// parameter names
    pub params: Option<Vec<&'a str>>,
    /// raw tokens
    pub raw: Option<VecDeque<Token<'a>>>
}

impl<'a> Function<'a> {
    pub fn new() -> Self {
        Self {
            params: None,
            raw: None
        }
    }
}

impl<'a> PartialEq for Function<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params
    }
}

/// simple brace-counting parser to detect abstract token syntaxes
fn discover_functions<'a>(tokens: &mut VecDeque<crate::Token<'a>>) -> HashSet<Function<'a>> {
    let mut funcs = HashSet::new();

    let mut name = None;
    let mut cur_fun = Function::new();

    let mut assigned = false;
    let mut brace_cnt = 0;
    let mut parent_cnt = 0;

    while let Some(top) = tokens.pop_front() {
        
        match &top {
            crate::Token::Operator(op) => {
                match op {
                    crate::Operator::Assign => if cur_fun.raw.is_none() {
                        assigned = true;
                        cur_fun.raw = Some(VecDeque::new());
                        continue;
                    },
                    _ => ()
                }
            }
            crate::Token::LineBreak => if name.is_some() && cur_fun.raw.is_some() && assigned {
                funcs.insert(cur_fun);
                cur_fun = Function::new();
                continue;
            }
            crate::Token::Delemiter(char) => {
                match char {

                    '{' => {
                        brace_cnt += 1;
                        if brace_cnt == 1 {
                            // start a new body
                            cur_fun.raw = Some(VecDeque::new());
                            assigned = false;
                            continue;
                        }
                    },
                    '}' => {
                        brace_cnt -= 1;
                        
                        // we have a full body!
                        if brace_cnt == 0 {
                            funcs.insert(cur_fun);
                            cur_fun = Function::new();
                            continue;
                        }
                    },

                    '(' => if cur_fun.raw.is_none() {
                        parent_cnt += 1;
                        if parent_cnt == 1 {
                            // start a new arg list
                            cur_fun.params = Some(Vec::new());
                            continue;
                        }
                    },
                    ')' => if cur_body.is_none() {
                        parent_cnt -= 1;
                        
                        // we have a full body!
                        if parent_cnt == 0 {
                            funcs.insert(cur_fun);
                            cur_fun = Function::new();
                            continue;
                        }
                    },
                    _ => ()
                }
            }
            _ => (),
        }

        if let Some(body) = &mut cur_body {
            body.push_back(top);
        } else if let Some(args) = &mut cur_args {
            match &top {
                Token::Word(text) => args.push(text),
                _ => panic!("Argument in list is not a word")
            }
        } else {
            body.push_back(top)
        }
    }
    
    funcs
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
pub fn parse<'a>(tokens: &mut VecDeque<crate::Token<'a>>)  {
    
}