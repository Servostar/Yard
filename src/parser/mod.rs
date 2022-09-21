
pub struct Function<'a> {
    /// name
    name: &'a str,
    /// parameter names
    params: Vec<&'a str>,  
    /// wether this function returns a single value or not
    ret: bool,
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
/// ```python
/// foo(a) = {
///  # function
/// }
/// ```
pub fn parse<'a>(tokens: &Vec<crate::Token<'a>>) -> Vec<Function<'a>> {
    let mut functions = vec![];

    

    functions
}