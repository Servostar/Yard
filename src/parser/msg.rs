use crate::token::DebugMsg;
use crate::token::MessageType::Error;
use crate::token::MessageType::Info;

pub const ERR10: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 10,
    msg: "Function defined multiple times",
};
pub const ERR11: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 11,
    msg: "Function missing return type",
};
pub const ERR12: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 12,
    msg: "Function missing name",
};
pub const ERR13: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 13,
    msg: "Open parameter list",
};
pub const ERR14: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 14,
    msg: "Closed non existant function body",
};
pub const ERR15: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 15,
    msg: "Open function body",
};
pub const ERR16: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 16,
    msg: "Missing assign for return type",
};
pub const ERR17: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 17,
    msg: "Double function assign",
};
pub const ERR18: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 18,
    msg: "Function has multiple names",
};
pub const ERR19: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 19,
    msg: "Multiple parameter lists",
};
pub const ERR20: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 20,
    msg: "not a valid parameter",
};
pub const ERR21: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 21,
    msg: "type declaration missing for parameter",
};
pub const ERR22: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 22,
    msg: "token outside of function definition",
};
pub const ERR23: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 23,
    msg: "token must be declaration",
};
pub const ERR24: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 24,
    msg: "function already has return type",
};


pub const ERR40: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 40,
    msg: "Open block",
};
pub const ERR41: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 41,
    msg: "Closed non existant block",
};

pub const INF51: &DebugMsg = &DebugMsg {
    typ: Info,
    code: 51,
    msg: "No variable type provided",
};

pub const ERR50: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 50,
    msg: "Expression has multiple values",
};
pub const ERR51: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 51,
    msg: "Missmatched variable types",
};
pub const ERR52: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 52,
    msg: "Unable to assign variable type",
};
pub const ERR53: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 53,
    msg: "Expected single boolean value",
};
pub const ERR54: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 54,
    msg: "Cannot return from function without result",
};
pub const ERR55: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 55,
    msg: "Expected single value",
};
pub const ERR56: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 56,
    msg: "Function missing return value",
};
pub const ERR57: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 57,
    msg: "Function does not return anything",
};
pub const ERR58: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 58,
    msg: "Yield must return a value",
};
pub const ERR59: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 59,
    msg: "Missmatched function return type",
};

pub const ERR60: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 60,
    msg: "Missmatched parameter count",
};
pub const ERR61: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 61,
    msg: "Missmatched parameter type",
};
pub const ERR62: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 62,
    msg: "Unknown symbol",
};
pub const ERR63: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 63,
    msg: "Missing left parenthesis",
};
pub const ERR64: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 64,
    msg: "Missing right parenthesis",
};
pub const ERR65: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 65,
    msg: "Missplaced character",
};

pub const ERR70: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 70,
    msg: "Unknown type declaration",
};
pub const ERR71: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 71,
    msg: "Unable to identify characters as token",
};

pub const ERR73: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 73,
    msg: "Missmatched types for operation",
};
pub const ERR74: &DebugMsg = &DebugMsg {
    typ: Error,
    code: 74,
    msg: "Missing operands",
};
pub const ERR75: &DebugMsg = &DebugMsg {
    typ: crate::token::MessageType::Critical,
    code: 75,
    msg: "Unknown operation",
};
