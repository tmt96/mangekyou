// Token Definition for Mangekyou

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Def,
    Extern,
    OpenParen,
    CloseParen,
    Comma,
    Semicolon,
    If,
    Then,
    Else,
    For,
    In,
    Assign,
    BinaryDef,
    UnaryDef,
    Var,
    BinaryOp(BinaryOp),
    Identifier(String),
    Number(f64),
    UnknownChar(char),
}

#[derive(Debug, PartialEq, Copy, Clone, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
}
