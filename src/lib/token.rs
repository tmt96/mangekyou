// Token Definition for Mangekyou

#[derive(Debug, PartialEq)]
pub enum Token {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    Kwd(char),
}
