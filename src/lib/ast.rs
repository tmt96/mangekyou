use crate::token::*;
// AST definition

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

impl Prototype {
    pub fn new(name: String, args: Vec<String>) -> Self {
        Self { name, args }
    }

    pub fn get_name(&self) -> String {
        self.name.to_string()
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub proto: Prototype,
    pub body: Expr,
}

impl Function {
    pub fn new(proto: Prototype, body: Expr) -> Self {
        Self { proto, body }
    }
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    FunctionNode(Function),
    PrototypeNode(Prototype),
    EmptyNode,
}
