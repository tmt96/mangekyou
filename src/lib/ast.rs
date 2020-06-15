use crate::token::*;
// AST definition

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
pub enum AstBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
    Assign,
    Custom(char),
}

impl From<BinaryOp> for AstBinaryOp {
    fn from(op: BinaryOp) -> Self {
        match op {
            BinaryOp::Add => AstBinaryOp::Add,
            BinaryOp::Sub => AstBinaryOp::Sub,
            BinaryOp::Mul => AstBinaryOp::Mul,
            BinaryOp::Div => AstBinaryOp::Div,
            BinaryOp::Lt => AstBinaryOp::Lt,
            BinaryOp::Gt => AstBinaryOp::Gt,
            BinaryOp::Eq => AstBinaryOp::Eq,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub op: AstBinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: char,
    pub operand: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct ForExpr {
    pub var_name: String,
    pub start: Box<Expr>,
    pub end: Box<Expr>,
    pub step: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct VarDefExpr {
    pub var_names: Vec<(String, Box<Expr>)>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
    If(IfExpr),
    For(ForExpr),
    VarDef(VarDefExpr),
}

#[derive(Debug, PartialEq)]
pub enum PrototypeType {
    Normal,
    Unary,
    Binary,
}

#[derive(Debug, PartialEq)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub prototype_type: PrototypeType,
    pub precedence: Option<i32>,
}

impl Prototype {
    pub fn new(name: String, args: Vec<String>) -> Self {
        Self {
            name,
            args,
            prototype_type: PrototypeType::Normal,
            precedence: None,
        }
    }

    pub fn prototype_type(self, prototype_type: PrototypeType) -> Self {
        Self {
            prototype_type,
            ..self
        }
    }

    pub fn precedence(self, precedence: Option<i32>) -> Self {
        Self { precedence, ..self }
    }

    pub fn get_name(&self) -> String {
        self.name.to_string()
    }

    fn is_op(&self) -> bool {
        self.is_binary_op() || self.is_unary_op()
    }

    fn is_unary_op(&self) -> bool {
        self.prototype_type == PrototypeType::Unary
    }

    fn is_binary_op(&self) -> bool {
        self.prototype_type == PrototypeType::Binary
    }

    pub fn get_op_name(&self) -> Result<char, &str> {
        if self.is_op() {
            self.name.chars().last().ok_or("Operator name is empty")
        } else {
            Err("Not an operator")
        }
    }

    pub fn get_binary_pred(&self) -> Option<i32> {
        self.precedence
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
