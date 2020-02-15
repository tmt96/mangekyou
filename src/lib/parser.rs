use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn from_source(source: &'a str) -> Self {
        Self::from_lexer(Lexer::new(source))
    }

    pub fn from_lexer(mut lexer: Lexer<'a>) -> Self {
        let cur_token = lexer.next();
        Self { lexer, cur_token }
    }

    fn get_next_token(&mut self) -> Option<Token> {
        self.cur_token = self.lexer.next();
        self.cur_token.clone()
    }

    fn log_error(&self, message: &str) -> Option<Expr> {
        dbg!("LogError: {}", message);
        None
    }

    fn log_prototype_error(&self, message: &str) -> Option<Prototype> {
        dbg!("LogError: {}", message);
        None
    }
    fn parse_number(&mut self, number: f64) -> Option<Expr> {
        self.get_next_token();
        Some(Expr::Number(number))
    }

    fn parse_paren(&mut self) -> Option<Expr> {
        self.get_next_token();
        let expr = self.parse_expression()?;
        if let Some(Token::CloseParen) = self.cur_token {
            self.get_next_token();
            Some(expr)
        } else {
            self.log_error("Expected ')'")
        }
    }

    fn parse_identifier(&mut self, identifier: String) -> Option<Expr> {
        if let Some(Token::OpenParen) = self.get_next_token() {
            self.get_next_token(); // eat '('
            let mut args = vec![];

            loop {
                args.push(self.parse_expression()?);
                match self.cur_token {
                    Some(Token::CloseParen) => break,
                    Some(Token::Comma) => {
                        self.get_next_token();
                    }
                    _ => return self.log_error("Expected ')' or ',' in argument list"),
                }
            }

            self.lexer.next(); // eat ')'
            Some(Expr::Call {
                callee: identifier,
                args,
            })
        } else {
            Some(Expr::Variable(identifier))
        }
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        match self.cur_token.clone() {
            Some(Token::Number(num)) => self.parse_number(num),
            Some(Token::Identifier(ident)) => self.parse_identifier(ident),
            Some(Token::OpenParen) => self.parse_paren(),
            _ => self.log_error("Unknown token when expecting an expression"),
        }
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        let lhs = self.parse_primary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    fn get_token_precedent(token: BinaryOp) -> i32 {
        match token {
            BinaryOp::Lt | BinaryOp::Gt => 10,
            BinaryOp::Add | BinaryOp::Sub => 20,
            BinaryOp::Mul | BinaryOp::Div => 40,
        }
    }

    fn parse_bin_op_rhs(&mut self, expr_precedent: i32, mut lhs: Expr) -> Option<Expr> {
        loop {
            let op = if let Some(Token::BinaryOp(op)) = self.cur_token {
                op
            } else {
                return Some(lhs);
            };

            let token_precedent = Self::get_token_precedent(op);
            if token_precedent < expr_precedent {
                return Some(lhs);
            }
            self.get_next_token();

            let mut rhs = self.parse_primary()?;
            if let Some(Token::BinaryOp(next_op)) = self.cur_token {
                let next_precedent = Self::get_token_precedent(next_op);
                if token_precedent < next_precedent {
                    rhs = self.parse_bin_op_rhs(token_precedent + 1, rhs)?;
                }
            }

            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
    }

    fn parse_prototype(&mut self) -> Option<Prototype> {
        let fn_name = if let Some(Token::Identifier(identifier)) = &mut self.cur_token {
            (*identifier).to_string()
        } else {
            return self.log_prototype_error("Expected function name in prototype");
        };

        self.get_next_token();
        if let Some(Token::OpenParen) = self.cur_token {
            let mut arg_names = Vec::new();
            while let Some(Token::Identifier(arg)) = self.get_next_token() {
                arg_names.push(arg);
            }

            if let Some(Token::CloseParen) = self.cur_token {
                self.get_next_token();
                Some(Prototype::new(fn_name, arg_names))
            } else {
                self.log_prototype_error("Expected ')' in prototype")
            }
        } else {
            self.log_prototype_error("Expected '(' in prototype")
        }
    }

    fn parse_definition(&mut self) -> Option<Function> {
        self.get_next_token();
        let proto = self.parse_prototype()?;
        let expr = self.parse_expression()?;
        Some(Function::new(proto, expr))
    }

    fn parse_extern(&mut self) -> Option<Prototype> {
        self.get_next_token();
        self.parse_prototype()
    }

    fn parse_top_level_expr(&mut self) -> Option<Function> {
        let expr = self.parse_expression()?;
        let proto = Prototype::new("".to_string(), vec![]);
        Some(Function::new(proto, expr))
    }

    pub fn parse(&mut self) -> Option<AstNode> {
        match self.cur_token {
            Some(Token::Semicolon) => {
                self.get_next_token();
                self.parse()
            }
            Some(Token::Def) => self.parse_definition().map(AstNode::FunctionNode),
            Some(Token::Extern) => self.parse_extern().map(AstNode::PrototypeNode),
            None => None,
            _ => self.parse_top_level_expr().map(AstNode::FunctionNode),
        }
    }

    pub fn parse_loop(&mut self) -> Vec<AstNode> {
        let mut ast_tree = vec![];
        while let Some(node) = self.parse() {
            ast_tree.push(node);
        }
        ast_tree
    }
}
