use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Option<Token>,
}

type ParseResult<T> = Result<T, String>;

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

    fn log_error(&self, message: &str) -> ParseResult<Expr> {
        Err(format!("Parse Error: {}", message))
    }

    fn log_prototype_error(&self, message: &str) -> ParseResult<Prototype> {
        Err(format!("Parse Error: {}", message))
    }
    fn parse_number(&mut self, number: f64) -> ParseResult<Expr> {
        self.get_next_token();
        Ok(Expr::Number(number))
    }

    fn parse_paren(&mut self) -> ParseResult<Expr> {
        self.get_next_token();
        let expr = self.parse_expression()?;
        if let Some(Token::CloseParen) = self.cur_token {
            self.get_next_token();
            Ok(expr)
        } else {
            self.log_error("Expected ')'")
        }
    }

    fn parse_identifier(&mut self, identifier: String) -> ParseResult<Expr> {
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
            Ok(Expr::Call {
                callee: identifier,
                args,
            })
        } else {
            Ok(Expr::Variable(identifier))
        }
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.cur_token.clone() {
            Some(Token::Number(num)) => self.parse_number(num),
            Some(Token::Identifier(ident)) => self.parse_identifier(ident),
            Some(Token::OpenParen) => self.parse_paren(),
            _ => self.log_error("Unknown token when expecting an expression"),
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
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

    fn parse_bin_op_rhs(&mut self, expr_precedent: i32, mut lhs: Expr) -> ParseResult<Expr> {
        loop {
            let op = if let Some(Token::BinaryOp(op)) = self.cur_token {
                op
            } else {
                return Ok(lhs);
            };

            let token_precedent = Self::get_token_precedent(op);
            if token_precedent < expr_precedent {
                return Ok(lhs);
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

    fn parse_prototype(&mut self) -> ParseResult<Prototype> {
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
                Ok(Prototype::new(fn_name, arg_names))
            } else {
                self.log_prototype_error("Expected ')' in prototype")
            }
        } else {
            self.log_prototype_error("Expected '(' in prototype")
        }
    }

    fn parse_definition(&mut self) -> ParseResult<Function> {
        self.get_next_token();
        let proto = self.parse_prototype()?;
        let expr = self.parse_expression()?;
        Ok(Function::new(proto, expr))
    }

    fn parse_extern(&mut self) -> ParseResult<Prototype> {
        self.get_next_token();
        self.parse_prototype()
    }

    fn parse_top_level_expr(&mut self) -> ParseResult<Function> {
        let expr = self.parse_expression()?;
        let proto = Prototype::new("".to_string(), vec![]);
        Ok(Function::new(proto, expr))
    }

    pub fn parse(&mut self) -> ParseResult<AstNode> {
        match self.cur_token {
            Some(Token::Semicolon) => {
                self.get_next_token();
                self.parse()
            }
            Some(Token::Def) => self.parse_definition().map(AstNode::FunctionNode),
            Some(Token::Extern) => self.parse_extern().map(AstNode::PrototypeNode),
            None => Ok(AstNode::EmptyNode),
            _ => self.parse_top_level_expr().map(AstNode::FunctionNode),
        }
    }

    pub fn parse_loop(&mut self) -> ParseResult<Vec<AstNode>> {
        let mut ast_tree = vec![];
        loop {
            let node = self.parse()?;
            if node == AstNode::EmptyNode {
                return Ok(ast_tree);
            }
            ast_tree.push(node);
        }
    }
}
