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

    fn format_error<T>(&self, message: &str) -> ParseResult<T> {
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
            self.format_error("Expected ')'")
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
                    _ => return self.format_error("Expected ')' or ',' in argument list"),
                }
            }

            self.get_next_token(); // eat ')'
            Ok(Expr::Call(CallExpr {
                callee: identifier,
                args,
            }))
        } else {
            Ok(Expr::Variable(identifier))
        }
    }

    fn parse_if_else(&mut self) -> ParseResult<Expr> {
        self.get_next_token(); // eat "if"
        let cond = self.parse_expression()?;
        if let Some(Token::Then) = self.cur_token {
            self.get_next_token();
            let then_branch = self.parse_expression()?;
            if let Some(Token::Else) = self.cur_token {
                self.get_next_token();
                let else_branch = self.parse_expression()?;
                Ok(Expr::If(IfExpr {
                    cond: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                }))
            } else {
                Err("Expected else".to_string())
            }
        } else {
            Err("Expected then".to_string())
        }
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.cur_token.clone() {
            Some(Token::Number(num)) => self.parse_number(num),
            Some(Token::Identifier(ident)) => self.parse_identifier(ident),
            Some(Token::OpenParen) => self.parse_paren(),
            Some(Token::If) => self.parse_if_else(),
            _ => self.format_error("Unknown token when expecting an expression"),
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

            lhs = Expr::Binary(BinaryExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
    }

    fn parse_prototype(&mut self) -> ParseResult<Prototype> {
        let fn_name = if let Some(Token::Identifier(identifier)) = &mut self.cur_token {
            (*identifier).to_string()
        } else {
            return self.format_error("Expected function name in prototype");
        };

        if let Some(Token::OpenParen) = self.get_next_token() {
            let mut arg_names = Vec::new();
            while let Some(Token::Identifier(arg)) = self.get_next_token() {
                arg_names.push(arg);
            }

            if let Some(Token::CloseParen) = self.cur_token {
                self.get_next_token();
                Ok(Prototype::new(fn_name, arg_names))
            } else {
                self.format_error("Expected ')' in prototype")
            }
        } else {
            self.format_error("Expected '(' in prototype")
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_number_parsing() {
        let mut parser = Parser::from_source("1");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1.0));
        let mut parser = Parser::from_source("1234567890");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1_234_567_890.0));
        let mut parser = Parser::from_source("1.2345");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1.2345));
        let mut parser = Parser::from_source("1.");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1.0));
        let mut parser = Parser::from_source(".1");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(0.1));
    }

    #[test]
    fn test_basic_expression_parsing() {
        let mut parser = Parser::from_source("1 + 1");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(
            ast,
            Expr::Binary(BinaryExpr {
                op: BinaryOp::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Number(1.0)),
            })
        )
    }

    #[test]
    fn test_complicated_expression_parsing() {
        let mut parser = Parser::from_source("1 + 2 * 3 - 2");
        let got = parser.parse_expression().unwrap();
        let inner_rhs = Expr::Binary(BinaryExpr {
            op: BinaryOp::Mul,
            lhs: Box::new(Expr::Number(2.0)),
            rhs: Box::new(Expr::Number(3.0)),
        });
        let lhs = Expr::Binary(BinaryExpr {
            op: BinaryOp::Add,
            lhs: Box::new(Expr::Number(1.0)),
            rhs: Box::new(inner_rhs),
        });
        let expected = Expr::Binary(BinaryExpr {
            op: BinaryOp::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(Expr::Number(2.0)),
        });
        assert_eq!(got, expected)
    }

    #[test]
    fn test_prototype_parsing() {
        let mut parser = Parser::from_source("foo()");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("foo"), vec![]);
        assert_eq!(got, expected);
        let mut parser = Parser::from_source("bar(a)");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("bar"), vec![String::from("a")]);
        assert_eq!(got, expected);
        let mut parser = Parser::from_source("bar(a b c)");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(
            String::from("bar"),
            vec![String::from("a"), String::from("b"), String::from("c")],
        );
        assert_eq!(got, expected);
    }

    #[test]
    fn test_function_definition_parsing() {
        let mut parser = Parser::from_source("def foo() 1 + 1");
        let got = parser.parse_definition().unwrap();
        let expected = Function::new(
            Prototype::new(String::from("foo"), vec![]),
            Expr::Binary(BinaryExpr {
                op: BinaryOp::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Number(1.0)),
            }),
        );
        assert_eq!(got, expected);
    }

    #[test]
    fn test_extern_parsing() {
        let mut parser = Parser::from_source("extern sin(a)");
        let got = parser.parse_extern().unwrap();
        let expected = Prototype::new(String::from("sin"), vec![String::from("a")]);
        assert_eq!(got, expected);
    }
}
