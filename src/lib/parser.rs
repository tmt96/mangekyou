use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::*;
use std::collections::HashMap;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Option<Token>,
    op_precedence_map: &'a HashMap<AstBinaryOp, i32>,
}

type ParseResult<T> = Result<T, String>;

impl<'a> Parser<'a> {
    pub fn from_source(source: &'a str, op_precedence_map: &'a HashMap<AstBinaryOp, i32>) -> Self {
        Self::from_lexer(Lexer::new(source), op_precedence_map)
    }

    pub fn from_lexer(
        mut lexer: Lexer<'a>,
        op_precedence_map: &'a HashMap<AstBinaryOp, i32>,
    ) -> Self {
        let cur_token = lexer.next();
        Self {
            lexer,
            cur_token,
            op_precedence_map,
        }
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

    fn parse_for(&mut self) -> ParseResult<Expr> {
        let var_name = match self.get_next_token() {
            Some(Token::Identifier(name)) => name,
            _ => return self.format_error("Expected identifier after for"),
        };

        match self.get_next_token() {
            Some(Token::Assign) => {
                self.get_next_token();
            }
            _ => return self.format_error("Exprected assignment"),
        }

        let start = self.parse_expression()?;
        match self.cur_token {
            Some(Token::Comma) => {
                self.get_next_token();
            }
            _ => return self.format_error("Expected ',' after for start value"),
        }

        let end = self.parse_expression()?;

        let step = match self.cur_token {
            Some(Token::Comma) => {
                self.get_next_token();
                Some(self.parse_expression()?)
            }
            _ => None,
        };

        if let Some(Token::In) = self.cur_token {
            self.get_next_token();
            let body = self.parse_expression()?;
            Ok(Expr::For(ForExpr {
                var_name,
                start: Box::new(start),
                end: Box::new(end),
                step: step.map(Box::new),
                body: Box::new(body),
            }))
        } else {
            Err("Exprected 'in' after for expr".to_string())
        }
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.cur_token.clone() {
            Some(Token::Number(num)) => self.parse_number(num),
            Some(Token::Identifier(ident)) => self.parse_identifier(ident),
            Some(Token::OpenParen) => self.parse_paren(),
            Some(Token::If) => self.parse_if_else(),
            Some(Token::For) => self.parse_for(),
            None => self.format_error("No token left"),
            Some(token) => self.format_error(&format!(
                "Unknown token {:#?} when expecting an expression",
                token
            )),
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_primary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    fn get_token_precedent(&self, token: AstBinaryOp) -> i32 {
        *self.op_precedence_map.get(&token).unwrap_or(&100)
    }

    fn parse_bin_op_rhs(&mut self, expr_precedent: i32, mut lhs: Expr) -> ParseResult<Expr> {
        loop {
            let op = match self.cur_token {
                Some(Token::BinaryOp(op)) => op.into(),
                Some(Token::UnknownChar(ch)) => AstBinaryOp::Custom(ch),
                _ => {
                    return Ok(lhs);
                }
            };

            let token_precedent = self.get_token_precedent(op);
            if token_precedent < expr_precedent {
                return Ok(lhs);
            }
            self.get_next_token();

            let mut rhs = self.parse_primary()?;
            if let Some(Token::BinaryOp(next_op)) = self.cur_token {
                let next_precedent = self.get_token_precedent(next_op.into());
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
        let cur_token = &self.cur_token;
        let (prototype_type, fn_name, precedence) = match cur_token {
            Some(Token::Identifier(identifier)) => {
                (PrototypeType::Normal, identifier.to_string(), None)
            }
            Some(Token::BinaryDef) => match self.get_next_token() {
                Some(Token::UnknownChar(ch)) => {
                    if !ch.is_ascii() {
                        return self.format_error("Expected binary operator");
                    }
                    let mut identifier = "binary".to_string();
                    identifier.push(ch);

                    let precedence = match self.get_next_token() {
                        Some(Token::Number(n)) => {
                            if n < 1.0 || n > 100.0 {
                                return self.format_error("Invalid precedence: must be 1..100");
                            }
                            self.get_next_token();
                            Some(n.round() as i32)
                        }
                        _ => None,
                    };
                    (PrototypeType::Binary, identifier, precedence)
                }
                _ => {
                    return self.format_error("Expected binary operator");
                }
            },
            _ => return self.format_error("Expected function name in prototype"),
        };

        if let Some(Token::OpenParen) = self.get_next_token() {
            let mut arg_names = Vec::new();
            while let Some(Token::Identifier(arg)) = self.get_next_token() {
                arg_names.push(arg);
            }
            if prototype_type == PrototypeType::Binary && arg_names.len() != 2 {
                return self.format_error("Binary operator requries exactly 2 operands");
            }

            if let Some(Token::CloseParen) = self.cur_token {
                self.get_next_token();
                Ok(Prototype::new(fn_name, arg_names)
                    .prototype_type(prototype_type)
                    .precedence(precedence))
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
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init_op_pred_map() -> HashMap<AstBinaryOp, i32> {
        [
            (AstBinaryOp::Lt, 10),
            (AstBinaryOp::Gt, 10),
            (AstBinaryOp::Eq, 10),
            (AstBinaryOp::Add, 20),
            (AstBinaryOp::Sub, 20),
            (AstBinaryOp::Mul, 40),
            (AstBinaryOp::Div, 40),
        ]
        .iter()
        .cloned()
        .collect()
    }

    #[test]
    fn test_number_parsing() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("1", &op_pred_map);
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1.0));
        let mut parser = Parser::from_source("1234567890", &op_pred_map);
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1_234_567_890.0));
        let mut parser = Parser::from_source("1.2345", &op_pred_map);
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1.2345));
        let mut parser = Parser::from_source("1.", &op_pred_map);
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(1.0));
        let mut parser = Parser::from_source(".1", &op_pred_map);
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Expr::Number(0.1));
    }

    #[test]
    fn test_basic_expression_parsing() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("1 + 1", &op_pred_map);
        let ast = parser.parse_expression().unwrap();
        assert_eq!(
            ast,
            Expr::Binary(BinaryExpr {
                op: AstBinaryOp::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Number(1.0)),
            })
        )
    }

    #[test]
    fn test_complicated_expression_parsing() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("1 + 2 * 3 - 2", &op_pred_map);
        let got = parser.parse_expression().unwrap();
        let inner_rhs = Expr::Binary(BinaryExpr {
            op: AstBinaryOp::Mul,
            lhs: Box::new(Expr::Number(2.0)),
            rhs: Box::new(Expr::Number(3.0)),
        });
        let lhs = Expr::Binary(BinaryExpr {
            op: AstBinaryOp::Add,
            lhs: Box::new(Expr::Number(1.0)),
            rhs: Box::new(inner_rhs),
        });
        let expected = Expr::Binary(BinaryExpr {
            op: AstBinaryOp::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(Expr::Number(2.0)),
        });
        assert_eq!(got, expected)
    }

    #[test]
    fn test_prototype_parsing() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("foo()", &op_pred_map);
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("foo"), vec![]);
        assert_eq!(got, expected);
        let mut parser = Parser::from_source("bar(a)", &op_pred_map);
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("bar"), vec![String::from("a")]);
        assert_eq!(got, expected);
        let mut parser = Parser::from_source("bar(a b c)", &op_pred_map);
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(
            String::from("bar"),
            vec![String::from("a"), String::from("b"), String::from("c")],
        );
        assert_eq!(got, expected);
    }

    #[test]
    fn test_function_definition_parsing() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("def foo() 1 + 1", &op_pred_map);
        let got = parser.parse_definition().unwrap();
        let expected = Function::new(
            Prototype::new(String::from("foo"), vec![]),
            Expr::Binary(BinaryExpr {
                op: AstBinaryOp::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Number(1.0)),
            }),
        );
        assert_eq!(got, expected);
    }

    #[test]
    fn test_extern_parsing() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("extern sin(a)", &op_pred_map);
        let got = parser.parse_extern().unwrap();
        let expected = Prototype::new(String::from("sin"), vec![String::from("a")]);
        assert_eq!(got, expected);
    }

    #[test]
    fn test_if_expr() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("if x < 3 then 1 else 2", &op_pred_map);
        let got = parser.parse_expression().unwrap();
        let expected = Expr::If(IfExpr {
            cond: Box::new(Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Variable("x".to_string())),
                op: AstBinaryOp::Lt,
                rhs: Box::new(Expr::Number(3.0)),
            })),
            then_branch: Box::new(Expr::Number(1.0)),
            else_branch: Box::new(Expr::Number(2.0)),
        });
        assert_eq!(got, expected);
    }

    #[test]
    fn test_for_expr() {
        let op_pred_map = init_op_pred_map();
        let mut parser = Parser::from_source("for i = 1, i < 3, 1 in putchard(42)", &op_pred_map);
        let got = parser.parse_expression().unwrap();
        let expected = Expr::For(ForExpr {
            var_name: "i".to_string(),
            start: Box::new(Expr::Number(1.0)),
            end: Box::new(Expr::Binary(BinaryExpr {
                lhs: Box::new(Expr::Variable("x".to_string())),
                op: AstBinaryOp::Lt,
                rhs: Box::new(Expr::Number(3.0)),
            })),
            step: Some(Box::new(Expr::Number(1.0))),
            body: Box::new(Expr::Call(CallExpr {
                callee: "putchard".to_string(),
                args: vec![Expr::Number(42.0)],
            })),
        });
        assert_eq!(got, expected);
    }
}
