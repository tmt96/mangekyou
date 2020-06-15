use crate::token::*;
use std::iter::Iterator;
use std::str::Chars;

// Lexer/tokenizer implementation
pub struct Lexer<'a> {
    stream: Chars<'a>,
    cur_char: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut stream = input.chars();
        let cur_char = stream.next();
        Self { stream, cur_char }
    }

    fn get_next_char(&mut self) -> Option<char> {
        self.cur_char = self.stream.next();
        self.cur_char
    }

    fn lex(&mut self) -> Option<Token> {
        match self.cur_char? {
            '#' => self.comment(),
            ch if ch.is_whitespace() => self.whitespace(),
            ch if ch.is_alphabetic() => self.get_identifier_token(ch),
            ch if is_float_digit(ch) => self.get_number_token(ch),
            ch => self.get_single_char_token(ch),
        }
    }

    fn whitespace(&mut self) -> Option<Token> {
        while let Some(ch) = self.get_next_char() {
            if !ch.is_whitespace() {
                break;
            }
        }
        self.lex()
    }

    fn comment(&mut self) -> Option<Token> {
        while let Some(ch) = self.get_next_char() {
            if ch == '\n' {
                break;
            }
        }
        self.lex()
    }

    fn get_identifier_token(&mut self, first_char: char) -> Option<Token> {
        let mut identifier = String::new();
        identifier.push(first_char);

        while let Some(ch) = self.get_next_char() {
            if ch.is_alphanumeric() {
                identifier.push(ch);
            } else {
                break;
            }
        }

        match identifier.as_ref() {
            "def" => Some(Token::Def),
            "extern" => Some(Token::Extern),
            "if" => Some(Token::If),
            "then" => Some(Token::Then),
            "else" => Some(Token::Else),
            "for" => Some(Token::For),
            "in" => Some(Token::In),
            "binary" => Some(Token::BinaryDef),
            "unary" => Some(Token::UnaryDef),
            "var" => Some(Token::Var),
            _ => Some(Token::Identifier(identifier)),
        }
    }

    fn get_number_token(&mut self, first_char: char) -> Option<Token> {
        let mut number = String::new();
        number.push(first_char);

        while let Some(ch) = self.get_next_char() {
            if is_float_digit(ch) {
                number.push(ch);
            } else {
                break;
            }
        }

        number.parse().ok().map(Token::Number)
    }

    fn get_single_char_token(&mut self, ch: char) -> Option<Token> {
        let token = match ch {
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '+' => Token::BinaryOp(BinaryOp::Add),
            '-' => Token::BinaryOp(BinaryOp::Sub),
            '*' => Token::BinaryOp(BinaryOp::Mul),
            '/' => Token::BinaryOp(BinaryOp::Div),
            '<' => Token::BinaryOp(BinaryOp::Lt),
            '>' => Token::BinaryOp(BinaryOp::Gt),
            '=' => match self.get_next_char() {
                Some('=') => Token::BinaryOp(BinaryOp::Eq),
                _ => return Some(Token::Assign),
            },
            _ => Token::UnknownChar(ch),
        };
        self.get_next_char();
        Some(token)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.lex()
    }
}

fn is_float_digit(ch: char) -> bool {
    ch.is_ascii_digit() || ch == '.'
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_simple_tokens_and_value() {
        let lexer = Lexer::new("1 + 1 - foo");
        assert!(lexer.eq(vec![
            Token::Number(1.0),
            Token::BinaryOp(BinaryOp::Add),
            Token::Number(1.0),
            Token::BinaryOp(BinaryOp::Sub),
            Token::Identifier(String::from("foo")),
        ]));
    }

    #[test]
    fn test_simple_tokens_and_value_no_whitespace() {
        let lexer = Lexer::new("1+1-foo");
        assert!(lexer.eq(vec![
            Token::Number(1.0),
            Token::BinaryOp(BinaryOp::Add),
            Token::Number(1.0),
            Token::BinaryOp(BinaryOp::Sub),
            Token::Identifier(String::from("foo")),
        ]));
    }

    #[test]
    fn test_comments() {
        let code = "# This is a comment 1+1
        1 + 2 # <- is code
        # this is not";
        let lexer = Lexer::new(code);
        assert!(lexer.eq(vec![
            Token::Number(1.0),
            Token::BinaryOp(BinaryOp::Add),
            Token::Number(2.0),
        ]));
    }

    #[test]
    fn test_if_else() {
        let code = "if x < 3 then 1 else 2";
        let lexer = Lexer::new(code);
        assert!(lexer.eq(vec![
            Token::If,
            Token::Identifier("x".to_string()),
            Token::BinaryOp(BinaryOp::Lt),
            Token::Number(3.0),
            Token::Then,
            Token::Number(1.0),
            Token::Else,
            Token::Number(2.0)
        ]));
    }

    #[test]
    fn test_for_loop() {
        let code = "for i = 1, i < n, 1 in putchard(42)";
        let lexer = Lexer::new(code);
        assert!(lexer.eq(vec![
            Token::For,
            Token::Identifier("i".to_string()),
            Token::Assign,
            Token::Number(1.0),
            Token::Comma,
            Token::Identifier("i".to_string()),
            Token::BinaryOp(BinaryOp::Lt),
            Token::Identifier("n".to_string()),
            Token::Comma,
            Token::Number(1.0),
            Token::In,
            Token::Identifier("putchard".to_string()),
            Token::OpenParen,
            Token::Number(42.0),
            Token::CloseParen,
        ]))
    }

    #[test]
    fn test_def() {
        let code = "def cmp(x y) x < y";
        let lexer = Lexer::new(code);
        assert!(lexer.eq(vec![
            Token::Def,
            Token::Identifier("cmp".to_string()),
            Token::OpenParen,
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::CloseParen,
            Token::Identifier("x".to_string()),
            Token::BinaryOp(BinaryOp::Lt),
            Token::Identifier("y".to_string())
        ]))
    }
}
