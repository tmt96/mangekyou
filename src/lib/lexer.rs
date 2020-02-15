use crate::token::*;
use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

// Lexer/tokenizer implementation
pub struct Lexer<'a> {
    // TODO: use a separate char state instead of using peekable
    stream: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: input.chars().peekable(),
        }
    }

    // TODO: use Result type instead of Option
    fn lex(&mut self) -> Option<Token> {
        self.skip_whitespace()
            .skip_comment()
            .or_else(|| self.get_identifier_token())
            .or_else(|| self.get_number_token())
            .or_else(|| self.get_separator_token())
            .or_else(|| self.get_binary_operator_token())
            .or_else(|| self.get_unknown_char_token())
    }

    fn skip_whitespace(&mut self) -> &mut Self {
        while let Some(ch) = self.stream.peek() {
            if !ch.is_whitespace() {
                break;
            }
            self.stream.next();
        }

        self
    }

    fn skip_comment(&mut self) -> Option<Token> {
        if let Some('#') = self.stream.peek() {
            while let Some(ch) = self.stream.next() {
                if ch == '\n' {
                    break;
                }
            }
            self.lex()
        } else {
            None
        }
    }

    fn get_identifier_token(&mut self) -> Option<Token> {
        let next_char = *self.stream.peek()?;
        if !next_char.is_alphabetic() {
            return None;
        }
        self.stream.next();

        let mut identifier = String::new();
        identifier.push(next_char);
        while let Some(&ch) = self.stream.peek() {
            if ch.is_alphanumeric() {
                identifier.push(ch);
                self.stream.next();
            } else {
                break;
            }
        }

        match identifier.as_ref() {
            "def" => Some(Token::Def),
            "extern" => Some(Token::Extern),
            _ => Some(Token::Identifier(identifier)),
        }
    }

    fn get_number_token(&mut self) -> Option<Token> {
        let next_char = *self.stream.peek()?;
        if !is_float_digit(next_char) {
            return None;
        }
        self.stream.next();

        let mut identifier = String::new();
        identifier.push(next_char);
        while let Some(&ch) = self.stream.peek() {
            if is_float_digit(ch) {
                identifier.push(ch);
                self.stream.next();
            } else {
                break;
            }
        }

        identifier.parse().ok().map(Token::Number)
    }

    fn get_unknown_char_token(&mut self) -> Option<Token> {
        self.stream.next().map(Token::UnknownChar)
    }

    fn get_separator_token(&mut self) -> Option<Token> {
        let token = match self.stream.peek() {
            Some('(') => Some(Token::OpenParen),
            Some(')') => Some(Token::CloseParen),
            Some(',') => Some(Token::Comma),
            Some(';') => Some(Token::Semicolon),
            _ => None,
        };

        if token.is_some() {
            self.stream.next();
        }
        token
    }

    fn get_binary_operator_token(&mut self) -> Option<Token> {
        let operator = match self.stream.peek() {
            Some('+') => Some(BinaryOp::Add),
            Some('-') => Some(BinaryOp::Sub),
            Some('*') => Some(BinaryOp::Mul),
            Some('/') => Some(BinaryOp::Div),
            Some('<') => Some(BinaryOp::Lt),
            Some('>') => Some(BinaryOp::Gt),
            _ => None,
        };

        if operator.is_some() {
            self.stream.next();
        }
        operator.map(Token::BinaryOp)
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
    // TODO: Fix tests!!
    use super::*;
    #[test]
    fn test_simple_tokens_and_value() {
        let mut lexer = Lexer::new("1 + 1 - foo");
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::BinaryOp(BinaryOp::Add));
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::BinaryOp(BinaryOp::Sub));
        assert_eq!(
            lexer.next().unwrap(),
            Token::Identifier(String::from("foo"))
        );
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_simple_tokens_and_value_no_whitespace() {
        let mut lexer = Lexer::new("1+1-foo");
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::BinaryOp(BinaryOp::Add));
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::BinaryOp(BinaryOp::Sub));
        assert_eq!(
            lexer.next().unwrap(),
            Token::Identifier(String::from("foo"))
        );
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_comments() {
        let code = "# This is a comment 1+1
        1 + 2 # <- is code
        # this is not";
        let mut lexer = Lexer::new(code);
        assert_eq!(lexer.next(), Some(Token::Number(1.0)));
        assert_eq!(lexer.next(), Some(Token::BinaryOp(BinaryOp::Add)));
        assert_eq!(lexer.next(), Some(Token::Number(2.0)));
        assert_eq!(lexer.next(), None);
    }
}
