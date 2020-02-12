use crate::token::Token;
use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    pub stream: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: input.chars().peekable(),
        }
    }

    fn lex(&mut self) -> Option<Token> {
        self.skip_whitespace()
            .skip_comment()
            .or_else(|| self.get_identifier_token())
            .or_else(|| self.get_number_token())
            .or_else(|| self.get_kwd_token())
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
            "" => None,
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

    fn get_kwd_token(&mut self) -> Option<Token> {
        self.stream.next().map(Token::Kwd)
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
        let mut lexer = Lexer::new("1 + 1 - foo");
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Kwd('+'));
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Kwd('-'));
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
        assert_eq!(lexer.next().unwrap(), Token::Kwd('+'));
        assert_eq!(lexer.next().unwrap(), Token::Number(1.0));
        assert_eq!(lexer.next().unwrap(), Token::Kwd('-'));
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
        assert_eq!(lexer.next(), Some(Token::Kwd('+')));
        assert_eq!(lexer.next(), Some(Token::Number(2.0)));
        assert_eq!(lexer.next(), None);
    }
}
