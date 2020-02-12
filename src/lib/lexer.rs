use crate::token::Token;
use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    pub stream: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn lex(&mut self) -> Option<Token> {
        self.skip_whitespace()
            .skip_comment()
            .get_identifier_token()
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

    fn skip_comment(&mut self) -> &mut Self {
        if let Some('#') = self.stream.peek() {
            self.stream.next();
            while let Some(&ch) = self.stream.peek() {
                if ch == 'n' {
                    break;
                }
                self.stream.next();
            }
        }
        self
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
