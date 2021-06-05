use self::Token::*;
use anyhow::{bail, Result};
use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    str::Chars,
};

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Token {
    Assign,
    Asterisk,
    Bang,
    Comma,
    Define,
    EndOfFile,
    Equal,
    False,
    Float(f64),
    GreaterThan,
    If,
    Illegal(String),
    Integer(i64),
    Lambda,
    LeftBrace,
    LeftParentheses,
    LessThan,
    Minus,
    NotEqual,
    Plus,
    Procedure(String),
    RightBrace,
    RightParentheses,
    Semicolon,
    Slash,
    StringLiteral(String),
    True,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let symbol = match self {
            Assign => "=".to_string(),
            Asterisk => "*".to_string(),
            Bang => "!".to_string(),
            Comma => ",".to_string(),
            Define => "define".to_string(),
            EndOfFile => EOF_CHAR.to_string(),
            Equal => "==".to_string(),
            False => "false".to_string(),
            Float(number) => number.to_string(),
            GreaterThan => ">".to_string(),
            If => "if".to_string(),
            Illegal(value) => value.to_string(),
            Integer(number) => number.to_string(),
            Lambda => "lambda".to_string(),
            LeftBrace => "{".to_string(),
            LeftParentheses => "(".to_string(),
            LessThan => "<".to_string(),
            Minus => "-".to_string(),
            NotEqual => "!=".to_string(),
            Plus => "+".to_string(),
            Procedure(value) => value.to_string(),
            RightBrace => "}".to_string(),
            RightParentheses => ")".to_string(),
            Semicolon => ";".to_string(),
            Slash => "/".to_string(),
            StringLiteral(value) => value.to_string(),
            True => "true".to_string(),
        };
        write!(f, "{}", symbol)
    }
}

pub const EOF_CHAR: char = '\0';

pub struct Lexer<'a> {
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Self {
            chars: input.chars(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let next_token = self.next_token()?;
            if let Token::EndOfFile = next_token {
                break;
            }
            tokens.push(next_token);
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token> {
        self.skip_while(Self::is_whitespace);
        let first_char = self.read_char();
        let token = match first_char {
            '=' => self.next_char_or(Assign, '=', Equal),
            ';' => Semicolon,
            '(' => LeftParentheses,
            ')' => RightParentheses,
            ',' => Comma,
            '+' => Plus,
            '{' => LeftBrace,
            '}' => RightBrace,
            '!' => self.next_char_or(Bang, '=', NotEqual),
            '<' => LessThan,
            '>' => GreaterThan,
            '-' => Minus,
            '*' => Asterisk,
            '/' => Slash,
            '"' => {
                let literal = self.take_while(|x| x != '"');
                match self.peek_nth(0) {
                    EOF_CHAR => bail!(
                        "Reached end of file while scanning string. Expected closing delimiter '\"'."
                    ),
                    '"' => {
                        self.read_char();
                    }
                    _ => bail!("String literal is missing closing delimiter"),
                }
                StringLiteral(literal)
            }
            EOF_CHAR => EndOfFile,
            c if Self::is_letter(c) => {
                let mut identifier = c.to_string();
                identifier.push_str(&self.take_while(Self::is_letter));
                Self::lookup_identifier(&identifier)
            }
            c if Self::is_digit(c) => {
                let mut number = c.to_string();
                number.push_str(&self.take_while(|c| Self::is_digit(c) || c == '.'));

                if number.chars().all(|c| c.is_numeric()) {
                    Integer(number.parse::<i64>()?)
                } else {
                    Float(number.parse::<f64>()?)
                }
            }
            illegal => Illegal(illegal.to_string()),
        };
        Ok(token)
    }

    fn read_char(&mut self) -> char {
        self.chars.next().unwrap_or(EOF_CHAR)
    }

    fn peek_nth(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or(EOF_CHAR)
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> String {
        let mut chars = String::new();
        while predicate(self.peek_nth(0)) && !self.is_eof() {
            chars.push(self.read_char());
        }
        chars
    }

    fn skip_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek_nth(0)) && !self.is_eof() {
            self.read_char();
        }
    }

    fn is_letter(c: char) -> bool {
        ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
    }

    fn is_digit(c: char) -> bool {
        ('0'..='9').contains(&c)
    }

    fn is_whitespace(c: char) -> bool {
        c == ' ' || c == '\t' || c == '\n' || c == '\r'
    }

    fn lookup_identifier(identifier: &str) -> Token {
        match identifier {
            "lambda" => Lambda,
            "define" => Define,
            "true" => True,
            "false" => False,
            "if" => If,
            _ => Procedure(identifier.to_string()),
        }
    }

    fn next_char_or(&mut self, default: Token, next_char: char, token: Token) -> Token {
        match self.peek_nth(0) {
            c if c == next_char => {
                self.read_char();
                token
            }
            _ => default,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        Lexer, Result,
        Token::{self, *},
    };

    fn check_tokens(input: &str, expected_tokens: &[Token]) -> Result<()> {
        let mut lexer = Lexer::new(input);
        for (token, expected_token) in lexer.tokenize()?.into_iter().zip(expected_tokens.iter()) {
            assert_eq!(token, *expected_token);
        }
        Ok(())
    }

    #[test]
    fn addition() -> Result<()> {
        check_tokens(
            "(+ 2 2.7)",
            &[
                LeftParentheses,
                Plus,
                Integer(2),
                Float(2.7),
                RightParentheses,
                EndOfFile,
            ],
        )
    }

    #[test]
    fn if_expression() -> Result<()> {
        check_tokens(
            "(if (> 6 5) (+ 1 1) (+ 2 2))",
            &[
                LeftParentheses,
                If,
                LeftParentheses,
                GreaterThan,
                Integer(6),
                Integer(5),
                RightParentheses,
                LeftParentheses,
                Plus,
                Integer(1),
                Integer(1),
                RightParentheses,
                LeftParentheses,
                Plus,
                Integer(2),
                Integer(2),
                RightParentheses,
                RightParentheses,
                EndOfFile,
            ],
        )
    }

    #[test]
    fn functions() -> Result<()> {
        check_tokens(
            "(define compose (lambda (f g) (lambda (x) (f (g x)))))",
            &[
                LeftParentheses,
                Define,
                Procedure("compose".to_string()),
                LeftParentheses,
                Lambda,
                LeftParentheses,
                Procedure("f".to_string()),
                Procedure("g".to_string()),
                RightParentheses,
                LeftParentheses,
                Lambda,
                LeftParentheses,
                Procedure("x".to_string()),
                RightParentheses,
                LeftParentheses,
                Procedure("f".to_string()),
                LeftParentheses,
                Procedure("g".to_string()),
                Procedure("x".to_string()),
                RightParentheses,
                RightParentheses,
                RightParentheses,
                RightParentheses,
                RightParentheses,
                EndOfFile,
            ],
        )
    }
}
