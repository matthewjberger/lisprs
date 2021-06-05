use crate::{flatten, Token};
use anyhow::{bail, Result};
use std::{
    convert::TryFrom,
    fmt::{Display, Formatter, Result as FmtResult},
    matches,
    slice::Iter,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    List(Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let expression = match self {
            Expression::Atom(atom) => atom.to_string(),
            Expression::List(expressions) => {
                let expressions = expressions
                    .into_iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>();
                format!("( {} )", flatten(&expressions, " "))
            }
        };
        write!(f, "{}", expression)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Symbol(String),
    Integer(i64),
    Float(f64),
    Operator(Operator),
}

impl From<&Token> for Atom {
    fn from(token: &Token) -> Self {
        if let Ok(operator) = Operator::try_from(token) {
            return Atom::Operator(operator);
        }
        let token = token.to_string();
        match token.parse::<i64>() {
            Ok(result) => Self::Integer(result),
            Err(_) => match token.parse::<f64>() {
                Ok(result) => Self::Float(result),
                Err(_) => Self::Symbol(token),
            },
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let atom = match self {
            Atom::Symbol(symbol) => symbol.to_string(),
            Atom::Integer(value) => value.to_string(),
            Atom::Float(value) => value.to_string(),
            Atom::Operator(operator) => operator.to_string(),
        };
        write!(f, "{}", atom)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Divide,
    Multiply,
    Not,
    Negate,
    Subtract,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
}

impl TryFrom<&Token> for Operator {
    type Error = anyhow::Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Plus => Self::Add,
            Token::Slash => Self::Divide,
            Token::Asterisk => Self::Multiply,
            Token::Bang => Self::Not,
            Token::Minus => Self::Subtract,
            Token::LessThan => Self::LessThan,
            Token::GreaterThan => Self::GreaterThan,
            Token::Equal => Self::Equal,
            Token::NotEqual => Self::NotEqual,
            token => bail!("Token is not an operator: {}", token),
        })
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let statement = match self {
            Self::Add => "+",
            Self::Subtract | Self::Negate => "-",
            Self::Divide => "/",
            Self::Multiply => "*",
            Self::Not => "!",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "==",
            Self::NotEqual => "!=",
        };
        write!(f, "{}", statement.to_string())
    }
}

pub struct Parser<'a> {
    pub tokens: Iter<'a, Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter(),
        }
    }

    fn read_token(&mut self) -> &Token {
        self.tokens.next().unwrap_or(&Token::EndOfFile)
    }

    fn peek_nth(&self, n: usize) -> &Token {
        self.tokens.clone().nth(n).unwrap_or(&Token::EndOfFile)
    }

    pub fn parse(&mut self) -> Result<Expression> {
        if matches!(self.peek_nth(0), Token::EndOfFile) {
            bail!("Unexpected end of file")
        }
        self.read_token();

        let token = self.read_token();
        let result = match token {
            Token::LeftParentheses => self.parse_expression()?,
            Token::RightParentheses => bail!("Unexpected )"),
            token => Expression::Atom(Atom::from(token)),
        };

        Ok(result)
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        let mut expressions = Vec::new();
        self.read_token(); // (
        while self.peek_nth(0) != &Token::RightParentheses {
            expressions.push(self.parse()?);
        }
        self.read_token(); // )
        Ok(Expression::List(expressions))
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, Parser};
    use crate::{lexer::Lexer, Atom};
    use anyhow::Result;

    #[test]
    fn string_literal_expression() -> Result<()> {
        parse_expression(
            "",
            &Expression::Atom(Atom::Symbol("hello world".to_string())),
        )
    }

    fn parse_expression(input: &str, expected_expression: &Expression) -> Result<()> {
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokenize()?;

        let mut parser = Parser::new(&tokens);
        let expression = parser.parse()?;
        assert_eq!(expression, *expected_expression);

        Ok(())
    }
}
