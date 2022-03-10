//! Contains functions for turning source code into tokens.

use crate::location;
use logos::Logos;

pub use sailar::format::Identifier;
pub use std::num::NonZeroUsize as LocationNumber;

#[non_exhaustive]
#[derive(Clone, Debug, Eq, Logos, PartialEq)]
#[logos(extras = location::MapBuilder)]
pub enum Token {
    #[token("{")]
    OpenCurlyBracket,
    #[token("}")]
    CloseCurlyBracket,
    #[token("(")]
    OpenParenthesis,
    #[token(")")]
    CloseParenthesis,
    #[token("[")]
    OpenSquareBracket,
    #[token("]")]
    OpenCloseBracket,
    #[token("=")]
    Equals,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    /// Indicates the return type of a function.
    #[token("->")]
    Arrow,
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", |lex| Identifier::try_from(lex.slice()))]
    Identifier(Identifier),
    #[token("func")]
    FunctionDefinition,
    #[token("namespace")]
    NamespaceDeclaration,
    #[token("use")]
    UseDeclaration,
    #[token("struct")]
    StructDefinition,
    #[token("::")]
    DoubleColon,
    #[regex(r"[ \t\r]+", logos::skip)]
    #[token("\n", push_new_line)]
    #[error]
    Unknown,
}

impl Token {
    pub fn new_identifier<S: TryInto<Identifier>>(identifier: S) -> Option<Self> {
        identifier.try_into().ok().map(Token::Identifier)
    }
}

fn push_new_line(lexer: &mut logos::Lexer<Token>) -> logos::Skip {
    let start_offset = lexer.span().start;
    lexer.extras.push(start_offset);
    logos::Skip
}

/// An iterator over tokens paired with their line and column numbers in the original source file.
///
/// See [`Output::located`] for more information.
#[derive(Clone, Debug)]
pub struct LocatedIter<'a> {
    tokens: std::slice::Iter<'a, (Token, std::ops::Range<usize>)>,
    locations: &'a location::Map,
}

impl<'a> std::iter::Iterator for LocatedIter<'a> {
    type Item = (&'a Token, location::Location);

    fn next(&mut self) -> Option<Self::Item> {
        let (token, offset) = self.tokens.next()?;
        Some((token, self.locations.get_location(offset.start)))
    }
}

impl std::iter::ExactSizeIterator for LocatedIter<'_> {
    fn len(&self) -> usize {
        self.tokens.len()
    }
}

impl std::iter::FusedIterator for LocatedIter<'_> {}

#[derive(Clone, Debug)]
pub struct Output {
    tokens: Vec<(Token, std::ops::Range<usize>)>,
    locations: location::Map,
}

impl Output {
    pub fn tokens(&self) -> &[(Token, std::ops::Range<usize>)] {
        &self.tokens
    }

    pub fn locations(&self) -> &location::Map {
        &self.locations
    }

    pub fn located(&self) -> LocatedIter {
        LocatedIter {
            tokens: self.tokens.iter(),
            locations: &self.locations,
        }
    }
}

/// Turns an input string into a sequence of tokens paired with their corresponding byte ranges.
pub fn tokenize(input: &str) -> Output {
    let mut tokens = Vec::with_capacity(256);
    let mut lexer = Token::lexer(input);

    while let Some(next_token) = lexer.next() {
        tokens.push((next_token, lexer.span()));
    }

    Output {
        tokens,
        locations: lexer.extras.finish(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_location_eq {
        ($locations: expr, $offset: expr, $expected_line: expr, $expected_column: expr) => {{
            assert_eq!(
                $locations.get_location($offset),
                location::Location {
                    line: LocationNumber::new($expected_line).expect("invalid line number"),
                    column: LocationNumber::new($expected_column).expect("invalid column number")
                }
            )
        }};
    }

    #[test]
    fn empty() {
        assert_eq!(tokenize("").tokens(), &[])
    }

    #[test]
    fn single_line() {
        let output = tokenize("func my_function_name () {}");

        assert_eq!(
            output.tokens(),
            &[
                (Token::FunctionDefinition, 0..4),
                (Token::new_identifier("my_function_name").unwrap(), 5..21),
                (Token::OpenParenthesis, 22..23),
                (Token::CloseParenthesis, 23..24),
                (Token::OpenCurlyBracket, 25..26),
                (Token::CloseCurlyBracket, 26..27)
            ]
        );

        assert_location_eq!(output.locations(), 0, 1, 1);
        assert_location_eq!(output.locations(), 5, 1, 6);
    }

    #[test]
    fn multiple_lines() {
        let output = tokenize("{\n    (\n}\n\nfunc\n");

        assert_eq!(
            output.tokens(),
            vec![
                (Token::OpenCurlyBracket, 0..1),
                (Token::OpenParenthesis, 6..7),
                (Token::CloseCurlyBracket, 8..9),
                (Token::FunctionDefinition, 11..15)
            ]
        );

        let line_numbers = output
            .locations()
            .iter_over(0..16)
            .map(|location| (location.line.get(), location.column.get()))
            .collect::<Vec<_>>();

        assert_eq!(
            line_numbers,
            vec![
                (1, 1),
                (2, 1),
                (2, 2),
                (2, 3),
                (2, 4),
                (2, 5),
                (2, 6),
                (3, 1),
                (3, 2),
                (4, 1),
                (5, 1),
                (5, 2),
                (5, 3),
                (5, 4),
                (5, 5),
                (6, 1),
            ]
        );
    }
}
