//! Contains functions for turning source code into tokens.
use logos::Logos;

pub use sailar::format::Identifier;
pub use std::num::NonZeroUsize as LocationNumber;

pub const DEFAULT_LOCATION_NUMBER: LocationNumber = unsafe { LocationNumber::new_unchecked(1) };

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Location {
    pub line: LocationNumber,
    pub column: LocationNumber,
}

impl Default for Location {
    fn default() -> Self {
        Self {
            line: DEFAULT_LOCATION_NUMBER,
            column: DEFAULT_LOCATION_NUMBER,
        }
    }
}

#[derive(Debug)]
struct LocationMapEntry {
    offset: usize,
    byte_length: usize,
    line: LocationNumber,
}

/// Maps a byte offset into a UTF-8 source file to a line and column number.
#[derive(Debug)]
pub struct LocationMap {
    entries: Vec<LocationMapEntry>,
}

impl LocationMap {
    pub fn get_location(&self, offset: usize) -> Option<Location> {
        let index = self
            .entries
            .binary_search_by(|entry| {
                if entry.offset >= offset && offset < entry.offset + entry.byte_length {
                    std::cmp::Ordering::Equal
                } else if entry.offset < offset {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            })
            .ok()?;

        // Index is guaranteed to point to a valid entry.
        let entry = unsafe { self.entries.get_unchecked(index) };

        Some(Location {
            line: entry.line,
            // NOTE: Currently, the column number is not calculated correctly for multi-byte characters.
            column: LocationNumber::new(offset - entry.offset + 1).expect("column number overflow"),
        })
    }
}

pub struct LocationMapBuilder {
    lookup: LocationMap,
    next: Location,
}

impl Default for LocationMapBuilder {
    fn default() -> Self {
        Self {
            lookup: LocationMap {
                entries: Vec::default(),
            },
            next: Location::default(),
        }
    }
}

fn push_new_line(lexer: &mut logos::Lexer<Token>) -> logos::Skip {
    let byte_length = lexer.slice().len();
    let start_offset = lexer.span().start;
    let builder = &mut lexer.extras;

    builder.lookup.entries.push(LocationMapEntry {
        offset: start_offset,
        byte_length,
        line: LocationNumber::new(builder.next.line.get() + 1).expect("line number overflow"),
    });

    logos::Skip
}

#[non_exhaustive]
#[derive(Clone, Debug, Eq, Logos, PartialEq)]
#[logos(extras = LocationMapBuilder)]
pub enum Token {
    #[token("{")]
    OpenBracket,
    #[token("}")]
    CloseBracket,
    #[token("(")]
    OpenParenthesis,
    #[token(")")]
    CloseParenthesis,
    #[token("=")]
    Equals,
    #[token(".")]
    Period,
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", |lex| Identifier::try_from(lex.slice()))]
    Identifier(Identifier),
    #[token("func")]
    FunctionDefinition,
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

pub fn tokenize(input: &str) -> (Vec<(Token, std::ops::Range<usize>)>, LocationMap) {
    let mut tokens = Vec::with_capacity(256);
    let mut lexer = Token::lexer(input);

    while let Some(next_token) = lexer.next() {
        tokens.push((next_token, lexer.span()));
    }

    (tokens, lexer.extras.lookup)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(tokenize("").0, Vec::new())
    }

    #[test]
    fn single_line() {
        let (tokens, locations) = tokenize("func my_function_name () {}");

        assert_eq!(
            tokens,
            vec![
                (Token::FunctionDefinition, 0..4),
                (Token::new_identifier("my_function_name").unwrap(), 5..21),
                (Token::OpenParenthesis, 22..23),
                (Token::CloseParenthesis, 23..24),
                (Token::OpenBracket, 25..26),
                (Token::CloseBracket, 26..27)
            ]
        )
    }
}
