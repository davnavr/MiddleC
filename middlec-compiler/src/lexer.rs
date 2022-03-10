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
    pub fn get_location(&self, offset: usize) -> Location {
        let entry_index = self.entries.binary_search_by(|entry| {
            if offset >= entry.offset && offset < entry.offset + entry.byte_length {
                std::cmp::Ordering::Equal
            } else if entry.offset < offset {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            }
        });

        let line;
        // NOTE: Currently, the column number is not calculated correctly for multi-byte characters.
        let column;
        
        match entry_index {
            Ok(index) => {
                // Index is guaranteed to point to a valid entry.
                let entry = unsafe { self.entries.get_unchecked(index) };
                line = entry.line;
                column = offset - entry.offset + 1;
            },
            Err(index) => {
                if index == 0 || self.entries.is_empty() {
                    line = DEFAULT_LOCATION_NUMBER;
                    column = offset + 1;
                } else {
                    // The entries are guaranteed to not be empty.
                    let last = unsafe { self.entries.get_unchecked(self.entries.len() - 1) };
                    line = last.line;
                    dbg!(offset, last);
                    column = offset - last.offset + 1;
                }
            }
        }

        Location {
            line,
            column: LocationNumber::new(column).expect("column number overflow"),
        }
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

    macro_rules! assert_location_eq {
        ($locations: expr, $offset: expr, $expected_line: expr, $expected_column: expr) => {{
            assert_eq!(
                $locations.get_location($offset),
                Location {
                    line: LocationNumber::new($expected_line).expect("invalid line number"),
                    column: LocationNumber::new($expected_column).expect("invalid column number")
                }
            )
        }};
    }

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
        );

        assert_location_eq!(locations, 0, 1, 1);
        assert_location_eq!(locations, 5, 1, 6);
    }

    #[test]
    fn multiple_lines() {
        let (tokens, locations) = tokenize("{\n    (\n}");

        assert_eq!(
            tokens,
            vec![
                (Token::OpenBracket, 0..1),
                (Token::OpenParenthesis, 6..7),
                (Token::CloseBracket, 8..9)
            ]
        );

        assert_location_eq!(locations, 0, 1, 1);
        assert_location_eq!(locations, 6, 2, 5);
    }
}
