use std::collections::hash_map;

pub use sailar::format::Identifier;
pub use std::num::NonZeroUsize as LocationNumber;

pub const DEFAULT_LOCATION_NUMBER: LocationNumber = unsafe { LocationNumber::new_unchecked(1) };

#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Dedent,
    Indent,
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,
    Equals,
    Identifier(Identifier),
    /// Keyword used in a function declaration.
    Function,
    Unknown(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Location {
    pub line: LocationNumber,
    pub column: LocationNumber,
}

/// Maps an offset into a UTF-8 source file to a line and column number.
#[derive(Clone, Debug)]
pub struct LocationMap(hash_map::HashMap<usize, Location>);

impl LocationMap {
    /// Gets the location of the token at the specified offset.
    pub fn get(&self, offset: usize) -> Option<&Location> {
        self.0.get(&offset)
    }
}

pub trait Input {
    type Error;

    fn next(&mut self) -> Result<Option<char>, Self::Error>;
}

pub struct IteratorInput<I>(I);

impl<I: std::iter::Iterator<Item = char>> Input for IteratorInput<I> {
    type Error = std::convert::Infallible;

    fn next(&mut self) -> Result<Option<char>, Self::Error> {
        Ok(std::iter::Iterator::next(&mut self.0))
    }
}

impl<I: std::iter::IntoIterator<Item = char>> From<I> for IteratorInput<I::IntoIter> {
    fn from(iterator: I) -> Self {
        Self(iterator.into_iter())
    }
}

pub fn tokenize<I, C>(input: C) -> Result<(Vec<(Token, usize)>, LocationMap), I::Error>
where
    I: Input,
    C: Into<I>,
{
    enum MatchResult {
        Continue,
        Success(Token, usize),
        Failure,
    }

    type Match = fn(char, usize, &mut State) -> MatchResult;

    struct CharBuffer {
        buffer: String,
        /// The total number of code points that are part of an unknown token.
        unknown_code_points: usize,
        /// The total number of code points in the buffer.
        code_point_length: usize,
    }

    impl CharBuffer {
        fn with_capacity(capacity: usize) -> Self {
            Self {
                buffer: String::with_capacity(capacity),
                unknown_code_points: 0,
                code_point_length: 0,
            }
        }

        fn push(&mut self, c: char) {
            self.buffer.push(c);
            self.code_point_length += 1;
        }

        fn clear(&mut self) {
            self.buffer.clear();
            self.code_point_length = 0;
        }

        fn contents(&self) -> (&str, &str) {
            (
                &self.buffer[0..self.unknown_code_points],
                &self.buffer[self.unknown_code_points..],
            )
        }

        fn take_unknown(&mut self) -> (String, usize) {
            if self.unknown_code_points > 0 {
                let unknown_characters = self.buffer.drain(0..self.unknown_code_points).collect();
                self.code_point_length -= self.unknown_code_points;
                (
                    unknown_characters,
                    std::mem::replace(&mut self.unknown_code_points, 0),
                )
            } else {
                (String::default(), 0)
            }
        }
    }

    let mut matches: [(Match, usize); 6] = {
        macro_rules! character_match {
            ($expected: expr, $token: ident) => {{
                fn character_match(c: char, index: usize, _: &mut State) -> MatchResult {
                    if index == 0 && c == $expected {
                        MatchResult::Success(Token::$token, 1)
                    } else {
                        MatchResult::Failure
                    }
                }

                (character_match, 0)
            }};
        }

        macro_rules! string_match {
            ($expected: expr, $token: ident) => {{
                fn string_match(c: char, index: usize, _: &mut State) -> MatchResult {
                    static EXPECTED: &[char] = &$expected;

                    match EXPECTED.get(index) {
                        Some(expected) if *expected == c => {
                            if index == EXPECTED.len() - 1 {
                                MatchResult::Success(Token::$token, EXPECTED.len())
                            } else {
                                MatchResult::Continue
                            }
                        }
                        Some(_) | None => MatchResult::Failure,
                    }
                }

                (string_match, 0)
            }};
        }

        [
            character_match!('{', OpenBracket),
            character_match!('}', CloseBracket),
            character_match!('(', OpenParenthesis),
            character_match!(')', CloseParenthesis),
            character_match!('=', Equals),
            string_match!(['f', 'u', 'n', 'c', 't', 'i', 'o', 'n'], Function),
        ]
    };

    struct State {
        /// A byte offset from the start of the input to the current code point.
        offset: usize,
        line: LocationNumber,
        /// An index from the start of the current line to the current code point.
        column: LocationNumber,
        /// Contains unknown characters followed by the contents of the token that is currently being read.
        buffer: CharBuffer,
    }

    impl State {
        fn emit_token(
            &mut self,
            tokens: &mut Vec<(Token, usize)>,
            locations: &mut LocationMap,
            token: Token,
            byte_length: usize,
            code_point_length: usize,
        ) {
            let actual_offset = self.offset - byte_length;
            tokens.push((token, actual_offset));
            locations.0.insert(
                actual_offset,
                Location {
                    line: self.line,
                    column: self.column,
                },
            );
            self.column = LocationNumber::new(self.column.get() + code_point_length).unwrap();
            self.buffer.clear();
        }

        fn emit_unknown_token(
            &mut self,
            tokens: &mut Vec<(Token, usize)>,
            locations: &mut LocationMap,
        ) {
            let (unknown_token, code_point_length) = self.buffer.take_unknown();
            if !unknown_token.is_empty() {
                let byte_length = unknown_token.len();
                self.emit_token(
                    tokens,
                    locations,
                    Token::Unknown(unknown_token),
                    byte_length,
                    code_point_length,
                )
            }
        }
    }

    let mut state = State {
        offset: 0,
        line: DEFAULT_LOCATION_NUMBER,
        column: DEFAULT_LOCATION_NUMBER,
        buffer: CharBuffer::with_capacity(256),
    };

    let mut input_code_points = input.into();
    let mut tokens = Vec::new();
    let mut locations = LocationMap(hash_map::HashMap::new());

    while let Some(code_point) = input_code_points.next()? {
        state.offset += code_point.len_utf8();
        state.buffer.push(code_point);

        let mut best_result = MatchResult::Failure;
        for (character_match, ref mut start) in matches.iter_mut() {
            let result = character_match(code_point, *start, &mut state);

            match result {
                MatchResult::Continue | MatchResult::Success(_, _) => {
                    best_result = result;
                    *start += 1
                }
                MatchResult::Failure => *start = 0,
            }
        }

        match best_result {
            MatchResult::Continue => (),
            MatchResult::Failure => state.buffer.unknown_code_points += 1,
            MatchResult::Success(token, token_length) => {
                state.emit_unknown_token(&mut tokens, &mut locations);
                state.emit_token(
                    &mut tokens,
                    &mut locations,
                    token,
                    token_length,
                    state.buffer.code_point_length,
                );
                matches.iter_mut().for_each(|(_, start)| *start = 0);
            }
        }
    }

    state.emit_unknown_token(&mut tokens, &mut locations);

    Ok((tokens, locations))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(
            tokenize::<IteratorInput<_>, _>("".chars()).unwrap().0,
            Vec::new()
        )
    }

    #[test]
    fn simple_characters() {
        let (tokens, locations) = tokenize::<IteratorInput<_>, _>("(){}".chars()).unwrap();

        assert_eq!(
            tokens,
            vec![
                (Token::OpenParenthesis, 0),
                (Token::CloseParenthesis, 1),
                (Token::OpenBracket, 2),
                (Token::CloseBracket, 3),
            ]
        )
    }

    #[test]
    fn single_line() {
        let (tokens, locations) =
            tokenize::<IteratorInput<_>, _>("function my_function_name () =".chars()).unwrap();

        assert_eq!(
            tokens,
            vec![
                (Token::Function, 0),
                (
                    Token::Identifier(Identifier::try_from("my_function_name").unwrap()),
                    9
                ),
                (Token::OpenParenthesis, 26),
                (Token::CloseParenthesis, 27),
                (Token::Equals, 29)
            ]
        )
    }

    // #[test]
    // fn multiple_lines() {
    //     let (tokens, locations) = tokenize("function test () =\n    ()\n".chars());
    //     dbg!(tokens);
    //     todo!()
    // }
}
