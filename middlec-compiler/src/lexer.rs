use std::collections::hash_map;

pub use sailar::format::Identifier;
pub use std::num::NonZeroUsize as LocationNum;

pub const DEFAULT_LOCATION_NUMBER: LocationNum = unsafe { LocationNum::new_unchecked(1) };

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
    pub line: LocationNum,
    pub column: LocationNum,
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

//pub trait Input

// TODO: Maybe add parameter that says how many spaces correspond to a tab?
/// Tokenizes a string of characters, returning the tokens paired with UTF-8 byte offsets pointing into the original string, as
/// well as a map to determine the line and column numbers from an offset.
pub fn tokenize<I: std::iter::IntoIterator<Item = char>>(
    input: I,
) -> (Vec<(Token, usize)>, LocationMap) {
    enum MatchResult {
        Continue,
        Success(Token),
        Failure,
    }

    //struct State

    // TODO: Make this a simple type alias.
    /// Matches against a character in the input, taking an offset from the start of the token.
    struct Match(fn(char, usize, &mut &'static [Match]) -> MatchResult);

    macro_rules! string_match {
        ($string: expr, $token: ident) => {{
            fn string_match(c: char, index: usize, _: &mut &'static [Match]) -> MatchResult {
                static EXPECTED_STRING: &[char] = &$string;

                match EXPECTED_STRING.get(index) {
                    Some(expected) if *expected == c => {
                        if index == EXPECTED_STRING.len() - 1 {
                            MatchResult::Success(Token::$token)
                        } else {
                            MatchResult::Continue
                        }
                    }
                    Some(_) | None => MatchResult::Failure,
                }
            }

            Match(string_match)
        }};
    }

    macro_rules! character_match {
        ($expected: expr, $token: ident) => {{
            fn char_match(c: char, index: usize, _: &mut &'static [Match]) -> MatchResult {
                if index == 0 && c == $expected {
                    MatchResult::Success(Token::$token)
                } else {
                    MatchResult::Failure
                }
            }

            Match(char_match)
        }};
    }

    // Matches further down have higher priority.
    static DEFAULT_MATCHES: &[Match] = &[
        string_match!(['f', 'u', 'n', 'c', 't', 'i', 'o', 'n'], Function),
        character_match!('{', OpenBracket),
        character_match!('}', CloseBracket),
        character_match!('(', OpenParenthesis),
        character_match!(')', CloseParenthesis),
        character_match!('=', Equals),
    ];

    let mut characters = String::new();
    let mut current_matches = DEFAULT_MATCHES;
    let mut unknown_length = 0usize;
    let mut offset = 0usize;
    let mut tokens = Vec::new();
    let mut locations = LocationMap(hash_map::HashMap::new());
    let mut column = DEFAULT_LOCATION_NUMBER;
    let mut line = DEFAULT_LOCATION_NUMBER;
    let mut indentation_level = 0usize;

    macro_rules! emit_token {
        ($token: expr, $offset: expr) => {{
            let actual_offset = offset - characters.len() - ($offset);
            locations.0.insert(actual_offset, Location { line, column });
            tokens.push(($token, actual_offset));
            characters.clear();
        }};
    }

    for code_point in input.into_iter() {
        offset += code_point.len_utf8();

        if code_point == '\n' {
            if !characters.is_empty() {
                emit_token!(Token::Unknown(characters.clone()), 0);
                unknown_length = 0;
            }

            column = DEFAULT_LOCATION_NUMBER;
            line = line
                .get()
                .checked_add(1)
                .and_then(LocationNum::new)
                .expect("line number overflow");
        } else if code_point.is_whitespace() && column > DEFAULT_LOCATION_NUMBER {
            debug_assert_eq!(characters.chars().count(), unknown_length);

            if !characters.is_empty() {
                emit_token!(Token::Unknown(characters.clone()), 1);
                unknown_length = 0;
            }

            column = LocationNum::new(column.get() + 1 + unknown_length).expect("column overflow");

            // // TODO: Collect whitespace instead.
            // let indentation_amount = if code_point != '\t' {
            //     1usize
            // } else {
            //     todo!("special handling for calculating indentation when code point is tab")
            // };

            // indentation_level = indentation_amount;

            // if indentation_amount < indentation_level {
            //     emit_token!(Token::Dedent);
            // } else if indentation_amount > indentation_level {
            //     emit_token!(Token::Indent);
            // }
        } else {
            let mut best_result = MatchResult::Failure;

            for character_match in current_matches {
                let result = character_match.0(code_point, characters.len(), &mut current_matches);

                match result {
                    MatchResult::Success(_) | MatchResult::Continue => best_result = result,
                    MatchResult::Failure => (),
                }
            }

            characters.push(code_point);

            match best_result {
                MatchResult::Continue => (),
                MatchResult::Failure => unknown_length += 1,
                MatchResult::Success(token) => {
                    if unknown_length > 0 {
                        emit_token!(
                            Token::Unknown(characters.chars().take(unknown_length).collect()),
                            unknown_length
                        );

                        column = LocationNum::new(column.get() + unknown_length)
                            .expect("column overflow");

                        unknown_length = 0;
                    }

                    let next_column = column.get() + characters.chars().count();
                    emit_token!(token, 0);
                    column = LocationNum::new(next_column).expect("column overflow");
                }
            }
        }
    }

    if !characters.is_empty() {
        emit_token!(Token::Unknown(characters.clone()), 0);
    }

    (tokens, locations)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(tokenize("".chars()).0, Vec::new())
    }

    #[test]
    fn single_line() {
        let (tokens, locations) = tokenize("function my_function_name () =".chars());

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

    #[test]
    fn multiple_lines() {
        let (tokens, locations) = tokenize("function test () =\n    ()\n".chars());
        dbg!(tokens);
        todo!()
    }
}
