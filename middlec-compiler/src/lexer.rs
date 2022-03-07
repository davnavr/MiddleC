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

    macro_rules! string_match {
        ($name: ident, $string: expr, $token: ident) => {
            fn $name(c: char, index: usize) -> MatchResult {
                static EXPECTED_STRING: &[char] = &$string;

                match EXPECTED_STRING.get(index) {
                    None => MatchResult::Success(Token::$token),
                    Some(expected) if *expected == c => MatchResult::Continue,
                    Some(_) => MatchResult::Failure,
                }
            }
        };
    }

    string_match!(
        function_keyword,
        ['f', 'u', 'n', 'c', 't', 'i', 'o', 'n'],
        Function
    );

    /// A function that matches against a character in the input, as well as an offset from the start of the token.
    type Match = fn(char, usize) -> MatchResult;

    static DEFAULT_MATCHES: &[Match] = &[function_keyword];

    let mut characters = String::new();
    let mut current_matches = DEFAULT_MATCHES;
    let mut offset = 0usize;
    let mut tokens = Vec::new();
    let mut locations = LocationMap(hash_map::HashMap::new());
    let mut column = DEFAULT_LOCATION_NUMBER;
    let mut line = DEFAULT_LOCATION_NUMBER;

    macro_rules! emit_token {
        ($token: expr) => {{
            dbg!(&offset, &characters);
            let actual_offset = offset - characters.len();
            locations.0.insert(actual_offset, Location { line, column });
            tokens.push(($token, actual_offset));
            characters.clear();
            column = LocationNum::new(column.get() + characters.chars().count())
                .expect("column overflow");
        }};
    }

    for code_point in input.into_iter() {
        offset += code_point.len_utf8();

        if code_point == '\n' {
            if !characters.is_empty() {
                emit_token!(Token::Unknown(characters.clone()));
            }

            column = DEFAULT_LOCATION_NUMBER;
            line = line
                .get()
                .checked_add(1)
                .and_then(LocationNum::new)
                .expect("line number overflow");
        } else {
            let mut result = MatchResult::Failure;

            for character_match in current_matches {
                result = character_match(code_point, characters.len());

                if let MatchResult::Success(_) = result {
                    break;
                }
            }

            characters.push(code_point);

            match result {
                MatchResult::Continue => (),
                MatchResult::Failure => {
                    emit_token!(Token::Unknown(characters.clone()));
                }
                MatchResult::Success(token) => emit_token!(token),
            }
        }
    }

    if !characters.is_empty() {
        emit_token!(Token::Unknown(characters.clone()));
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
        assert_eq!(
            tokenize("function my_function_name () =".chars()).0,
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
        todo!()
    }
}
