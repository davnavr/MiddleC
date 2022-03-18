//! Transforms a sequence of tokens into an abstract syntax tree.

use crate::ast;
use crate::lexer::{self, Token};
use crate::location::{self, Location};
use std::fmt::Formatter;
use std::ops::Range;

fn display_token(token: &Token, f: &mut Formatter) -> std::fmt::Result {
    f.write_str(match token {
        Token::Unknown => "unknown",
        Token::OpenCurlyBracket => "{",
        Token::CloseCurlyBracket => "}",
        Token::OpenParenthesis => "(",
        Token::CloseParenthesis => ")",
        Token::OpenSquareBracket => "[",
        Token::CloseSquareBracket => "]",
        Token::Equals => "=",
        Token::Period => ".",
        Token::Comma => ",",
        Token::Semicolon => ";",
        Token::Identifier(identifier) => identifier,
        Token::Arrow => "->",
        Token::FunctionDefinition => "func",
        Token::NamespaceDeclaration => "namespace",
        Token::UseDeclaration => "use",
        Token::StructDefinition => "struct",
        Token::DoubleColon => "::",
    })
}

#[derive(Debug)]
pub struct UnexpectedTokenError {
    expected: &'static [Token],
    actual: Token,
}

impl UnexpectedTokenError {
    pub fn expected(&self) -> &[Token] {
        self.expected
    }

    pub fn actual(&self) -> &Token {
        &self.actual
    }
}

impl std::fmt::Display for UnexpectedTokenError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str("expected ")?;
        for (index, expected) in self.expected.iter().enumerate() {
            display_token(expected, f)?;
            if index > 0 {
                f.write_str(if index == self.expected.len() - 1 {
                    " or "
                } else {
                    ", "
                })?;
            }
        }
        f.write_str(" but got ")?;
        display_token(&self.actual, f)
    }
}

impl std::error::Error for UnexpectedTokenError {}

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("unexpected end of file")]
    UnexpectedEndOfFile,
    #[error(transparent)]
    UnexpectedToken(#[from] UnexpectedTokenError),
    #[error("expected identifier but got {0:?}")]
    TemporaryExpectedIdentifier(Token),
}

#[derive(Debug, thiserror::Error)]
#[error("{location}: {kind}")]
pub struct Error {
    #[source]
    kind: ErrorKind,
    location: Location,
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct Output {
    declarations: Vec<ast::Node<ast::Declaration>>,
    errors: Vec<Error>,
}

impl Output {
    pub fn declarations(&self) -> &[ast::Node<ast::Declaration>] {
        &self.declarations
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    pub fn ok(&self) -> Result<&[ast::Node<ast::Declaration>], &[Error]> {
        if self.errors.is_empty() {
            Ok(&self.declarations)
        } else {
            Err(&self.errors)
        }
    }
}

#[derive(Copy, Clone)]
struct Tokens<'t> {
    tokens: &'t [(Token, Range<usize>)],
    locations: &'t location::Map,
}

impl<'t> Tokens<'t> {
    fn next(&self) -> (Self, Option<&'t (Token, Range<usize>)>) {
        let mut tokens = self.tokens.iter();
        let next = tokens.next();
        (
            Self {
                tokens: tokens.as_slice(),
                locations: self.locations,
            },
            next,
        )
    }

    fn current_location(&self) -> Location {
        // TODO: How to get line and column of last character in file?
        self.tokens
            .first()
            .map(|(_, std::ops::Range { start, .. })| self.locations.get_location(*start))
            .unwrap_or_default()
    }
}

// TODO: Could use (Tokens<'t>, Result<T, (Option<T>, Vec<Error>)>) instead to allow efficient retrieval of tokens.
type MatchResult<'t, T> = Result<(T, Tokens<'t>), (Option<T>, Tokens<'t>, Vec<Error>)>;

type Parser<'t, T> = fn(Tokens<'t>) -> MatchResult<'t, T>;

macro_rules! handle {
    ($expression: expr) => {
        $expression.map_err(|(_, remaining, errors)| (None, remaining, errors))?
    };
}

fn single_error<E: Into<ErrorKind>>(error: E, location: Location) -> Vec<Error> {
    vec![Error {
        kind: error.into(),
        location,
    }]
}

fn single_failure_at<T, E: Into<ErrorKind>>(
    result: Option<T>,
    remaining: Tokens,
    error: E,
    location: Location,
) -> MatchResult<'_, T> {
    Err((result, remaining, single_error(error, location)))
}

fn single_failure<T, E: Into<ErrorKind>>(
    result: Option<T>,
    remaining: Tokens,
    error: E,
) -> MatchResult<'_, T> {
    let location = remaining.current_location();
    single_failure_at(result, remaining, error, location)
}

fn any_token(tokens: Tokens) -> MatchResult<'_, &(Token, Range<usize>)> {
    match tokens.next() {
        (remaining, Some(token)) => Ok((token, remaining)),
        (remaining, None) => single_failure(None, remaining, ErrorKind::UnexpectedEndOfFile),
    }
}

fn expect_token<'t>(tokens: Tokens<'t>, expected: &'static [Token; 1]) -> MatchResult<'t, ()> {
    let ((token, _), remaining) = handle!(any_token(tokens));
    if token == &expected[0] {
        Ok(((), remaining))
    } else {
        single_failure_at(
            None,
            remaining,
            UnexpectedTokenError {
                expected,
                actual: token.clone(),
            },
            tokens.current_location(),
        )
    }
}

fn token_choice<M, T>(tokens: Tokens<'_>, matches: M) -> MatchResult<'_, T>
where
    M: FnOnce(&Token) -> Result<Parser<'_, T>, &'static [Token]>,
{
    let ((token, _), remaining) = handle!(any_token(tokens));
    match matches(token) {
        Ok(parser) => parser(remaining),
        Err(expected) => Err((
            None,
            remaining,
            single_error(
                UnexpectedTokenError {
                    expected,
                    actual: token.clone(),
                },
                tokens.current_location(),
            ),
        )),
    }
}

fn node<'t, T>(tokens: Tokens<'t>, parser: Parser<'t, T>) -> MatchResult<'t, ast::Node<T>> {
    let start_location = tokens.current_location();
    match parser(tokens) {
        Ok((contents, remaining)) => Ok((
            ast::Node::new(contents, &start_location, &remaining.current_location()),
            remaining,
        )),
        Err((contents, remaining, error)) => Err((
            contents
                .map(|node| ast::Node::new(node, &start_location, &remaining.current_location())),
            remaining,
            error,
        )),
    }
}

// NOTE: Could have references to the Identifiers, etc. in the tokens instead of expensive identifier cloning?
fn identifier(tokens: Tokens) -> MatchResult<'_, ast::Node<ast::Identifier>> {
    match handle!(any_token(tokens)) {
        ((Token::Identifier(identifier), range), remaining) => Ok((
            ast::Node::new(
                identifier.clone(),
                &tokens.locations.get_location(range.start),
                &tokens.locations.get_location(range.end),
            ),
            remaining,
        )),
        ((actual, _), remaining) => single_failure(
            None,
            remaining,
            ErrorKind::TemporaryExpectedIdentifier(actual.clone()),
        ),
    }
}

fn namespace_identifier(tokens: Tokens) -> MatchResult<'_, ast::Node<ast::NamespaceIdentifier>> {
    let mut names = Vec::new();
    let (first_identifier, mut remaining_tokens) = handle!(identifier(tokens));
    let start_location = first_identifier.position.start;
    names.push(first_identifier);

    while let Ok(((Token::DoubleColon, _), next_remaining_tokens)) = any_token(remaining_tokens) {
        let (next_identifier, tokens_after_identifier) = handle!(identifier(next_remaining_tokens));
        names.push(next_identifier);
        remaining_tokens = tokens_after_identifier;
    }

    let end_location = unsafe { names.get_unchecked(names.len() - 1) }.position.end;

    Ok((
        ast::Node {
            node: unsafe { ast::NamespaceIdentifier::new_unchecked(names) },
            position: Range {
                start: start_location,
                end: end_location,
            },
        },
        remaining_tokens,
    ))
}

fn delimited_by<'t, T>(
    tokens: Tokens<'t>,
    start_delimiter: &'static [Token; 1],
    end_delimiter: &'static [Token; 1],
    parser: Parser<'t, T>,
) -> MatchResult<'t, T> {
    let ((), remaining) = handle!(expect_token(tokens, start_delimiter));
    let (result, remaining) = handle!(parser(remaining));
    let ((), remaining) = handle!(expect_token(remaining, end_delimiter));
    Ok((result, remaining))
}

fn repeat_parser<'t, T>(tokens: Tokens<'t>, parser: Parser<'t, T>) -> MatchResult<'t, Vec<T>> {
    //let mut errors = Vec::new();
    //let mut results = Vec::new();

    todo!("how does parser indicate that parsing should stop?");
}

fn top_level_declaration_node(tokens: Tokens) -> MatchResult<'_, ast::Node<ast::Declaration>> {
    node(tokens, |tokens| {
        token_choice(tokens, |token| match token {
            Token::NamespaceDeclaration => Ok(|tokens| {
                let (name, remaining) = handle!(namespace_identifier(tokens));
                let (declarations, remaining) = delimited_by(
                    remaining,
                    &[Token::OpenCurlyBracket],
                    &[Token::CloseCurlyBracket],
                    top_level_declarations,
                )
                .unwrap_or_else(|(result, remaining, _)| (result.unwrap_or_default(), remaining));

                Ok((
                    ast::Declaration::Namespace { name, declarations },
                    remaining,
                ))
            }),
            _ => Err(&[Token::NamespaceDeclaration]),
        })
    })
}

fn top_level_declarations(tokens: Tokens) -> MatchResult<'_, Vec<ast::Node<ast::Declaration>>> {
    repeat_parser(tokens, top_level_declaration_node)
}

pub fn parse(tokens: &lexer::Output) -> Output {
    let mut errors = Vec::new();
    let mut declarations = Vec::new();
    let mut input = Tokens {
        tokens: tokens.tokens(),
        locations: tokens.locations(),
    };

    while !input.tokens.is_empty() {
        match top_level_declaration_node(input) {
            Ok((next_declaration, remaining_input)) => {
                input = remaining_input;
                declarations.push(next_declaration)
            }
            Err((parsed_declaration, remaining_input, mut next_errors)) => {
                if let Some(next_declaration) = parsed_declaration {
                    declarations.push(next_declaration);
                }

                input = remaining_input;
                errors.append(&mut next_errors);
            }
        }
    }

    Output {
        declarations,
        errors,
    }
}

// TODO: Have property tests that generates AST -> calls Display -> parses output -> etc.
