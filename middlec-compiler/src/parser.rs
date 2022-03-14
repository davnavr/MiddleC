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

macro_rules! handle {
    ($expression: expr) => {
        $expression.map_err(|(_, remaining, errors)| (None, remaining, errors))?
    };
}

fn parsed_node_at<'t, T>(
    tokens: Tokens<'t>,
    node: T,
    range: &Range<usize>,
) -> MatchResult<'t, ast::Node<T>> {
    Ok((
        ast::Node {
            node,
            position: tokens.locations.get_location_range(range),
        },
        tokens,
    ))
}

fn single_error(kind: ErrorKind, location: Location) -> Vec<Error> {
    vec![Error { kind, location }]
}

fn single_failure_at<T>(
    result: Option<T>,
    remaining: Tokens,
    kind: ErrorKind,
    location: Location,
) -> MatchResult<'_, T> {
    Err((result, remaining, single_error(kind, location)))
}

fn single_failure<T>(result: Option<T>, remaining: Tokens, kind: ErrorKind) -> MatchResult<'_, T> {
    let location = remaining.current_location();
    single_failure_at(result, remaining, kind, location)
}

// TODO: Make tokens a Tokens<'t> to ensure that the tokens aren't accidentally reused.
fn any_token<'t>(tokens: &Tokens<'t>) -> MatchResult<'t, &'t (Token, Range<usize>)> {
    match tokens.next() {
        (remaining, Some(token)) => Ok((token, remaining)),
        (remaining, None) => single_failure(None, remaining, ErrorKind::UnexpectedEndOfFile),
    }
}

//fn choice<T, >

fn token_choice<M, T>(tokens: Tokens<'_>, matches: M) -> MatchResult<'_, T>
where
    M: FnOnce(&Token) -> Result<fn(Tokens) -> MatchResult<'_, T>, &'static [Token]>,
{
    let location = tokens.current_location();
    let ((token, _), remaining) = handle!(any_token(&tokens));
    match matches(token) {
        Ok(parser) => parser(remaining),
        Err(expected) => Err((
            None,
            remaining,
            single_error(
                UnexpectedTokenError {
                    expected,
                    actual: token.clone(),
                }
                .into(),
                location,
            ),
        )),
    }
}

// NOTE: Could have references to the Identifiers, etc. in the tokens instead of expensive identifier cloning?
fn identifier(tokens: Tokens) -> MatchResult<'_, ast::Node<ast::Identifier>> {
    match handle!(any_token(&tokens)) {
        ((Token::Identifier(identifier), range), remaining) => {
            parsed_node_at(remaining, identifier.clone(), range)
        }
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

    while let Ok(((Token::DoubleColon, _), next_remaining_tokens)) = any_token(&remaining_tokens) {
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

fn top_level_declaration(tokens: Tokens) -> MatchResult<'_, ast::Node<ast::Declaration>> {
    fn namespace_declaration(tokens: Tokens) -> MatchResult<'_, ast::Declaration> {
        //namespace_identifier(tokens)
    }

    token_choice(tokens, |token| match token {
        Token::NamespaceDeclaration => Ok(namespace_declaration),
        _ => Err(&[Token::NamespaceDeclaration]),
    })
}

pub fn parse(tokens: &lexer::Output) -> Output {
    let mut errors = Vec::new();
    let mut declarations = Vec::new();
    let mut input = Tokens {
        tokens: tokens.tokens(),
        locations: tokens.locations(),
    };

    while !input.tokens.is_empty() {
        match top_level_declaration(input) {
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
