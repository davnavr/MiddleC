//! Transforms a sequence of tokens into an abstract syntax tree.

use crate::{ast, lexer, location::Location};

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("unexpected {0:?}")]
    UnknownToken(lexer::Token),
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

trait ErrorHandler {
    fn push(&mut self, error: Error);
}

impl ErrorHandler for Vec<Error> {
    fn push(&mut self, error: Error) {
        Vec::push(self, error)
    }
}

type Tokens<'t, 'l> = &'t mut lexer::LocatedIter<'l>;

/*
struct Tokens<'a> {
    tokens: lexer::LocatedIter<'a>,
    peek_buffer: Vec<(&'a Token, Location)>,
}
*/

// NOTE: Having input be a slice would be more convenient, and more functional:
/*
// NOTE: This could be used with combine parsers? Each parser could output Result<T, Vec<(ErrorKind, std::ops::Range<usize>)>>.
struct Tokens<'a> {
    tokens: &'a [(Token, std::ops::Range<usize>)],
}

enum MatchResult<'a, T> {
    Success(T, Tokens<'a>),
    /// Indicates that the parser encountered errors.
    /// If no errors are specified, then the parser SOMETHING SOMETHING SPECIAL.
    Failure(Vec<(ErrorKind, std::ops::Range<usize>)>, Tokens<'a>),
}
*/

enum MatchResult<T> {
    Success(T),
    Failure, // (FnOnce(E) -> ())
    /// Indicates that the parser should move back.
    Partial,
}

/*
type MatchResult = Result<T, Vec<Error>>;
*/

fn match_next_token<T, E, F>(tokens: Tokens, mut errors: E, f: F) -> Option<T>
where
    E: ErrorHandler,
    F: FnOnce(&lexer::Token, &Location) -> Option<T>, // TODO: Instead of Option, use a union type
{
    let (token, location) = tokens.next()?;
    let result = f(token, &location);
    if result.is_none() {
        errors.push(Error {
            kind: ErrorKind::UnknownToken(token.clone()),
            location,
        });
    }
    result
}

fn match_next_token_with_position<T, E, F>(tokens: Tokens, errors: E, f: F) -> Option<ast::Node<T>>
where
    E: ErrorHandler,
    F: FnOnce(&lexer::Token, &Location) -> Option<T>,
{
    match_next_token(tokens, errors, |token, location| {
        Some(ast::Node {
            node: f(token, location)?,
            position: location.clone(),
        })
    })
}

// NOTE: Could have references to the Identifiers, etc. in the tokens instead of expensive identifier cloning?

// NOTE: Currently, error handling could mean that a None could pop up with no corresponding error.

fn identifier<E: ErrorHandler>(
    tokens: &mut Tokens,
    errors: E,
) -> Option<ast::Node<ast::Identifier>> {
    match_next_token_with_position(tokens, errors, |token, _| {
        if let lexer::Token::Identifier(identifier) = token {
            Some(identifier.clone())
        } else {
            None
        }
    })
}

fn namespace_identifier<E: ErrorHandler>(
    tokens: &mut Tokens,
    errors: E,
) -> Option<ast::NamespaceIdentifier> {
    let mut names = Vec::new();
    names.push(identifier(tokens, errors)?);
    todo!("parse identifiers");
    Some(unsafe { ast::NamespaceIdentifier::new_unchecked(names) })
}

fn declaration<E: ErrorHandler>(
    tokens: &mut Tokens,
    errors: E,
) -> Option<ast::Node<ast::Declaration>> {
    match_next_token_with_position(tokens, errors, |token, location| match token {
        lexer::Token::NamespaceDeclaration => None,
        _ => None,
    })
}

pub fn parse(tokens: &lexer::Output) -> Output {
    let mut input = tokens.located();
    let mut errors = Vec::<Error>::new();

    todo!()
}

// TODO: Have property tests that generates AST -> calls Display -> parses output -> etc.
