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

struct State<'a> {
    tokens: lexer::LocatedIter<'a>,
    errors: Vec<Error>,
}

impl State<'_> {
    fn match_next_token<T, F: FnOnce(&lexer::Token, &Location) -> Option<T>>(
        &mut self,
        f: F,
    ) -> Option<T> {
        let (token, location) = self.tokens.next()?;
        let result = f(token, &location);
        if result.is_none() {
            self.errors.push(Error {
                kind: ErrorKind::UnknownToken(token.clone()),
                location,
            });
        }
        result
    }

    fn match_next_token_with_position<T, F: FnOnce(&lexer::Token, &Location) -> Option<T>>(
        &mut self,
        f: F,
    ) -> Option<ast::Node<T>> {
        self.match_next_token(|token, location| {
            Some(ast::Node {
                node: f(token, location)?,
                position: location.clone(),
            })
        })
    }
}

// TODO: How to have better error handling? Pass an error handler object around?

// NOTE: Could have references to the Identifiers, etc. in the tokens instead of expensive identifier cloning?

fn identifier(state: &mut State) -> Option<ast::Node<ast::Identifier>> {
    state.match_next_token_with_position(|token, _| {
        if let lexer::Token::Identifier(identifier) = token {
            Some(identifier.clone())
        } else {
            None
        }
    })
}

fn namespace_identifier(state: &mut State) -> Option<ast::NamespaceIdentifier> {
    let mut names = Vec::new();
    names.push(identifier(state)?);
    todo!("parse identifiers");
    Some(unsafe { ast::NamespaceIdentifier::new_unchecked(names) })
}

fn declaration(state: &mut State) -> Option<ast::Node<ast::Declaration>> {
    state.match_next_token_with_position(|token, location| match token {
        lexer::Token::NamespaceDeclaration => None,
        _ => None,
    })
}

pub fn parse(tokens: &lexer::Output) -> Output {
    let mut state = State {
        tokens: tokens.located(),
        errors: Vec::default(),
    };

    todo!()
}

// TODO: Have property tests that generates AST -> calls Display -> parses output -> etc.
