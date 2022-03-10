//! Transforms a sequence of tokens into an abstract syntax tree.

use crate::{ast, lexer, location::Location};

type Tokens<'a, 'b> = &'b mut lexer::LocatedIter<'a>;

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

pub fn parse(tokens: &lexer::Output) -> Output {
    let mut input = tokens.located();
    todo!()
}

// TODO: Have property tests that generates AST -> calls Display -> parses output -> etc.
