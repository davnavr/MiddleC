//! Transforms a sequence of tokens into an abstract syntax tree.

use crate::{ast, lexer};

#[derive(Debug)]
pub enum Error {}

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

pub fn parse(tokens: lexer::Output) -> Output {
    todo!()
}

// TODO: Have property tests that generates AST -> calls Display -> parses output -> etc.
