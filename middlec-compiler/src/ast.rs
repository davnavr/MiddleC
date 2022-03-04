use std::fmt::{Display, Formatter, Write as _};

pub use sailar::format::{instruction_set::IntegerConstant as LiteralInteger, Identifier};

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Node<N> {
    pub node: N,
    //pub position:
}

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Type {
    Primitive(sailar::format::type_system::Primitive),
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Expression {
    LiteralInteger(LiteralInteger),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Visibility {
    Public,
    //Internal,
    Private,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum FunctionModifier {
    Entry,
}

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct FunctionBody {
    contents: Vec<Expression>,
}

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Function {
    pub name: Node<Identifier>,
    pub visibility: Node<Visibility>,
    pub modifiers: Vec<Node<FunctionModifier>>,
    pub parameters: Vec<Parameter>,
    pub result_types: Vec<Node<Type>>,
    pub body: FunctionBody,
}

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Parameter {
    pub name: Node<Identifier>,
    pub value_type: Node<Type>,
}

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Declaration {
    // Namespace {
    //     name: Vec<Node<Name>>,
    //     items: Declaration,
    // },
    Function(Function),
}

impl<N: Display> Display for Node<N> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.node, f)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Primitive(primitive_type) => Display::fmt(primitive_type, f),
        }
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Public => "public",
            Self::Private => "private",
        })
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} {}", self.name, self.value_type)
    }
}

impl Display for FunctionModifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Entry => "entry",
        })
    }
}

fn display_separated<T: Display, S: FnMut(usize, &T, &mut Formatter) -> std::fmt::Result>(
    items: &[T],
    mut separator: S,
    f: &mut Formatter,
) -> std::fmt::Result {
    for (index, item) in items.iter().enumerate() {
        if index > 0usize {
            separator(index, item, f)?;
        }

        Display::fmt(item, f)?;
    }

    Ok(())
}

fn display_separated_by<T: Display, S: Display>(
    items: &[T],
    separator: S,
    f: &mut Formatter,
) -> std::fmt::Result {
    display_separated(items, |_, _, f| Display::fmt(&separator, f), f)
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::LiteralInteger(literal) => write!(f, "{}", literal.value()),
        }
    }
}

impl Display for FunctionBody {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str("{")?;
        let length = self.contents.len();
        if length > 0usize {
            for (index, expression) in self.contents.iter().enumerate() {
                Display::fmt(expression, f)?;
                if index < length - 1 {
                    f.write_char(';')?;
                }
                f.write_char('\n')?;
            }
        }
        f.write_char('}')
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "func {} {}", self.name, self.visibility)?;

        for Node { node: modifier, .. } in self.modifiers.iter() {
            write!(f, " {}", modifier)?;
        }

        f.write_str(" (")?;
        display_separated_by(&self.parameters, ", ", f)?;
        f.write_str(") ")?;

        if !self.result_types.is_empty() {
            f.write_str(" -> (")?;
            display_separated_by(&self.result_types, ", ", f)?;
            f.write_str(") ")?;
        }

        Display::fmt(&self.body, f)
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Function(declaration) => Display::fmt(declaration, f)?,
        }

        f.write_char('\n')
    }
}
