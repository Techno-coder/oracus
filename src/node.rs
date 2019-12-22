use std::fmt;

use crate::span::Spanned;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Identifier<'a>(pub &'a str);

impl<'a> fmt::Display for Identifier<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let Identifier(identifier) = self;
		write!(f, "{}", identifier)
	}
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Path<'a>(pub Vec<Identifier<'a>>);

impl<'a> Path<'a> {
	pub fn single(identifier: Identifier<'a>) -> Self {
		Path(vec![identifier])
	}

	pub fn push(mut self, identifier: Identifier<'a>) -> Self {
		let Path(elements) = &mut self;
		elements.push(identifier);
		self
	}

	pub fn prefix(&self, identifier: Identifier<'a>) -> Self {
		let Path(mut elements) = self.clone();
		elements.insert(0, identifier);
		Path(elements)
	}
}

impl<'a> fmt::Display for Path<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let Path(identifiers) = self;
		identifiers.split_last().map(|(last, slice)| {
			slice.iter().try_for_each(|identifier| write!(f, "{}::", identifier))?;
			write!(f, "{}", last)
		}).unwrap_or(Ok(()))
	}
}

#[derive(Debug, PartialEq)]
pub enum Type<'a> {
	Concrete(Path<'a>, Vec<Type<'a>>),
	Reference(Box<Type<'a>>),
	Pointer(Box<Type<'a>>),
	Constant(Box<Type<'a>>),
}

impl<'a> Type<'a> {
	pub fn void() -> Type<'static> {
		Type::Concrete(Path::single(Identifier("void")), Vec::new())
	}
}

impl<'a> fmt::Display for Type<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Type::Concrete(path, templates) => {
				match templates.split_last() {
					None => write!(f, "{}", path),
					Some((last, slice)) => {
						write!(f, "{}<", path)?;
						slice.iter().try_for_each(|template| write!(f, "{}, ", template))?;
						write!(f, "{}>", last)
					}
				}
			}
			Type::Pointer(pointer) => write!(f, "{}*", pointer),
			Type::Reference(reference) => write!(f, "{}&", reference),
			Type::Constant(constant) => write!(f, "const {}", constant),
		}
	}
}

#[derive(Debug)]
pub enum RootNode<'a> {
	Include(Spanned<Identifier<'a>>),
	UsingNamespace(Spanned<Identifier<'a>>),
	Constant(Type<'a>, Identifier<'a>, Expression<'a>),
	Function(Function<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
	pub return_type: Type<'a>,
	pub identifier: Identifier<'a>,
	pub parameters: Vec<(Identifier<'a>, Type<'a>)>,
	pub body: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
	Variable(Type<'a>, Vec<(Identifier<'a>, Option<Vec<Expression<'a>>>)>),
	Conditional(Expression<'a>, Box<Statement<'a>>, Option<Box<Statement<'a>>>),
	ForLoop((Box<Statement<'a>>, Expression<'a>, Box<Statement<'a>>), Box<Statement<'a>>),
	ForRange((Type<'a>, Identifier<'a>, Expression<'a>), Box<Statement<'a>>),
	DoWhile(Box<Statement<'a>>, Expression<'a>),
	While(Expression<'a>, Box<Statement<'a>>),
	Return(Option<Expression<'a>>),
	Expression(Expression<'a>),
	Scope(Vec<Statement<'a>>),
	Break,
	Empty,
}

impl<'a> Statement<'a> {
	pub fn terminated(&self) -> bool {
		match self {
			Statement::Conditional(_, _, _) => false,
			Statement::ForLoop(_, _) => false,
			Statement::ForRange(_, _) => false,
			Statement::While(_, _) => false,
			Statement::Scope(_) => false,
			_ => true,
		}
	}
}

#[derive(Debug)]
pub enum Expression<'a> {
	Float(f64),
	Integer(i128),
	Boolean(bool),
	String(&'a str),
	Character(char),
	Variable(Path<'a>),
	InitializerList(Vec<Expression<'a>>),
	Construction(Type<'a>, Vec<Expression<'a>>),
	Unary(UnaryOperator, Box<Expression<'a>>),
	PostUnary(PostUnaryOperator, Box<Expression<'a>>),
	Binary(BinaryOperator, Box<Expression<'a>>, Box<Expression<'a>>),
	BinaryAssign(ValueOperator, Box<Expression<'a>>, Box<Expression<'a>>),
	Ternary(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
	Assign(Box<Expression<'a>>, Box<Expression<'a>>),
	Index(Box<Expression<'a>>, Box<Expression<'a>>),
	Field(Box<Expression<'a>>, Identifier<'a>),
	FunctionCall(Path<'a>, Vec<Expression<'a>>),
	MethodCall(Box<Expression<'a>>, Identifier<'a>, Vec<Expression<'a>>),
	Dereference(Box<Expression<'a>>),
}

#[derive(Debug)]
pub enum UnaryOperator {
	Increment,
	Decrement,
	Minus,
	Not,
}

#[derive(Debug)]
pub enum PostUnaryOperator {
	Increment,
	Decrement,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
	Value(ValueOperator),
	LessThan,
	LessEqual,
	GreaterThan,
	GreaterEqual,
	Equal,
	NotEqual,
	LogicalOr,
	LogicalAnd,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueOperator {
	Add,
	Minus,
	Multiply,
	Divide,
	Modulo,
	And,
	Or,
	ExclusiveOr,
	ShiftLeft,
	ShiftRight,
}
