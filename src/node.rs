use std::collections::HashMap;
use std::fmt;

use crate::execute::{ExecutionContext, ExecutionResult, Reference};
use crate::span::S;
use crate::symbol::SymbolContext;
use crate::value::Value;

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

impl<'a, T> From<T> for Path<'a> where T: AsRef<[&'a str]> {
	fn from(elements: T) -> Self {
		Path(elements.as_ref().iter().cloned().map(Identifier).collect())
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

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
	Integral(IntegralKind),
	Concrete(Path<'a>, Vec<Type<'a>>),
	Reference(Box<Type<'a>>),
	Pointer(Box<Type<'a>>),
	Constant(Box<Type<'a>>),
}

impl<'a> Type<'a> {
	pub fn single(path: Path<'a>) -> Self {
		Type::Concrete(path, Vec::new())
	}

	pub fn void() -> Type<'static> {
		Type::single(Path::single(Identifier("void")))
	}
}

impl<'a> fmt::Display for Type<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Type::Integral(kind) => write!(f, "{}", kind),
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

#[derive(Debug, Clone, PartialEq)]
pub struct IntegralKind(pub bool, pub IntegralRank);

impl fmt::Display for IntegralKind {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let IntegralKind(unsigned, rank) = self;
		if *unsigned { write!(f, "unsigned ")?; }
		write!(f, "{}", rank)
	}
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum IntegralRank {
	Byte = 1,
	Short = 2,
	Integer = 3,
	LongLong = 4,
	Unknown = 0,
}

impl fmt::Display for IntegralRank {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			IntegralRank::Byte => write!(f, "char"),
			IntegralRank::Short => write!(f, "short"),
			IntegralRank::Integer => write!(f, "int"),
			IntegralRank::LongLong => write!(f, "long long"),
			IntegralRank::Unknown => write!(f, "integral"),
		}
	}
}

#[derive(Debug, Default)]
pub struct Program<'a> {
	pub functions: HashMap<Path<'a>, Vec<Function<'a>>>,
	pub intrinsics: Vec<Box<dyn Intrinsic>>,
}

pub trait Intrinsic: fmt::Debug {
	fn register<'a>(&self, context: &mut SymbolContext<'a>);
	fn variable<'a, 'b>(&self, _program: &'b Program<'a>, _context: &mut ExecutionContext<'a, 'b>,
	                    _variable: &S<Path<'a>>) -> ExecutionResult<Option<Value<'a>>> { Ok(None) }
	fn operation<'a, 'b>(&self, _program: &'b Program<'a>, _context: &mut ExecutionContext<'a, 'b>,
	                     _operator: BinaryOperator, _left: &S<Value<'a>>, _right: &S<Value<'a>>)
	                     -> ExecutionResult<Option<Value<'a>>> { Ok(None) }
	fn function<'a, 'b>(&self, _program: &'b Program<'a>, _context: &mut ExecutionContext<'a, 'b>,
	                    _function: &S<Path<'a>>, _arguments: &[S<Value<'a>>])
	                    -> ExecutionResult<Option<Value<'a>>> { Ok(None) }
	fn method<'a, 'b>(&self, _program: &'b Program<'a>, _context: &mut ExecutionContext<'a, 'b>,
	                  _target: &S<Value<'a>>, _method: &S<Identifier<'a>>, _arguments: &[S<Value<'a>>])
	                  -> ExecutionResult<Option<Value<'a>>> { Ok(None) }
	fn assign<'a, 'b>(&self, _program: &'b Program<'a>, _context: &mut ExecutionContext<'a, 'b>,
	                  _target: &S<Reference<'a>>, _value: &S<Value<'a>>)
	                  -> ExecutionResult<Option<Value<'a>>> { Ok(None) }
	fn construct<'a, 'b>(&self, _program: &'b Program<'a>, _context: &mut ExecutionContext<'a, 'b>,
	                     _structure: &S<Type<'a>>, _arguments: &[Value<'a>])
	                     -> ExecutionResult<Option<Value<'a>>> { Ok(None) }
}

#[derive(Debug)]
pub enum Root<'a> {
	Include(S<Identifier<'a>>),
	UsingNamespace(S<Identifier<'a>>),
	Function(Function<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
	pub return_type: S<Type<'a>>,
	pub identifier: Identifier<'a>,
	pub parameters: Vec<(S<Identifier<'a>>, S<Type<'a>>)>,
	pub body: Vec<S<Statement<'a>>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
	Variable(S<Type<'a>>, Vec<(S<Identifier<'a>>, Vec<S<Expression<'a>>>)>),
	Conditional(S<Expression<'a>>, Box<S<Statement<'a>>>, Option<Box<S<Statement<'a>>>>),
	ForLoop((Box<S<Statement<'a>>>, S<Expression<'a>>, Box<S<Statement<'a>>>), Box<S<Statement<'a>>>),
	ForRange((S<Type<'a>>, S<Identifier<'a>>, S<Expression<'a>>), Box<S<Statement<'a>>>),
	DoWhile(Box<S<Statement<'a>>>, S<Expression<'a>>),
	While(S<Expression<'a>>, Box<S<Statement<'a>>>),
	Return(Option<S<Expression<'a>>>),
	Expression(S<Expression<'a>>),
	Scope(Vec<S<Statement<'a>>>),
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
	Variable(S<Path<'a>>),
	InitializerList(Vec<S<Expression<'a>>>),
	Construction(S<Type<'a>>, Vec<S<Expression<'a>>>),
	Unary(UnaryOperator, Box<S<Expression<'a>>>),
	PostUnary(PostUnaryOperator, Box<S<Expression<'a>>>),
	Binary(BinaryOperator, Box<S<Expression<'a>>>, Box<S<Expression<'a>>>),
	BinaryAssign(ValueOperator, Box<S<Expression<'a>>>, Box<S<Expression<'a>>>),
	Ternary(Box<S<Expression<'a>>>, Box<S<Expression<'a>>>, Box<S<Expression<'a>>>),
	Assign(Box<S<Expression<'a>>>, Box<S<Expression<'a>>>),
	Index(Box<S<Expression<'a>>>, Box<S<Expression<'a>>>),
	Field(Box<S<Expression<'a>>>, S<Identifier<'a>>),
	FunctionCall(S<Path<'a>>, Vec<S<Expression<'a>>>),
	MethodCall(Box<S<Expression<'a>>>, S<Identifier<'a>>, Vec<S<Expression<'a>>>),
	Dereference(Box<S<Expression<'a>>>),
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
