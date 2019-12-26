use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use Value::*;

use crate::execute::{self, ExecutionContext, ExecutionError, ExecutionResult, Field, Reference};
use crate::node::{Expression, Identifier, IntegralKind, IntegralRank, Program, Type};
use crate::operation;
use crate::span::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
	Float(f64),
	Boolean(bool),
	Integer(IntegralKind, i128),
	String(std::string::String),
	List(Vec<Value<'a>>),
	Reference(Reference<'a>),
	Structure(Structure<'a>),
	Uninitialised,
	Void,
}

impl<'a> Value<'a> {
	pub fn boolean(self) -> bool {
		match self {
			Boolean(boolean) => boolean,
			other => panic!("Condition: {:?}, must be boolean", other),
		}
	}

	pub fn reference(self, span: Span) -> Spanned<Reference<'a>> {
		match self {
			Value::Reference(reference) => Spanned::new(reference, span),
			other => panic!("Cannot modify immutable value: {:?}", other),
		}
	}

	pub fn structure(&mut self) -> &mut Structure<'a> {
		match self {
			Value::Structure(structure) => structure,
			other => panic!("Value: {:?}, is not structure", other),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Structure<'a> {
	pub structure: Type<'a>,
	pub fields: HashMap<Field<'a>, (Value<'a>, Type<'a>)>,
}

impl<'a> Structure<'a> {
	pub fn new(structure: Type<'a>) -> Self {
		Self { structure, fields: HashMap::new() }
	}
}

impl<'a> Index<&Identifier<'a>> for Structure<'a> {
	type Output = (Value<'a>, Type<'a>);

	fn index(&self, index: &Identifier<'a>) -> &Self::Output {
		self.fields.get(index).unwrap_or_else(|| panic!("Field: {}, does not exist", index))
	}
}

impl<'a> IndexMut<&Identifier<'a>> for Structure<'a> {
	fn index_mut(&mut self, index: &Identifier<'a>) -> &mut Self::Output {
		self.fields.get_mut(index).unwrap_or_else(|| panic!("Field: {}, does not exist", index))
	}
}

/// Evaluates an expression. Prefers to return a reference when possible.
pub fn evaluate<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                        expression: &Spanned<Expression<'a>>) -> ExecutionResult<Value<'a>> {
	let span = expression.span;
	Ok(match &expression.node {
		Expression::Float(float) => Float(*float),
		Expression::Integer(integer) =>
			Integer(IntegralKind(false, IntegralRank::Unknown), *integer),
		Expression::Boolean(boolean) => Boolean(*boolean),
		Expression::String(string) => String(string.to_string()),
		Expression::Character(character) => Integer(IntegralKind(false,
			IntegralRank::Byte), *character as u8 as i128),
		Expression::Variable(variable) => {
			let value = program.intrinsics.iter().map(|intrinsic| intrinsic
				.variable(program, context, variable)).find_map(Result::transpose);
			if let Some(value) = value { return value; }
			Value::Reference(match context.variable(&variable.node) {
				(_, Value::Reference(reference), _) => reference.clone(),
				(frame, _, _) => Reference(frame, variable.node.clone(), Vec::new()),
			})
		}
		Expression::InitializerList(expressions) => expressions.iter().map(|expression|
			concrete(program, context, expression)).collect::<Result<_, _>>().map(List)?,
		Expression::Construction(structure, arguments) =>
			construct(program, context, structure, arguments)?,
		Expression::Unary(operator, expression) =>
			operation::unary(program, context, &operator, expression)?,
		Expression::PostUnary(operator, expression) =>
			operation::post_unary(program, context, operator, &expression)?,
		Expression::Binary(operator, left, right) => operation::binary(program,
			context, expression, operator, left, right, span)?,
		Expression::BinaryAssign(operator, target, expression) =>
			operation::binary_assign(program, context, span, operator, target, expression)?,
		Expression::Ternary(condition, branch, default) =>
			match concrete(program, context, condition)?.boolean() {
				true => evaluate(program, context, branch),
				false => evaluate(program, context, default),
			}?,
		Expression::Assign(target, expression) => {
			let reference = evaluate(program, context, target)?.reference(target.span);
			let mut expression = Spanned::new(concrete(program, context, expression)?, expression.span);
			let value = program.intrinsics.iter().map(|intrinsic| intrinsic.assign(program,
				context, &reference, &expression)).find_map(Result::transpose);
			if let Some(value) = value { return value; }

			let (target, _) = context.dereference(&reference)?;
			match (&target, &mut expression.node) {
				(Float(_), Integer(_, value)) =>
					expression.node = Float(*value as f64),
				(Integer(kind, _), Float(value)) => expression.node =
					Integer(kind.clone(), numeric(kind, *value as i128, expression.span)?),
				(Integer(kind, _), Integer(other, value)) => {
					*other = kind.clone();
					numeric(other, *value, expression.span)?;
				}
				_ => (),
			}

			*target = expression.node;
			target.clone()
		}
		Expression::Index(target, _) =>
			panic!("Cannot index into: {:?}", evaluate(program, context, target)?),
		Expression::Field(expression, field) => match evaluate(program, context, expression)? {
			Value::Reference(Reference(frame, path, fields)) => Value::Reference(Reference(frame,
				path, thread(fields, |fields| fields.push(field.node.clone())))),
			Value::Structure(mut structure) => structure.fields.remove(&field.node).map(|(value, _)|
				value).unwrap_or_else(|| panic!("Field: {}, does not exist on structure")),
			other => panic!("Cannot access field: {}, on value: {:?}", field.node, other),
		},
		Expression::FunctionCall(path, arguments) => {
			let arguments = arguments.iter().map(|argument| Ok(Spanned::new(evaluate(program,
				context, argument)?, argument.span))).collect::<Result<Vec<_>, _>>()?;
			let value = program.intrinsics.iter().map(|intrinsic| intrinsic.function(program,
				context, path, &arguments)).find_map(Result::transpose);
			if let Some(value) = value { return value; }

			let function = program.functions.get(&path.node).and_then(|functions| functions.iter()
				.find(|function| function.parameters.len() == arguments.len())).unwrap_or_else(||
				panic!("No function: {}, exists with arity: {}", path.node, arguments.len()));
			let arguments = Iterator::zip(function.parameters.iter().cloned(), arguments)
				.map(|((_, structure), argument)| match structure.node {
					Type::Reference(_) => Ok(argument.node),
					_ => context.concrete(argument),
				}).collect::<Result<_, _>>()?;
			execute::function(program, context, function, arguments)?
		}
		Expression::MethodCall(target, method, arguments) => {
			let target = Spanned::new(evaluate(program, context, target)?, target.span);
			let arguments = arguments.iter().map(|argument| Ok(Spanned::new(evaluate(program,
				context, argument)?, argument.span))).collect::<Result<Vec<_>, _>>()?;
			program.intrinsics.iter().map(|intrinsic| intrinsic.method(program, context,
				&target, method, &arguments)).find_map(Result::transpose).unwrap_or_else(||
				panic!("Method: {}, does not exist for value: {:?}", method.node, target))?
		}
		Expression::Dereference(_) => panic!("Cannot dereference value that is not a pointer"),
	})
}

/// Constructs a typed object from provided arguments.
pub fn construct<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                         structure: &Spanned<Type<'a>>, arguments: &[Spanned<Expression<'a>>])
                         -> ExecutionResult<Value<'a>> {
	let mut arguments: Vec<_> = arguments.iter().map(|argument|
		parameter(program, context, structure, argument)).collect::<Result<_, _>>()?;
	let value = program.intrinsics.iter().map(|intrinsic| intrinsic.construct(program,
		context, structure, &arguments)).find_map(Result::transpose);
	if let Some(value) = value { return value; }
	match arguments.len() {
		0 => Ok(Value::Uninitialised),
		1 => {
			let mut value = arguments.remove(0);
			match (&structure.node, &mut value) {
				(Type::Integral(target), Integer(kind, integer)) => {
					*kind = target.clone();
					numeric(kind, *integer, structure.span)?;
				}
				(Type::Integral(kind), Float(float)) => value =
					Integer(kind.clone(), numeric(kind, *float as i128, structure.span)?),
				(_, Integer(_, integer)) if structure.node ==
					Type::single(["float"].into()) => value = Float(*integer as f64),
				_ => (),
			}
			Ok(value)
		}
		_ => panic!("No valid construction for type: {}", structure.node),
	}
}

/// Evaluates an expression and promotes any references to a concrete value.
pub fn concrete<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                        expression: &Spanned<Expression<'a>>) -> ExecutionResult<Value<'a>> {
	let value = evaluate(program, context, expression)?;
	context.concrete(Spanned::new(value, expression.span))
}

/// Evaluates an expression and promotes any references depending on the type.
pub fn parameter<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                         structure: &Spanned<Type<'a>>, expression: &Spanned<Expression<'a>>)
                         -> ExecutionResult<Value<'a>> {
	match structure.node {
		Type::Reference(_) => evaluate(program, context, expression),
		_ => concrete(program, context, expression),
	}
}

pub fn numeric(IntegralKind(unsigned, rank): &IntegralKind, integer: i128,
               span: Span) -> ExecutionResult<i128> {
	let bounds = match (unsigned, rank) {
		(&false, &IntegralRank::Byte) => i8::min_value() as i128..=i8::max_value() as i128,
		(&false, &IntegralRank::Short) => i16::min_value() as i128..=i16::max_value() as i128,
		(&false, &IntegralRank::Integer) => i32::min_value() as i128..=i32::max_value() as i128,
		(&false, &IntegralRank::LongLong) => i64::min_value() as i128..=i64::max_value() as i128,
		(&true, &IntegralRank::Byte) => 0..=u8::max_value() as i128,
		(&true, &IntegralRank::Short) => 0..=u16::max_value() as i128,
		(&true, &IntegralRank::Integer) => 0..=u32::max_value() as i128,
		(&true, &IntegralRank::LongLong) => 0..=u64::max_value() as i128,
		(_, &IntegralRank::Unknown) => i128::min_value()..=i128::max_value(),
	};

	match bounds.contains(&integer) {
		false => Err(Spanned::new(ExecutionError::IntegralOverflow, span)),
		true => Ok(integer),
	}
}

pub fn thread<F, T>(mut value: T, function: F) -> T where F: FnOnce(&mut T) {
	function(&mut value);
	value
}
