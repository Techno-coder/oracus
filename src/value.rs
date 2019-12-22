use std::collections::HashMap;

use Value::*;

use crate::execute::{self, ExecutionContext, ExecutionError, ExecutionResult, Field, Reference};
use crate::node::{BinaryOperator, Expression, PostUnaryOperator, ProgramContext,
	Type, UnaryOperator, ValueOperator};

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
	Float(f64),
	Integer(i128),
	Boolean(bool),
	Character(char),
	String(std::string::String),
	List(Vec<Value<'a>>),
	Array(Vec<Value<'a>>),
	Vector(Vec<Value<'a>>),
	Reference(Reference<'a>),
	Structure(HashMap<Field<'a>, Value<'a>>),
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
}

pub fn evaluate<'a>(program: &ProgramContext<'a>, context: &mut ExecutionContext<'a>,
                    expression: &Expression<'a>) -> ExecutionResult<Value<'a>> {
	Ok(match expression {
		Expression::Float(float) => Float(*float),
		Expression::Integer(integer) => Integer(*integer),
		Expression::Boolean(boolean) => Boolean(*boolean),
		Expression::String(string) => String(string.to_string()),
		Expression::Character(character) => Character(*character),
		Expression::Variable(variable) => match context.variable(variable) {
			(_, Value::Reference(reference)) => Value::Reference(reference.clone()),
			(frame, _) => Value::Reference(Reference(frame, variable.clone(), Vec::new())),
		},
		Expression::InitializerList(expressions) => List(expressions.iter().map(|expression|
			concrete(program, context, expression)).collect::<Result<_, _>>()?),
		Expression::Construction(structure, arguments) =>
			construct(program, context, structure, arguments)?,
		Expression::Unary(operator, expression) => {
			let value = evaluate(program, context, expression)?;
			if let Value::Reference(reference) = &value {
				match (operator, context.dereference(reference)?) {
					(UnaryOperator::Increment, Float(float)) =>
						return Ok(Float(*thread(float, |float| **float += 1.0))),
					(UnaryOperator::Decrement, Float(float)) =>
						return Ok(Float(*thread(float, |float| **float -= 1.0))),
					(UnaryOperator::Increment, Integer(integer)) =>
						return Ok(Integer(*thread(integer, |integer| **integer += 1))),
					(UnaryOperator::Decrement, Integer(integer)) =>
						return Ok(Integer(*thread(integer, |integer| **integer -= 1))),
					_ => (),
				}
			}

			match (&operator, context.concrete(value)?) {
				(UnaryOperator::Minus, Float(float)) => Float(-float),
				(UnaryOperator::Minus, Integer(integer)) => Integer(-integer),
				(UnaryOperator::Not, Boolean(boolean)) => Boolean(!boolean),
				(_, value) => panic!("Invalid operation: {:?}, on value: {:?}", operator, value),
			}
		}
		Expression::PostUnary(operator, expression) =>
			match evaluate(program, context, expression)? {
				Value::Reference(reference) => match (operator, context.dereference(&reference)?) {
					(PostUnaryOperator::Increment, Float(float)) =>
						Float(*thread(float, |float| **float += 1.0) - 1.0),
					(PostUnaryOperator::Decrement, Float(float)) =>
						Float(*thread(float, |float| **float -= 1.0) + 1.0),
					(PostUnaryOperator::Increment, Integer(integer)) =>
						Integer(*thread(integer, |integer| **integer += 1) - 1),
					(PostUnaryOperator::Decrement, Integer(integer)) =>
						Integer(*thread(integer, |integer| **integer -= 1) + 1),
					(_, value) => panic!("Invalid operation: {:?}, on value: {:?}", operator, value),
				},
				other => panic!("Cannot modify immutable value: {:?}", other),
			},
		Expression::Binary(operator, left, right) => {
			let left = concrete(program, context, left)?;
			match (operator, &left) {
				(BinaryOperator::LogicalOr, Boolean(true)) => return Ok(Boolean(true)),
				(BinaryOperator::LogicalAnd, Boolean(false)) => return Ok(Boolean(false)),
				_ => (),
			}

			// TODO: Implicit promotions
			match (operator.clone(), left, concrete(program, context, right)?) {
				(BinaryOperator::Equal, left, right) => Boolean(equal(left, right)),
				(BinaryOperator::NotEqual, left, right) => Boolean(!equal(left, right)),
				(BinaryOperator::LogicalOr, Boolean(left), Boolean(right)) => Boolean(left || right),
				(BinaryOperator::LogicalAnd, Boolean(left), Boolean(right)) => Boolean(left && right),
				(BinaryOperator::LessThan, Float(left), Float(right)) => Boolean(left < right),
				(BinaryOperator::LessEqual, Float(left), Float(right)) => Boolean(left <= right),
				(BinaryOperator::GreaterThan, Float(left), Float(right)) => Boolean(left > right),
				(BinaryOperator::GreaterEqual, Float(left), Float(right)) => Boolean(left >= right),
				(BinaryOperator::LessThan, Integer(left), Integer(right)) => Boolean(left < right),
				(BinaryOperator::LessEqual, Integer(left), Integer(right)) => Boolean(left <= right),
				(BinaryOperator::GreaterThan, Integer(left), Integer(right)) => Boolean(left > right),
				(BinaryOperator::GreaterEqual, Integer(left), Integer(right)) => Boolean(left >= right),
				(BinaryOperator::Value(operator), left, right) => value_operator(operator, left, right),
				(operator, left, right) => panic!("Invalid operation: {:?}, on values: {:?}, and: {:?}",
					operator, left, right),
			}
		}
		Expression::BinaryAssign(operator, target, expression) =>
			match evaluate(program, context, target)? {
				Value::Reference(reference) => {
					let expression = concrete(program, context, expression)?;
					let target = context.dereference(&reference)?;
					*target = value_operator(operator.clone(), target.clone(), expression);
					target.clone()
				}
				other => panic!("Cannot modify immutable value: {:?}", other),
			},
		Expression::Ternary(condition, branch, default) =>
			match concrete(program, context, condition)?.boolean() {
				true => return evaluate(program, context, branch),
				false => return evaluate(program, context, default),
			},
		Expression::Assign(target, expression) => match evaluate(program, context, target)? {
			Value::Reference(reference) => {
				let expression = concrete(program, context, expression)?;
				let target = context.dereference(&reference)?;
				*target = expression;
				target.clone()
			}
			other => panic!("Cannot modify immutable value: {:?}", other),
		},
		Expression::Index(target, index) =>
			match (concrete(program, context, target)?, concrete(program, context, index)?) {
				(Array(array), Integer(index)) => array.get(index as usize).cloned(),
				(Vector(vector), Integer(index)) => vector.get(index as usize).cloned(),
				(target, index) => panic!("Cannot index into: {:?}, with: {:?}", target, index),
			}.ok_or(ExecutionError::IndexBounds)?,
		Expression::Field(expression, field) => match evaluate(program, context, expression)? {
			Value::Reference(Reference(frame, path, fields)) => Value::Reference(Reference(frame,
				path, thread(fields, |fields| fields.push(field.clone())))),
			Value::Structure(mut structure) => structure.remove(field)
				.unwrap_or_else(|| panic!("Field: {}, does not exist on structure")),
			other => panic!("Cannot access field: {}, on value: {:?}", field, other),
		},
		Expression::FunctionCall(path, arguments) => {
			// TODO: Implement intrinsic function calls
			let function = program.functions.get(path).and_then(|functions| functions.iter()
				.find(|function| function.parameters.len() == arguments.len())).unwrap_or_else(||
				panic!("No function: {}, exists with arity: {}", path, arguments.len()));
			let arguments = Iterator::zip(function.parameters.iter(), arguments.iter())
				.map(|((_, structure), argument)| match structure {
					Type::Reference(_) => evaluate(program, context, argument),
					_ => concrete(program, context, argument),
				}).collect::<Result<_, _>>()?;
			execute::function(program, context, function, arguments)?
		}
		// TODO: Implement method calls
		Expression::MethodCall(_, _, _) => unimplemented!(),
		Expression::Dereference(_) => panic!("Cannot dereference value that is not a pointer"),
	})
}

pub fn construct<'a>(program: &ProgramContext<'a>, context: &mut ExecutionContext<'a>,
                     _: &Type<'a>, arguments: &[Expression<'a>]) -> ExecutionResult<Value<'a>> {
	let mut arguments: Vec<_> = arguments.iter().map(|argument|
		concrete(program, context, argument)).collect::<Result<_, _>>()?;
	match arguments.len() {
		1 => Ok(arguments.remove(0)),
		// TODO: Implement custom constructors
		_ => unimplemented!()
	}
}

pub fn concrete<'a>(program: &ProgramContext<'a>, context: &mut ExecutionContext<'a>,
                    expression: &Expression<'a>) -> ExecutionResult<Value<'a>> {
	let value = evaluate(program, context, expression)?;
	context.concrete(value)
}

fn thread<F, T>(mut value: T, function: F) -> T where F: FnOnce(&mut T) {
	function(&mut value);
	value
}

fn equal(left: Value, right: Value) -> bool {
	match (left, right) {
		(Float(left), Float(right)) => left == right,
		(Integer(left), Integer(right)) => left == right,
		(Boolean(left), Boolean(right)) => left == right,
		(Character(left), Character(right)) => left == right,
		(String(left), String(right)) => left == right,
		(left, right) => panic!("Cannot equate values: {:?}, and {:?}", left, right),
	}
}

fn value_operator<'a>(operator: ValueOperator, left: Value<'a>, right: Value<'a>) -> Value<'a> {
	match (operator, left, right) {
		(ValueOperator::Add, Float(left), Float(right)) => Float(left + right),
		(ValueOperator::Minus, Float(left), Float(right)) => Float(left - right),
		(ValueOperator::Multiply, Float(left), Float(right)) => Float(left * right),
		(ValueOperator::Divide, Float(left), Float(right)) => Float(left / right),
		(ValueOperator::Modulo, Float(left), Float(right)) => Float(left % right),
		(ValueOperator::Add, Integer(left), Integer(right)) => Integer(left + right),
		(ValueOperator::Minus, Integer(left), Integer(right)) => Integer(left - right),
		(ValueOperator::Multiply, Integer(left), Integer(right)) => Integer(left * right),
		(ValueOperator::Divide, Integer(left), Integer(right)) => Integer(left / right),
		(ValueOperator::Modulo, Integer(left), Integer(right)) => Integer(left % right),
		(ValueOperator::And, Integer(left), Integer(right)) => Integer(left & right),
		(ValueOperator::Or, Integer(left), Integer(right)) => Integer(left | right),
		(ValueOperator::ShiftLeft, Integer(left), Integer(right)) => Integer(left << right),
		(ValueOperator::ShiftRight, Integer(left), Integer(right)) => Integer(left >> right),
		(operator, left, right) => panic!("Invalid operation: {:?}, on values: {:?}, and: {:?}",
			operator, left, right),
	}
}
