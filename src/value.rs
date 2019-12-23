use std::collections::HashMap;

use Value::*;

use crate::execute::{self, ExecutionContext, ExecutionResult, Field, Reference};
use crate::node::{BinaryOperator, Expression, PostUnaryOperator, Program,
	Type, UnaryOperator, ValueOperator};

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
	Float(f64),
	Integer(i128),
	Boolean(bool),
	Character(char),
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

	pub fn reference(self) -> Reference<'a> {
		match self {
			Value::Reference(reference) => reference,
			other => panic!("Cannot modify immutable value: {:?}", other),
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

/// Evaluates an expression. Prefers to return a reference when possible.
pub fn evaluate<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                        expression: &Expression<'a>) -> ExecutionResult<Value<'a>> {
	Ok(match expression {
		Expression::Float(float) => Float(*float),
		Expression::Integer(integer) => Integer(*integer),
		Expression::Boolean(boolean) => Boolean(*boolean),
		Expression::String(string) => String(string.to_string()),
		Expression::Character(character) => Character(*character),
		Expression::Variable(variable) => {
			let value = program.intrinsics.iter().map(|intrinsic| intrinsic
				.variable(program, context, variable)).find_map(Result::transpose);
			if let Some(value) = value { return value; }
			match context.variable(variable) {
				(_, Value::Reference(reference), _) => Value::Reference(reference.clone()),
				(frame, _, _) => Value::Reference(Reference(frame, variable.clone(), Vec::new())),
			}
		}
		Expression::InitializerList(expressions) => List(expressions.iter().map(|expression|
			concrete(program, context, expression)).collect::<Result<_, _>>()?),
		Expression::Construction(structure, arguments) =>
			construct(program, context, structure, arguments)?,
		Expression::Unary(operator, expression) => {
			let value = evaluate(program, context, expression)?;
			if let Value::Reference(reference) = &value {
				match (operator, context.dereference(reference)?) {
					(UnaryOperator::Increment, (Float(float), _)) =>
						return Ok(Float(*thread(float, |float| **float += 1.0))),
					(UnaryOperator::Decrement, (Float(float), _)) =>
						return Ok(Float(*thread(float, |float| **float -= 1.0))),
					(UnaryOperator::Increment, (Integer(integer), _)) =>
						return Ok(Integer(*thread(integer, |integer| **integer += 1))),
					(UnaryOperator::Decrement, (Integer(integer), _)) =>
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
		Expression::PostUnary(operator, expression) => {
			let reference = evaluate(program, context, expression)?.reference();
			match (operator, context.dereference(&reference)?) {
				(PostUnaryOperator::Increment, (Float(float), _)) =>
					Float(*thread(float, |float| **float += 1.0) - 1.0),
				(PostUnaryOperator::Decrement, (Float(float), _)) =>
					Float(*thread(float, |float| **float -= 1.0) + 1.0),
				(PostUnaryOperator::Increment, (Integer(integer), _)) =>
					Integer(*thread(integer, |integer| **integer += 1) - 1),
				(PostUnaryOperator::Decrement, (Integer(integer), _)) =>
					Integer(*thread(integer, |integer| **integer -= 1) + 1),
				(_, value) => panic!("Invalid operation: {:?}, on value: {:?}", operator, value),
			}
		}
		Expression::Binary(operator, left, right) => {
			let left = evaluate(program, context, left)?;
			let right = evaluate(program, context, right)?;
			let value = program.intrinsics.iter().map(|intrinsic| intrinsic.operation(program,
				context, operator.clone(), &left, &right)).find_map(Result::transpose);
			if let Some(value) = value { return value; }

			let left = context.concrete(left)?;
			match (operator, &left) {
				(BinaryOperator::LogicalOr, Boolean(true)) => return Ok(Boolean(true)),
				(BinaryOperator::LogicalAnd, Boolean(false)) => return Ok(Boolean(false)),
				_ => (),
			}

			// TODO: Implicit promotions
			match (operator.clone(), left, context.concrete(right)?) {
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
		Expression::BinaryAssign(operator, target, expression) => {
			let reference = evaluate(program, context, target)?.reference();
			let expression = concrete(program, context, expression)?;
			let (target, _) = context.dereference(&reference)?;
			*target = value_operator(operator.clone(), target.clone(), expression);
			target.clone()
		}
		Expression::Ternary(condition, branch, default) =>
			match concrete(program, context, condition)?.boolean() {
				true => return evaluate(program, context, branch),
				false => return evaluate(program, context, default),
			},
		Expression::Assign(target, expression) => {
			let reference = evaluate(program, context, target)?.reference();
			let expression = concrete(program, context, expression)?;
			let (target, _) = context.dereference(&reference)?;
			*target = expression;
			target.clone()
		}
		Expression::Index(target, _) =>
			panic!("Cannot index into: {:?}", evaluate(program, context, target)?),
		Expression::Field(expression, field) => match evaluate(program, context, expression)? {
			Value::Reference(Reference(frame, path, fields)) => Value::Reference(Reference(frame,
				path, thread(fields, |fields| fields.push(field.clone())))),
			Value::Structure(mut structure) => structure.fields.remove(field).map(|(value, _)|
				value).unwrap_or_else(|| panic!("Field: {}, does not exist on structure")),
			other => panic!("Cannot access field: {}, on value: {:?}", field, other),
		},
		Expression::FunctionCall(path, arguments) => {
			let arguments: Vec<_> = arguments.iter().map(|argument|
				evaluate(program, context, argument)).collect::<Result<_, _>>()?;
			let value = program.intrinsics.iter().map(|intrinsic| intrinsic.function(program,
				context, path, &arguments)).find_map(Result::transpose);
			if let Some(value) = value { return value; }

			let function = program.functions.get(path).and_then(|functions| functions.iter()
				.find(|function| function.parameters.len() == arguments.len())).unwrap_or_else(||
				panic!("No function: {}, exists with arity: {}", path, arguments.len()));
			let arguments = Iterator::zip(function.parameters.iter().cloned(), arguments)
				.map(|((_, structure), argument)| match structure {
					Type::Reference(_) => Ok(argument),
					_ => context.concrete(argument),
				}).collect::<Result<_, _>>()?;
			execute::function(program, context, function, arguments)?
		}
		Expression::MethodCall(target, method, arguments) => {
			let target = evaluate(program, context, target)?;
			let arguments: Vec<_> = arguments.iter().map(|argument|
				evaluate(program, context, argument)).collect::<Result<_, _>>()?;
			program.intrinsics.iter().map(|intrinsic| intrinsic.method(program, context,
				&target, &method, &arguments)).find_map(Result::transpose).unwrap_or_else(||
				panic!("Method: {}, does not exist for value: {:?}", method, target))?
		}
		Expression::Dereference(_) => panic!("Cannot dereference value that is not a pointer"),
	})
}

/// Constructs a typed object from provided arguments.
pub fn construct<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                         structure: &Type<'a>, arguments: &[Expression<'a>]) -> ExecutionResult<Value<'a>> {
	let mut arguments: Vec<_> = arguments.iter().map(|argument|
		parameter(program, context, structure, argument)).collect::<Result<_, _>>()?;
	match arguments.len() {
		1 => Ok(arguments.remove(0)),
		// TODO: Implement custom constructors
		_ => unimplemented!()
	}
}

/// Evaluates an expression and promotes any references to a concrete value.
pub fn concrete<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                        expression: &Expression<'a>) -> ExecutionResult<Value<'a>> {
	let value = evaluate(program, context, expression)?;
	context.concrete(value)
}

/// Evaluates an expression and promotes any references depending on the type.
pub fn parameter<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                         structure: &Type<'a>, expression: &Expression<'a>) -> ExecutionResult<Value<'a>> {
	match structure {
		Type::Reference(_) => evaluate(program, context, expression),
		_ => concrete(program, context, expression),
	}
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
