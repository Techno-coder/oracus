use crate::execute::{ExecutionContext, ExecutionResult};
use crate::node::{BinaryOperator, Expression, IntegralKind,
	PostUnaryOperator, Program, UnaryOperator, ValueOperator};
use crate::span::{Span, Spanned};
use crate::value::{self, Value::*, Value};

pub fn unary<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                     operator: &UnaryOperator, expression: &Spanned<Expression<'a>>)
                     -> ExecutionResult<Value<'a>> {
	let value = value::evaluate(program, context, expression)?;
	if let Value::Reference(reference) = &value {
		let reference = Spanned::new(reference.clone(), expression.span);
		match (operator, context.dereference(&reference)?) {
			(UnaryOperator::Increment, (Float(float), _)) =>
				return Ok(Float(*value::thread(float, |float| **float += 1.0))),
			(UnaryOperator::Decrement, (Float(float), _)) =>
				return Ok(Float(*value::thread(float, |float| **float -= 1.0))),
			(UnaryOperator::Increment, (Integer(kind, integer), _)) =>
				return Ok(Integer(kind.clone(), value::numeric(&kind,
					*value::thread(integer, |integer| **integer += 1), expression.span)?)),
			(UnaryOperator::Decrement, (Integer(kind, integer), _)) =>
				return Ok(Integer(kind.clone(), value::numeric(&kind,
					*value::thread(integer, |integer| **integer -= 1), expression.span)?)),
			_ => (),
		}
	}

	let value = Spanned::new(value, expression.span);
	Ok(match (&operator, context.concrete(value)?) {
		(UnaryOperator::Minus, Float(float)) => Float(-float),
		(UnaryOperator::Minus, Integer(kind, integer)) => Integer(kind.clone(),
			value::numeric(&kind, -integer, expression.span)?),
		(UnaryOperator::Not, Boolean(boolean)) => Boolean(!boolean),
		(_, value) => panic!("Invalid operation: {:?}, on value: {:?}", operator, value),
	})
}

pub fn post_unary<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                          operator: &PostUnaryOperator, expression: &Spanned<Expression<'a>>)
                          -> ExecutionResult<Value<'a>> {
	let value = value::evaluate(program, context, expression)?;
	Ok(match (operator, context.dereference(&value.reference(expression.span))?) {
		(PostUnaryOperator::Increment, (Float(float), _)) =>
			Float(*value::thread(float, |float| **float += 1.0) - 1.0),
		(PostUnaryOperator::Decrement, (Float(float), _)) =>
			Float(*value::thread(float, |float| **float -= 1.0) + 1.0),
		(PostUnaryOperator::Increment, (Integer(kind, integer), _)) =>
			Integer(kind.clone(), value::numeric(&kind, *value::thread(integer,
				|integer| **integer += 1), expression.span)? - 1),
		(PostUnaryOperator::Decrement, (Integer(kind, integer), _)) =>
			Integer(kind.clone(), value::numeric(&kind, *value::thread(integer,
				|integer| **integer -= 1), expression.span)? + 1),
		(_, value) => panic!("Invalid operation: {:?}, on value: {:?}", operator, value),
	})
}

pub fn binary<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                      expression: &Spanned<Expression<'a>>, operator: &BinaryOperator,
                      left: &Spanned<Expression<'a>>, right: &Spanned<Expression<'a>>,
                      span: Span) -> ExecutionResult<Value<'a>> {
	let left = Spanned::new(value::evaluate(program, context, left)?, left.span);
	let right = Spanned::new(value::evaluate(program, context, right)?, right.span);
	let value = program.intrinsics.iter().map(|intrinsic| intrinsic.operation(program,
		context, operator.clone(), &left, &right)).find_map(Result::transpose);
	if let Some(value) = value { return value; }

	let mut left = context.concrete(left)?;
	match (operator, &left) {
		(BinaryOperator::LogicalOr, Boolean(true)) => return Ok(Boolean(true)),
		(BinaryOperator::LogicalAnd, Boolean(false)) => return Ok(Boolean(false)),
		_ => (),
	}

	let mut right = context.concrete(right)?;
	promote(&mut left, &mut right, expression.span)?;
	Ok(match (operator.clone(), left, right) {
		(BinaryOperator::Equal, left, right) => Boolean(equal(left, right)),
		(BinaryOperator::NotEqual, left, right) => Boolean(!equal(left, right)),
		(BinaryOperator::LogicalOr, Boolean(left), Boolean(right)) => Boolean(left || right),
		(BinaryOperator::LogicalAnd, Boolean(left), Boolean(right)) => Boolean(left && right),
		(BinaryOperator::LessThan, Float(left), Float(right)) => Boolean(left < right),
		(BinaryOperator::LessEqual, Float(left), Float(right)) => Boolean(left <= right),
		(BinaryOperator::GreaterThan, Float(left), Float(right)) => Boolean(left > right),
		(BinaryOperator::GreaterEqual, Float(left), Float(right)) => Boolean(left >= right),
		(BinaryOperator::LessThan, Integer(_, left), Integer(_, right)) => Boolean(left < right),
		(BinaryOperator::LessEqual, Integer(_, left), Integer(_, right)) => Boolean(left <= right),
		(BinaryOperator::GreaterThan, Integer(_, left), Integer(_, right)) => Boolean(left > right),
		(BinaryOperator::GreaterEqual, Integer(_, left), Integer(_, right)) => Boolean(left >= right),
		(BinaryOperator::Value(operator), left, right) => value_operator(operator, left, right, span)?,
		(operator, left, right) => panic!("Invalid operation: {:?}, on values: {:?}, and: {:?}",
			operator, left, right),
	})
}

pub fn binary_assign<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                             span: Span, operator: &ValueOperator, target: &Spanned<Expression<'a>>,
                             expression: &Spanned<Expression<'a>>) -> ExecutionResult<Value<'a>> {
	let reference = value::evaluate(program, context, target)?.reference(target.span);
	let mut expression = value::concrete(program, context, expression)?;
	let (target, _) = context.dereference(&reference)?;
	if let (Float(_), Integer(_, value)) = (&target, &expression) {
		expression = Float(*value as f64);
	}

	*target = value_operator(operator.clone(), target.clone(), expression, span)?;
	Ok(target.clone())
}

fn promote(left: &mut Value, right: &mut Value, span: Span) -> ExecutionResult<()> {
	if let Integer(IntegralKind(left_unsigned, left_rank), left) = left {
		if let Integer(IntegralKind(right_unsigned, right_rank), right) = right {
			match left_rank == right_rank {
				true => if left_unsigned != right_unsigned {
					*left_unsigned = true;
					*right_unsigned = true;
				}
				false => {
					*left_rank = Ord::max(left_rank.clone(), right_rank.clone());
					*right_rank = left_rank.clone();
					*left_unsigned = false;
					*right_unsigned = false;
				}
			}

			*left = value::numeric(&IntegralKind(*left_unsigned,
				left_rank.clone()), *left, span)?;
			*right = value::numeric(&IntegralKind(*right_unsigned,
				right_rank.clone()), *right, span)?;
		}
	}

	Ok(match (&left, &right) {
		(Float(_), Integer(_, integer)) => *right = Float(*integer as f64),
		(Integer(_, integer), Float(_)) => *left = Float(*integer as f64),
		_ => (),
	})
}

fn value_operator<'a>(operator: ValueOperator, left: Value<'a>,
                      right: Value<'a>, span: Span) -> ExecutionResult<Value<'a>> {
	use ValueOperator::*;
	numeric_value(match (operator, left, right) {
		(Add, Float(left), Float(right)) => Float(left + right),
		(Minus, Float(left), Float(right)) => Float(left - right),
		(Multiply, Float(left), Float(right)) => Float(left * right),
		(Divide, Float(left), Float(right)) => Float(left / right),
		(Modulo, Float(left), Float(right)) => Float(left % right),
		(Add, Integer(kind, left), Integer(_, right)) => Integer(kind, left + right),
		(Minus, Integer(kind, left), Integer(_, right)) => Integer(kind, left - right),
		(Multiply, Integer(kind, left), Integer(_, right)) => Integer(kind, left * right),
		(Divide, Integer(kind, left), Integer(_, right)) => Integer(kind, left / right),
		(Modulo, Integer(kind, left), Integer(_, right)) => Integer(kind, left % right),
		(And, Integer(kind, left), Integer(_, right)) => Integer(kind, left & right),
		(Or, Integer(kind, left), Integer(_, right)) => Integer(kind, left | right),
		(ShiftLeft, Integer(kind, left), Integer(_, right)) => Integer(kind, left << right),
		(ShiftRight, Integer(kind, left), Integer(_, right)) => Integer(kind, left >> right),
		(operator, left, right) => panic!("Invalid operation: {:?}, on values: {:?}, and: {:?}",
			operator, left, right),
	}, span)
}

fn equal(left: Value, right: Value) -> bool {
	match (left, right) {
		(Float(left), Float(right)) => left == right,
		(Integer(_, left), Integer(_, right)) => left == right,
		(Boolean(left), Boolean(right)) => left == right,
		(String(left), String(right)) => left == right,
		(left, right) => panic!("Cannot equate values: {:?}, and {:?}", left, right),
	}
}

fn numeric_value(value: Value, span: Span) -> ExecutionResult<Value> {
	if let Integer(kind, integer) = &value {
		value::numeric(kind, *integer, span)?;
	}
	Ok(value)
}
