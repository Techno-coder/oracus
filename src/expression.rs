use crate::lexer::{Lexer, Token};
use crate::node::{BinaryOperator, Expression, PostUnaryOperator, UnaryOperator, ValueOperator};
use crate::parser::{self, ParserError, ParserResult};
use crate::span::Spanned;
use crate::symbol::SymbolContext;

pub fn expression_root<'a>(context: &SymbolContext<'a>, lexer: &mut Lexer<'a>)
                           -> ParserResult<Spanned<Expression<'a>>> {
	expression(context, lexer, 0)
}

fn expression<'a>(context: &SymbolContext<'a>, lexer: &mut Lexer<'a>,
                  precedence: usize) -> ParserResult<Spanned<Expression<'a>>> {
	let mut left = terminal(context, lexer)?;
	while self::precedence(&lexer.peek().node) > precedence {
		let (span, node) = (left.span, binder(context, lexer, left)?);
		left = Spanned::new(node, span.extend(lexer.previous()));
	}
	Ok(left)
}

fn precedence(token: &Token) -> usize {
	match token {
		Token::Increment | Token::Decrement |
		Token::SquareOpen | Token::Access | Token::DereferenceAccess => 12,
		Token::Asterisk | Token::Divide | Token::Modulo => 11,
		Token::Add | Token::Minus => 10,
		Token::ShiftLeft => 9,
		Token::AngleLeft | Token::AngleRight |
		Token::LessEqual | Token::GreaterEqual => 8,
		Token::Equal | Token::NotEqual => 7,
		Token::Ampersand => 6,
		Token::ExclusiveOr => 5,
		Token::Or => 4,
		Token::LogicalAnd => 3,
		Token::LogicalOr => 2,
		Token::Ternary | Token::Assign | Token::Assignment(_) => 1,
		_ => 0,
	}
}

fn binder<'a>(context: &SymbolContext<'a>, lexer: &mut Lexer<'a>,
              mut left: Spanned<Expression<'a>>) -> ParserResult<Expression<'a>> {
	let binder = lexer.next().node;
	let precedence = precedence(&binder);

	match binder {
		Token::Ternary => {
			let branch = Box::new(expression_root(context, lexer)?);
			parser::expect(lexer, Token::Separator)?;
			let default = Box::new(expression_root(context, lexer)?);
			return Ok(Expression::Ternary(Box::new(left), branch, default));
		}
		Token::Assign => return Ok(Expression::Assign(Box::new(left),
			Box::new(expression(context, lexer, precedence - 1)?))),
		Token::Assignment(operator) => return Ok(Expression::BinaryAssign(operator,
			Box::new(left), Box::new(expression(context, lexer, precedence - 1)?))),
		Token::SquareOpen => {
			let index = Box::new(expression_root(context, lexer)?);
			parser::expect(lexer, Token::SquareClose)?;
			return Ok(Expression::Index(Box::new(left), index));
		}
		Token::Access | Token::DereferenceAccess => {
			let access = parser::identifier(lexer)?;
			if binder == Token::DereferenceAccess {
				left = left.wrap(Expression::Dereference);
			}

			return Ok(match lexer.peek().node {
				Token::BracketOpen => Expression::MethodCall(Box::new(left),
					access, arguments(context, lexer)?),
				_ => Expression::Field(Box::new(left), access),
			});
		}
		Token::Increment => return Ok(PostUnaryOperator::Increment)
			.map(|operator| Expression::PostUnary(operator, Box::new(left))),
		Token::Decrement => return Ok(PostUnaryOperator::Decrement)
			.map(|operator| Expression::PostUnary(operator, Box::new(left))),
		Token::ShiftLeft => return match lexer.peek().node {
			Token::Assign => lexer.thread(Ok(ValueOperator::ShiftLeft))
				.and_then(|operator| Ok(Expression::BinaryAssign(operator, Box::new(left),
					Box::new(expression(context, lexer, precedence)?)))),
			_ => Ok(Expression::Binary(BinaryOperator::Value(ValueOperator::ShiftLeft),
				Box::new(left), Box::new(expression(context, lexer, precedence)?)))
		},
		Token::AngleRight => return match lexer.peek().node {
			Token::AngleRight => lexer.thread(Ok(BinaryOperator::Value(ValueOperator::ShiftRight)))
				.and_then(|operator| Ok(Expression::Binary(operator, Box::new(left),
					Box::new(expression(context, lexer, precedence)?)))),
			Token::GreaterEqual => lexer.thread(Ok(ValueOperator::ShiftRight))
				.and_then(|operator| Ok(Expression::BinaryAssign(operator, Box::new(left),
					Box::new(expression(context, lexer, precedence)?)))),
			_ => Ok(Expression::Binary(BinaryOperator::GreaterThan, Box::new(left),
				Box::new(expression(context, lexer, precedence)?)))
		},
		_ => (),
	}

	let right = expression(context, lexer, precedence)?;
	let operator = match binder {
		Token::Asterisk => BinaryOperator::Value(ValueOperator::Multiply),
		Token::Divide => BinaryOperator::Value(ValueOperator::Divide),
		Token::Modulo => BinaryOperator::Value(ValueOperator::Modulo),
		Token::Add => BinaryOperator::Value(ValueOperator::Add),
		Token::Minus => BinaryOperator::Value(ValueOperator::Minus),
		Token::AngleLeft => BinaryOperator::LessThan,
		Token::LessEqual => BinaryOperator::LessEqual,
		Token::GreaterEqual => BinaryOperator::GreaterEqual,
		Token::Equal => BinaryOperator::Equal,
		Token::NotEqual => BinaryOperator::NotEqual,
		Token::Ampersand => BinaryOperator::Value(ValueOperator::And),
		Token::LogicalAnd => BinaryOperator::LogicalAnd,
		Token::LogicalOr => BinaryOperator::LogicalOr,
		other => panic!("Invalid binder: {:?}", other),
	};

	Ok(Expression::Binary(operator, Box::new(left), Box::new(right)))
}

fn terminal<'a>(context: &SymbolContext<'a>, lexer: &mut Lexer<'a>)
                -> ParserResult<Spanned<Expression<'a>>> {
	if let Token::Identifier(_) = lexer.peek().node {
		let recovery = lexer.clone();
		let path = parser::path(lexer)?;
		let span = path.span;

		if let Some(variable) = context.resolve_variable(&path.node) {
			return Ok(Spanned::new(Expression::Variable(path.map(|_| variable)), span));
		} else if let Some(function) = context.resolve_function(&path.node) {
			let (path, arguments) = (path.map(|_| function), arguments(context, lexer)?);
			return Ok(Spanned::new(Expression::FunctionCall(path, arguments), span));
		} else if context.resolve_structure(&path.node).is_some() {
			*lexer = recovery;
			let structure = parser::parse_type(lexer)?;
			let arguments = arguments(context, lexer)?;
			let expression = Expression::Construction(structure, arguments);
			return Ok(Spanned::new(expression, span));
		} else {
			return Err(Spanned::new(ParserError::UndefinedPath, span));
		}
	}

	let token = lexer.next();
	Ok(match token.node {
		Token::Float(float) => Spanned::new(Expression::Float(float), token.span),
		Token::Integer(integer) => Spanned::new(Expression::Integer(integer), token.span),
		Token::Boolean(boolean) => Spanned::new(Expression::Boolean(boolean), token.span),
		Token::Character(character) => Spanned::new(Expression::Character(character), token.span),
		Token::String(string) => Spanned::new(Expression::String(string), token.span),
		Token::Asterisk => expression_root(context, lexer)?
			.wrap(Expression::Dereference).prefix(token.span),
		Token::Increment => expression_root(context, lexer)?.wrap(|expression|
			Expression::Unary(UnaryOperator::Increment, expression)).prefix(token.span),
		Token::Decrement => expression_root(context, lexer)?.wrap(|expression|
			Expression::Unary(UnaryOperator::Decrement, expression)).prefix(token.span),
		Token::Minus => expression_root(context, lexer)?.wrap(|expression|
			Expression::Unary(UnaryOperator::Minus, expression)).prefix(token.span),
		Token::Not => expression_root(context, lexer)?.wrap(|expression|
			Expression::Unary(UnaryOperator::Not, expression)).prefix(token.span),
		Token::BracketOpen => {
			// TODO: Check for type cast
			let expression = expression_root(context, lexer)?;
			parser::expect(lexer, Token::BracketClose)?;
			expression
		}
		Token::BraceOpen => {
			let mut elements = Vec::new();
			let span = parser::list(lexer, Token::BraceClose, |lexer|
				Ok(elements.push(expression_root(context, lexer)?)))?;
			Spanned::new(Expression::InitializerList(elements),
				token.span.extend(span))
		}
		other => return Err(Spanned::new(match other {
			Token::BracketClose => ParserError::UnmatchedBracket,
			_ => ParserError::ExpectedTerminal,
		}, token.span)),
	})
}

pub fn arguments<'a>(context: &SymbolContext<'a>, lexer: &mut Lexer<'a>)
                     -> ParserResult<Vec<Spanned<Expression<'a>>>> {
	let mut arguments = Vec::new();
	parser::expect(lexer, Token::BracketOpen)?;
	parser::list(lexer, Token::BracketClose, |lexer| Ok(arguments
		.push(expression_root(context, lexer)?))).map(|_| arguments)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn post_operator() {
		let context = &context();
		assert!(expression_root(context, &mut Lexer::new("variable++")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable--")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("function()")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("function(variable)")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable[variable]")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable[0][1]")).is_ok());
	}

	#[test]
	fn unary_operator() {
		let context = &context();
		assert!(expression_root(context, &mut Lexer::new("-variable")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("!variable")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("*variable")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("++variable")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("--variable")).is_ok());
	}

	#[test]
	fn arithmetic() {
		let context = &context();
		assert!(expression_root(context, &mut Lexer::new("variable + 0")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("0.0 * 3.0 / 4.0")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("(1 + 2) % (3 - 4)")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable << (2 >> 1)")).is_ok());
	}

	#[test]
	fn comparator() {
		let context = &context();
		assert!(expression_root(context, &mut Lexer::new("1 < 2")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("2 <= 2")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("3 >= 2")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("3 > 2")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("1 != 2")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("1 == 1")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("true && false")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("false || true")).is_ok());
	}

	#[test]
	fn expression() {
		let context = &context();
		assert!(expression_root(context, &mut Lexer::new("false ? 0 + 1 : 1")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable -= 0 + 1")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable >>= 1")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable <<= 1")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("structure<int>(0)")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable.method()")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("variable->field")).is_ok());
		assert!(expression_root(context, &mut Lexer::new("{0, {1, 2}}")).is_ok());
	}

	fn context() -> SymbolContext<'static> {
		let mut context = SymbolContext::new();
		context.variable(["variable"].into());
		context.functions.insert(["function"].into());
		context.structures.insert(["structure"].into());
		context
	}
}
