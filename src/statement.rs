use crate::expression;
use crate::lexer::{Lexer, Token};
use crate::node::{Expression, Path, Statement};
use crate::parser::{self, ParserError, ParserResult};
use crate::span::Spanned;
use crate::symbol::SymbolContext;

pub fn scope<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                 -> ParserResult<Vec<Statement<'a>>> {
	parser::expect(lexer, Token::BraceOpen)?;
	let mut statements = Vec::new();
	context.scope(|context| {
		Ok(while lexer.peek().node != Token::BraceClose {
			statements.push(statement(context, lexer)?);
		})
	}).map(|_| lexer.thread(statements))
}

fn statement<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                 -> ParserResult<Statement<'a>> {
	let statement = statement_head(context, lexer)?;
	if statement.terminated() {
		parser::expect(lexer, Token::Terminator)?;
	}
	Ok(statement)
}

fn statement_head<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                      -> ParserResult<Statement<'a>> {
	match lexer.peek().node {
		Token::Terminator => Ok(Statement::Empty),
		Token::BraceOpen => Ok(Statement::Scope(scope(context, lexer)?)),
		Token::Identifier("if") => conditional(context, lexer),
		Token::Identifier("for") => for_construct(context, lexer),
		Token::Identifier("do") => {
			let statement = Box::new(statement(context, lexer.skip())?);
			parser::expect(lexer, Token::Identifier("while"))?;
			parser::expect(lexer, Token::BracketOpen)?;
			let expression = expression::expression_root(context, lexer)?;
			parser::expect(lexer, Token::BracketClose)?;
			Ok(Statement::DoWhile(statement, expression))
		}
		Token::Identifier("while") => {
			parser::expect(lexer.skip(), Token::BracketOpen)?;
			let expression = expression::expression_root(context, lexer)?;
			parser::expect(lexer, Token::BracketClose)?;
			let statement = Box::new(statement(context, lexer)?);
			Ok(Statement::While(expression, statement))
		}
		Token::Identifier("return") => match lexer.peek().node {
			Token::Terminator => Ok(Statement::Return(None)),
			_ => expression::expression_root(context, lexer.skip())
				.map(Some).map(Statement::Return)
		}
		Token::Identifier("break") => lexer.thread(Ok(Statement::Break)),
		Token::Identifier(_) => lexer.recover(|lexer| identifier(context, lexer))
			.or_else(|_| expression(context, lexer)),
		_ => expression(context, lexer),
	}
}

fn expression<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                  -> ParserResult<Statement<'a>> {
	lexer.recover(|lexer| expression::expression_root(context, lexer))
		.map(Statement::Expression).map_err(|_| ParserError::ExpectedStatement)
		.map_err(|error| Spanned::new(error, lexer.peek().span))
}

fn identifier<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                  -> ParserResult<Statement<'a>> {
	let mut variables = Vec::new();
	let structure = parser::parse_type(lexer)?;
	parser::list_head(lexer, &Token::Terminator, |lexer| {
		let identifier = parser::identifier(lexer)?.node;
		context.variable(Path::single(identifier.clone()));
		Ok(variables.push((identifier, match lexer.peek().node {
			Token::BracketOpen => Some(expression::arguments(context, lexer)?),
			Token::Assign => expression::expression_root(context, lexer.skip())
				.map(|expression| Some(vec![expression]))?,
			_ => None,
		})))
	}).map(|_| Statement::Variable(structure, variables))
}

fn conditional<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                   -> ParserResult<Statement<'a>> {
	parser::expect(lexer, Token::Identifier("if"))?;
	parser::expect(lexer, Token::BracketOpen)?;
	let expression = expression::expression_root(context, lexer)?;
	parser::expect(lexer, Token::BracketClose)?;
	let branch = Box::new(statement(context, lexer)?);

	let mut default = None;
	lexer.test(Token::Identifier("else"), |lexer|
		Ok(default = Some(Box::new(statement(context, lexer)?))))?;
	Ok(Statement::Conditional(expression, branch, default))
}

fn for_construct<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
                     -> ParserResult<Statement<'a>> {
	parser::expect(lexer, Token::Identifier("for"))?;
	parser::expect(lexer, Token::BracketOpen)?;
	context.scope(|context| lexer.recover(|lexer| -> ParserResult<_> {
		let structure = parser::parse_type(lexer)?;
		let identifier = parser::identifier(lexer)?.node;
		parser::expect(lexer, Token::Separator)?;

		let expression = expression::expression_root(context, lexer)?;
		parser::expect(lexer, Token::BracketClose)?;
		Ok((structure, identifier, expression))
	}).map(|header| context.scope(|context| {
		let (_, identifier, _) = &header;
		context.variable(Path::single(identifier.clone()));
		let statement = statement(context, lexer)?;
		Ok(Statement::ForRange(header, Box::new(statement)))
	})).unwrap_or_else(|_| {
		let initializer = statement(context, lexer)?;
		let condition = match lexer.peek().node {
			Token::Terminator => Ok(Expression::Boolean(true)),
			_ => expression::expression_root(context, lexer),
		}?;

		parser::expect(lexer, Token::Terminator)?;
		let iteration = match lexer.peek().node {
			Token::BracketClose => Ok(Statement::Empty),
			_ => statement_head(context, lexer),
		}?;

		parser::expect(lexer, Token::BracketClose)?;
		let statement = statement(context, lexer)?;
		let header = (Box::new(initializer), condition, Box::new(iteration));
		Ok(Statement::ForLoop(header, Box::new(statement)))
	}))
}

#[cfg(test)]
mod tests {
	use crate::node::Identifier;

	use super::*;

	#[test]
	fn test_statement() {
		let context = &mut context();
		assert!(statement(context, &mut Lexer::new("int a, b = 0, c(0);")).is_ok());
		assert!(statement(context, &mut Lexer::new("if (variable) break;")).is_ok());
		assert!(statement(context, &mut Lexer::new("if (true); else if (false); else;")).is_ok());
		assert!(statement(context, &mut Lexer::new("for (const auto i : variable);")).is_ok());
		assert!(statement(context, &mut Lexer::new("for (int i = 0; i < 10; ++i);")).is_ok());
		assert!(statement(context, &mut Lexer::new("for (;;);")).is_ok());
		assert!(statement(context, &mut Lexer::new("while (true);")).is_ok());
		assert!(statement(context, &mut Lexer::new("do; while (true);")).is_ok());
		assert!(statement(context, &mut Lexer::new("{{} { int a = 0; } {}}")).is_ok());
	}

	fn context() -> SymbolContext<'static> {
		let mut context = SymbolContext::new();
		context.variable(Path::single(Identifier("variable")));
		context
	}
}
