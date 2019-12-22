use crate::lexer::{Lexer, LexerError, Token};
use crate::node::{Function, Identifier, Path, RootNode, Type};
use crate::span::Spanned;
use crate::symbol::SymbolContext;

pub type ParserResult<T> = Result<T, Spanned<ParserError>>;

#[derive(Debug)]
pub enum ParserError {
	Lexer(LexerError),
	Expected(Token<'static>),
	ExpectedIdentifier,
	ExpectedRoot,
	ExpectedType,
	ExpectedUnsigned,
	ExpectedTerminal,
	ExpectedStatement,
	UnmatchedBracket,
	UndefinedPath,
}

pub fn parse(string: &str) -> ParserResult<Vec<RootNode>> {
	let lexer = &mut Lexer::new(string);
	let context = &mut SymbolContext::new();
	std::iter::from_fn(|| root(context, lexer).transpose()).collect()
}

fn root<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
            -> ParserResult<Option<RootNode<'a>>> {
	let token = lexer.peek();
	Ok(Some(match token.node {
		Token::Hash => {
			expect(lexer.skip(), Token::Identifier("include"))?;
			expect(lexer, Token::AngleLeft)?;
			let identifier = identifier(lexer)?;
			expect(lexer, Token::AngleRight)?;

			match identifier.node {
				Identifier("iostream") => {
					context.variable(Path(vec![Identifier("std"), Identifier("cin")]));
					context.variable(Path(vec![Identifier("std"), Identifier("cout")]));
					context.variable(Path(vec![Identifier("std"), Identifier("endl")]));
					context.functions.insert(Path(vec![Identifier("std"),
						Identifier("ios"), Identifier("sync_with_stdio")]));
				}
				Identifier("algorithm") => {
					context.functions.insert(Path(vec![Identifier("std"), Identifier("sort")]));
					context.functions.insert(Path(vec![Identifier("std"), Identifier("swap")]));
				}
				Identifier("utility") => {
					context.structures.insert(Path(vec![Identifier("std"), Identifier("pair")]));
				}
				Identifier("vector") => {
					context.structures.insert(Path(vec![Identifier("std"), Identifier("vector")]));
				}
				Identifier("stack") => {
					context.structures.insert(Path(vec![Identifier("std"), Identifier("stack")]));
				}
				Identifier("map") => {
					context.structures.insert(Path(vec![Identifier("std"), Identifier("map")]));
				}
				Identifier("climits") => {
					context.variable(Path::single(Identifier("INT_MAX")));
				}
				Identifier("cassert") => {
					context.functions.insert(Path::single(Identifier("assert")));
				}
				_ => (),
			}

			RootNode::Include(identifier)
		}
		Token::Identifier("using") => {
			expect(lexer.skip(), Token::Identifier("namespace"))?;
			let identifier = identifier(lexer)?;
			expect(lexer, Token::Terminator)?;
			context.inclusions.insert(identifier.node.clone());
			RootNode::UsingNamespace(identifier)
		}
		Token::Identifier(_) => {
			let return_type = parse_type(lexer)?;
			let identifier = identifier(lexer)?.node;
			context.functions.insert(Path::single(identifier.clone()));

			let mut parameters = Vec::new();
			expect(lexer, Token::BracketOpen)?;
			list(lexer, Token::BracketClose, |lexer| {
				let parameter_type = parse_type(lexer)?;
				let identifier = self::identifier(lexer)?.node;
				Ok(parameters.push((identifier, parameter_type)))
			})?;

			let body = crate::statement::scope(context, lexer)?;
			let function = Function { return_type, identifier, parameters, body };
			RootNode::Function(function)
		}
		Token::End => return Ok(None),
		_ => return Err(Spanned::new(ParserError::ExpectedRoot, token.span)),
	}))
}

pub fn path<'a>(lexer: &mut Lexer<'a>) -> ParserResult<Path<'a>> {
	let mut path = Path::single(identifier(lexer)?.node);
	while lexer.peek().node == Token::PathSeparator {
		path = path.push(identifier(lexer.skip())?.node);
	}
	Ok(path)
}

pub fn parse_type<'a>(lexer: &mut Lexer<'a>) -> ParserResult<Type<'a>> {
	let token = lexer.peek();
	match token.node {
		Token::Identifier("const") =>
			Ok(Type::Constant(Box::new(parse_type(lexer.skip())?))),
		Token::Identifier(_) => {
			let Path(mut elements) = path(lexer)?;
			if elements.len() == 1 {
				let Identifier(identifier) = &mut elements[0];
				integer_type(lexer, identifier)?;
			}

			let mut templates = Vec::new();
			lexer.test(Token::AngleLeft, |lexer| list(lexer, Token::AngleRight,
				|lexer| Ok(templates.push(parse_type(lexer)?))))?;

			let mut concrete = Type::Concrete(Path(elements), templates);
			loop {
				concrete = match lexer.peek().node {
					Token::Asterisk => lexer.thread(Type::Pointer(Box::new(concrete))),
					Token::Ampersand => lexer.thread(Type::Reference(Box::new(concrete))),
					_ => break Ok(concrete),
				};
			}
		}
		_ => Err(Spanned::new(ParserError::ExpectedType, token.span)),
	}
}

fn integer_type(lexer: &mut Lexer, identifier: &mut &str) -> ParserResult<()> {
	Ok(*identifier = match *identifier {
		"unsigned" => {
			let integer_token = self::identifier(lexer)?;
			let Identifier(integer) = integer_token.node;
			match integer {
				"int" => "unsigned int",
				"short" => "unsigned short",
				"long" => match lexer.peek().node {
					Token::Identifier("int") => lexer.thread("unsigned long int"),
					Token::Identifier("long") => lexer.thread("unsigned long long"),
					_ => "long",
				}
				_ => return Err(Spanned::new(ParserError::ExpectedUnsigned, integer_token.span)),
			}
		}
		"long" => match lexer.peek().node {
			Token::Identifier("int") => lexer.thread("long int"),
			Token::Identifier("long") => lexer.thread("long long"),
			Token::Identifier("double") => lexer.thread("long double"),
			_ => "long",
		}
		_ => return Ok(()),
	})
}

pub fn list<'a, F>(lexer: &mut Lexer<'a>, close: Token<'static>, function: F)
                   -> ParserResult<()> where F: FnMut(&mut Lexer<'a>) -> ParserResult<()> {
	list_head(lexer, &close, function)?;
	expect(lexer, close)
}

pub fn list_head<'a, F>(lexer: &mut Lexer<'a>, close: &Token<'static>, mut function: F)
                        -> ParserResult<()> where F: FnMut(&mut Lexer<'a>) -> ParserResult<()> {
	Ok(while &lexer.peek().node != close {
		function(lexer)?;
		match lexer.peek().node {
			Token::ListSeparator => lexer.next(),
			_ => break,
		};
	})
}

pub fn expect(lexer: &mut Lexer, target: Token<'static>) -> ParserResult<()> {
	let token = lexer.next();
	match token.node == target {
		false => Err(Spanned::new(ParserError::Expected(target), token.span)),
		true => Ok(())
	}
}

pub fn identifier<'a>(lexer: &mut Lexer<'a>) -> ParserResult<Spanned<Identifier<'a>>> {
	let token = lexer.next();
	match token.node {
		Token::Identifier(identifier) => Ok(Spanned::new(Identifier(identifier), token.span)),
		_ => Err(Spanned::new(ParserError::ExpectedIdentifier, token.span)),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_root() {
		let context = &mut SymbolContext::new();
		assert!(root(context, &mut Lexer::new("#include <vector>")).is_ok());
		assert!(root(context, &mut Lexer::new("using namespace std;")).is_ok());
	}

	#[test]
	fn test_type() {
		assert!(parse_type(&mut Lexer::new("int")).is_ok());
		assert!(parse_type(&mut Lexer::new("const int")).is_ok());
		assert!(parse_type(&mut Lexer::new("int*")).is_ok());
		assert!(parse_type(&mut Lexer::new("int&")).is_ok());
		assert!(parse_type(&mut Lexer::new("const int&")).is_ok());
		assert!(parse_type(&mut Lexer::new("vector<int>")).is_ok());
		assert!(parse_type(&mut Lexer::new("pair<int, bool>")).is_ok());
		assert!(parse_type(&mut Lexer::new("vector<pair<int, int>>")).is_ok());
	}

	#[test]
	fn integer_type() {
		assert!(parse_type(&mut Lexer::new("int")).is_ok());
		assert!(parse_type(&mut Lexer::new("unsigned int")).is_ok());
		assert!(parse_type(&mut Lexer::new("unsigned long")).is_ok());
		assert!(parse_type(&mut Lexer::new("unsigned long long")).is_ok());
		assert!(parse_type(&mut Lexer::new("long double")).is_ok());
	}
}
