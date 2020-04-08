use crate::lexer::{Lexer, LexerError, Token};
use crate::node::{Function, Identifier, IntegralKind,
	IntegralRank, Path, Program, Root, Type};
use crate::span::{Span, Spanned};
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

pub fn parse(string: &str) -> ParserResult<Program> {
	let lexer = &mut Lexer::new(string);
	let context = &mut SymbolContext::new();
	let mut program = Program::default();
	while let Some(root) = root(context, lexer)? {
		program.roots.push(root);
	}
	Ok(program)
}

fn root<'a>(context: &mut SymbolContext<'a>, lexer: &mut Lexer<'a>)
            -> ParserResult<Option<Root<'a>>> {
	let token = lexer.peek();
	Ok(Some(match token.node {
		Token::Hash => {
			expect(lexer.skip(), Token::Identifier("include"))?;
			expect(lexer, Token::AngleLeft)?;
			let identifier = identifier(lexer)?;
			expect(lexer, Token::AngleRight)?;
			Root::Include(identifier)
		}
		Token::Identifier("using") => {
			expect(lexer.skip(), Token::Identifier("namespace"))?;
			let identifier = identifier(lexer)?;
			expect(lexer, Token::Terminator)?;
			context.inclusions.insert(identifier.node.clone());
			Root::UsingNamespace(identifier)
		}
		Token::Identifier(_) => {
			let return_type = parse_type(context, lexer)?;
			let identifier = identifier(lexer)?.node;
			context.functions.insert(Path::single(identifier.clone()));

			context.scope(|context| {
				let mut parameters = Vec::new();
				expect(lexer, Token::BracketOpen)?;
				list(lexer, Token::BracketClose, |lexer| {
					let parameter_type = parse_type(context, lexer)?;
					let identifier = self::identifier(lexer)?;
					context.variable(Path::single(identifier.node.clone()));
					Ok(parameters.push((identifier, parameter_type)))
				})?;

				let body = crate::statement::scope(context, lexer)?;
				let function = Function { return_type, identifier, parameters, body };
				Ok(Root::Function(function))
			})?
		}
		Token::End => return Ok(None),
		_ => return Err(Spanned::new(ParserError::ExpectedRoot, token.span)),
	}))
}

pub fn path<'a>(lexer: &mut Lexer<'a>) -> ParserResult<Spanned<Path<'a>>> {
	let mut path = identifier(lexer)?.map(Path::single);
	while lexer.peek().node == Token::PathSeparator {
		let identifier = identifier(lexer.skip())?;
		path.node = path.node.push(identifier.node);
		path.span = path.span.extend(identifier.span);
	}
	Ok(path)
}

pub fn parse_type<'a>(context: &SymbolContext<'a>, lexer: &mut Lexer<'a>)
                      -> ParserResult<Spanned<Type<'a>>> {
	let token = lexer.peek();
	match token.node {
		Token::Identifier("const") => Ok(parse_type(context, lexer.skip())?
			.map(|structure| Type::Constant(Box::new(structure)))),
		Token::Identifier(_) => {
			let mut concrete = numeric_type(lexer).transpose().unwrap_or_else(|| {
				let mut templates = Vec::new();
				let path = path(lexer)?.try_map(|path, span| context.resolve_structure(&path)
					.ok_or(Spanned::new(ParserError::UndefinedPath, span)))?;
				lexer.test(Token::AngleLeft, |lexer| list(lexer, Token::AngleRight,
					|lexer| Ok(templates.push(parse_type(context, lexer)?.node))).map(|_| ()))?;
				Ok(path.map(|path| Type::Concrete(path, templates)))
			})?;

			loop {
				concrete = match lexer.peek().node {
					Token::Asterisk => concrete.map(|concrete|
						lexer.thread(Type::Pointer(Box::new(concrete)))),
					Token::Ampersand => concrete.map(|concrete|
						lexer.thread(Type::Reference(Box::new(concrete)))),
					_ => break Ok(concrete),
				};
			}
		}
		_ => Err(Spanned::new(ParserError::ExpectedType, token.span)),
	}
}

fn numeric_type(lexer: &mut Lexer) -> ParserResult<Option<Spanned<Type<'static>>>> {
	let span = lexer.peek().span;
	let unsigned = match lexer.peek().node {
		Token::Identifier("unsigned") => lexer.thread(true),
		Token::Identifier(_) => false,
		_ => return Ok(None),
	};

	let span = span.extend(lexer.peek().span);
	let rank = match lexer.peek().node {
		Token::Identifier("char") => lexer.thread(IntegralRank::Byte),
		Token::Identifier("short") => lexer.thread(IntegralRank::Short),
		Token::Identifier("int") => lexer.thread(IntegralRank::Integer),
		Token::Identifier("long") => {
			expect(lexer.skip(), Token::Identifier("long"))?;
			IntegralRank::LongLong
		}
		_ if unsigned => return Err(Spanned::new(ParserError::ExpectedUnsigned, span)),
		_ => return Ok(None),
	};

	let integral = IntegralKind(unsigned, rank);
	Ok(Some(Spanned::new(Type::Integral(integral), span)))
}

pub fn list<'a, F>(lexer: &mut Lexer<'a>, close: Token<'static>, function: F)
                   -> ParserResult<Span> where F: FnMut(&mut Lexer<'a>) -> ParserResult<()> {
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

pub fn expect(lexer: &mut Lexer, target: Token<'static>) -> ParserResult<Span> {
	let token = lexer.next();
	match token.node == target {
		false => Err(Spanned::new(ParserError::Expected(target), token.span)),
		true => Ok(token.span)
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
		let context = &mut SymbolContext::new();
		context.structures.insert(["pair"].into());
		context.structures.insert(["vector"].into());
		assert!(parse_type(context, &mut Lexer::new("int")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("const int")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("int*")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("int&")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("const int&")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("vector<int>")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("pair<int, bool>")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("vector<pair<int, int>>")).is_ok());
	}

	#[test]
	fn integer_type() {
		let context = &SymbolContext::new();
		assert!(parse_type(context, &mut Lexer::new("int")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("char")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("short")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("unsigned int")).is_ok());
		assert!(parse_type(context, &mut Lexer::new("unsigned long long")).is_ok());
	}
}
