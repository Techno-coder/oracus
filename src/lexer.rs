use std::iter::Peekable;
use std::str::CharIndices;

use crate::node::ValueOperator;
use crate::span::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
	Hash,
	Assign,
	AngleLeft,
	AngleRight,
	ShiftLeft,
	BracketOpen,
	BracketClose,
	SquareOpen,
	SquareClose,
	BraceOpen,
	BraceClose,
	DereferenceAccess,
	Access,
	Separator,
	PathSeparator,
	ListSeparator,
	Ternary,
	ExclusiveOr,
	Or,
	Add,
	Minus,
	Asterisk,
	Divide,
	Modulo,
	Increment,
	Decrement,
	LogicalAnd,
	LogicalOr,
	Equal,
	Not,
	NotEqual,
	LessEqual,
	GreaterEqual,
	Ampersand,
	Terminator,
	Assignment(ValueOperator),
	Error(LexerError),
	Identifier(&'a str),
	Character(char),
	String(&'a str),
	Boolean(bool),
	Integer(i128),
	Float(f64),
	End,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
	UnmatchedQuote,
	UnmatchedSingleQuote,
	InvalidFloat,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
	string: &'a str,
	characters: Peekable<CharIndices<'a>>,
	buffer: Option<Spanned<Token<'a>>>,
}

impl<'a> Lexer<'a> {
	pub fn new(string: &'a str) -> Self {
		let characters = string.char_indices().peekable();
		Lexer { string, characters, buffer: None }
	}

	pub fn next(&mut self) -> Spanned<Token<'a>> {
		if let Some(token) = self.buffer.take() { return token; }
		let (start, character) = match self.characters.next() {
			None => return Spanned::new(Token::End,
				Span(self.string.len(), self.string.len() + 1)),
			Some(character) => character,
		};

		let span = Span(start, start + 1);
		match character {
			'#' => Spanned::new(Token::Hash, span),
			'.' => Spanned::new(Token::Access, span),
			',' => Spanned::new(Token::ListSeparator, span),
			';' => Spanned::new(Token::Terminator, span),
			'(' => Spanned::new(Token::BracketOpen, span),
			')' => Spanned::new(Token::BracketClose, span),
			'[' => Spanned::new(Token::SquareOpen, span),
			']' => Spanned::new(Token::SquareClose, span),
			'{' => Spanned::new(Token::BraceOpen, span),
			'}' => Spanned::new(Token::BraceClose, span),
			'/' => match self.characters.peek() {
				Some((_, '/')) => {
					self.characters.find(|(_, character)| character == &'\n');
					return self.next();
				}
				_ => Spanned::new(Token::Divide, span),
			},
			'?' => Spanned::new(Token::Ternary, span),
			'!' => self.double(start, Token::Not, &[('=', Token::NotEqual)]),
			'<' => self.double(start, Token::AngleLeft,
				&[('<', Token::ShiftLeft), ('=', Token::LessEqual)]),
			'>' => self.double(start, Token::AngleRight, &[('=', Token::GreaterEqual)]),
			'=' => self.double(start, Token::Assign, &[('=', Token::Equal)]),
			'+' => self.double(start, Token::Add, &[('+', Token::Increment),
				('=', Token::Assignment(ValueOperator::Add))]),
			'-' => self.double(start, Token::Minus, &[('-', Token::Decrement),
				('=', Token::Assignment(ValueOperator::Minus)), ('>', Token::DereferenceAccess)]),
			'*' => self.double(start, Token::Asterisk,
				&[('=', Token::Assignment(ValueOperator::Multiply))]),
			'%' => self.double(start, Token::Modulo,
				&[('=', Token::Assignment(ValueOperator::Modulo))]),
			'&' => self.double(start, Token::Ampersand, &[('&', Token::LogicalAnd),
				('=', Token::Assignment(ValueOperator::And))]),
			'|' => self.double(start, Token::Or, &[('|', Token::LogicalOr),
				('=', Token::Assignment(ValueOperator::Or))]),
			'^' => self.double(start, Token::ExclusiveOr,
				&[('=', Token::Assignment(ValueOperator::ExclusiveOr))]),
			':' => self.double(start, Token::Separator, &[(':', Token::PathSeparator)]),
			'\'' => {
				let character = self.characters.next();
				let quote = self.characters.next();
				match (character, quote) {
					(Some((_, character)), Some((end, '\''))) =>
						Spanned::new(Token::Character(character), Span(start, end + 1)),
					_ => Spanned::new(Token::Error(LexerError::UnmatchedSingleQuote), span),
				}
			}
			'"' => match self.characters.find(|(_, character)| character == &'"') {
				Some((end, _)) => Spanned::new(Token::String(&self
					.string[start + 1..end]), Span(start, end + 1)),
				None => Spanned::new(Token::Error(LexerError::UnmatchedQuote),
					Span(start, self.string.len())),
			},
			_ if character.is_whitespace() => return self.next(),
			_ if character.is_ascii_digit() => {
				let end = self.predicate(|character|
					character.is_ascii_digit() || character == &'.');
				let string = &self.string[start..end];
				Spanned::new(match string.contains('.') {
					true => string.parse().map(Token::Float)
						.unwrap_or(Token::Error(LexerError::InvalidFloat)),
					false => string.parse().map(Token::Integer).unwrap(),
				}, Span(start, end))
			}
			_ => {
				let end = self.predicate(|character| !character.is_whitespace()
					&& !character.is_ascii_punctuation() || character == &'_');
				Spanned::new(match &self.string[start..end] {
					"true" => Token::Boolean(true),
					"false" => Token::Boolean(false),
					identifier => Token::Identifier(identifier),
				}, Span(start, end))
			}
		}
	}

	pub fn peek(&mut self) -> &Spanned<Token<'a>> {
		if self.buffer.is_none() {
			self.buffer = Some(self.next());
		}
		self.buffer.as_ref().unwrap()
	}

	/// Executes the function only if the next token matches the target.
	/// Does not consume any tokens if the comparison fails.
	pub fn test<F, E>(&mut self, target: Token<'static>, function: F) -> Result<(), E>
		where F: FnOnce(&mut Self) -> Result<(), E> {
		match self.peek().node == target {
			false => Ok(()),
			true => {
				self.next();
				function(self)
			}
		}
	}

	/// Consumes the next token and returns the provided value.
	pub fn thread<T>(&mut self, value: T) -> T {
		self.next();
		value
	}

	/// Consumes the next token and returns the lexer.
	pub fn skip(&mut self) -> &mut Self {
		self.next();
		self
	}

	/// Applies the provided function and rolls back the lexer
	/// if an error is returned.
	pub fn recover<F, T, E>(&mut self, function: F) -> Result<T, E>
		where F: FnOnce(&mut Self) -> Result<T, E> {
		let recovery = self.clone();
		match function(self) {
			Ok(value) => Ok(value),
			Err(error) => {
				*self = recovery;
				Err(error)
			}
		}
	}

	fn double(&mut self, start: usize, single: Token<'a>,
	          pairs: &[(char, Token<'a>)]) -> Spanned<Token<'a>> {
		self.characters.peek().and_then(|(_, character)|
			pairs.iter().find(|(candidate, _)| candidate == character))
			.map(|(_, token)| (token.clone(), self.characters.next().unwrap()))
			.map(|(token, (end, _))| Spanned::new(token, Span(start, end + 1)))
			.unwrap_or(Spanned::new(single, Span(start, start + 1)))
	}

	fn predicate<F>(&mut self, predicate: F) -> usize where F: Fn(&char) -> bool {
		while let Some((_, character)) = self.characters.peek() {
			match predicate(character) {
				true => self.characters.next(),
				false => break,
			};
		}

		self.characters.peek().map(|(end, _)| *end)
			.unwrap_or(self.string.len())
	}
}
