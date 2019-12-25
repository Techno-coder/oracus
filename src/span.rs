use std::collections::BTreeMap;

pub type S<T> = Spanned<T>;

#[derive(Debug, Copy, Clone)]
pub struct Span(pub usize, pub usize);

impl Span {
	pub fn extend(self, Span(_, byte_end): Span) -> Self {
		let Span(byte_start, span_byte_end) = self;
		assert!(span_byte_end <= byte_end);
		Span(byte_start, byte_end)
	}
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
	pub span: Span,
	pub node: T,
}

impl<T> Spanned<T> {
	pub fn new(node: T, span: Span) -> Self {
		Spanned { span, node }
	}

	pub fn map<F, R>(self, apply: F) -> Spanned<R> where F: FnOnce(T) -> R {
		Spanned::new(apply(self.node), self.span)
	}

	pub fn try_map<F, R, E>(self, apply: F) -> Result<Spanned<R>, E>
		where F: FnOnce(T, Span) -> Result<R, E> {
		Ok(Spanned::new(apply(self.node, self.span)?, self.span))
	}

	pub fn wrap<F, R>(self, apply: F) -> Spanned<R> where F: FnOnce(Box<Spanned<T>>) -> R {
		let span = self.span;
		Spanned::new(apply(Box::new(self)), span)
	}

	pub fn prefix(mut self, span: Span) -> Self {
		self.span = span.extend(self.span);
		self
	}
}

pub fn emit<E>(text: &str, error: Spanned<E>) where E: std::fmt::Debug {
	let lines = text.char_indices().filter(|(_, character)| character == &'\n').enumerate()
		.map(|(line, (index, _))| (index, line + 1)).collect::<BTreeMap<_, _>>();
	let Span(byte_start, byte_end) = error.span;

	println!("[Fail] {:?}", error.node);
	let (line_start, line) = lines.range(..byte_start).next_back()
		.map(|(index, line)| (index + 1, line)).unwrap_or((0, &1));
	let line_end = *lines.range(byte_end..).next()
		.map(|(index, _)| index).unwrap_or(&text.len());
	println!("{:4} | \t{}", line, &text[line_start..line_end]);

	let prefix: String = text[line_start..byte_start].chars().map(pad).collect();
	println!("       \t{}{}", prefix, "^".repeat(byte_end - byte_start));
}

fn pad(character: char) -> char {
	match character.is_whitespace() {
		true => character,
		false => ' ',
	}
}
