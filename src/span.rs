#[derive(Debug, Copy, Clone)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone)]
pub struct Spanned<T> {
	pub span: Span,
	pub node: T,
}

impl<T> Spanned<T> {
	pub fn new(node: T, span: Span) -> Self {
		Spanned { span, node }
	}
}
