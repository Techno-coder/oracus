use std::collections::HashSet;

use crate::node::{Identifier, Path};

#[derive(Debug, Default)]
pub struct SymbolContext<'a> {
	pub inclusions: HashSet<Identifier<'a>>,
	pub structures: HashSet<Path<'a>>,
	pub functions: HashSet<Path<'a>>,
	pub frames: Vec<SymbolFrame<'a>>,
}

impl<'a> SymbolContext<'a> {
	/// Creates a symbol context with an initial frame.
	pub fn new() -> Self {
		let mut context = SymbolContext::default();
		context.frames.push(SymbolFrame::default());
		context.structures.insert(["auto"].into());
		context.structures.insert(["int"].into());
		context.structures.insert(["long"].into());
		context.structures.insert(["short"].into());
		context.structures.insert(["float"].into());
		context.structures.insert(["double"].into());
		context.structures.insert(["unsigned"].into());
		context.structures.insert(["char"].into());
		context.structures.insert(["bool"].into());
		context.variable(["nullptr"].into());
		context
	}

	pub fn variable(&mut self, variable: Path<'a>) {
		self.frames.last_mut().expect("Symbol context is empty")
			.variables.insert(variable);
	}

	pub fn resolve_variable(&self, variable: &Path<'a>) -> Option<Path<'a>> {
		std::iter::once(variable.clone()).chain(self.inclusions.iter().map(|inclusion|
			variable.prefix(inclusion.clone()))).find(|path| self.frames.iter().rev()
			.any(|frame| frame.variables.contains(path)))
	}

	pub fn resolve_function(&self, function: &Path<'a>) -> Option<Path<'a>> {
		std::iter::once(function.clone()).chain(self.inclusions.iter().map(|inclusion|
			function.prefix(inclusion.clone()))).find(|path| self.functions.contains(path))
	}

	pub fn resolve_structure(&self, structure: &Path<'a>) -> Option<Path<'a>> {
		std::iter::once(structure.clone()).chain(self.inclusions.iter().map(|inclusion|
			structure.prefix(inclusion.clone()))).find(|path| self.structures.contains(path))
	}

	pub fn scope<F, R>(&mut self, function: F) -> R where F: FnOnce(&mut Self) -> R {
		self.frames.push(SymbolFrame::default());
		let result = function(self);
		self.frames.pop().unwrap();
		result
	}
}

#[derive(Debug, Default)]
pub struct SymbolFrame<'a> {
	pub variables: HashSet<Path<'a>>,
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn context_variable() {
		let mut context = SymbolContext::new();
		context.inclusions.insert(Identifier("std"));
		context.variable(Path::single(Identifier("local")));
		context.variable(Path(vec![Identifier("std"), Identifier("cin")]));
		assert!(context.resolve_variable(&Path::single(Identifier("cin"))).is_some());
		assert!(context.resolve_variable(&Path::single(Identifier("local"))).is_some());
	}

	#[test]
	fn context_function() {
		let mut context = SymbolContext::new();
		context.inclusions.insert(Identifier("std"));
		context.functions.insert(Path::single(Identifier("local")));
		context.functions.insert(Path(vec![Identifier("std"), Identifier("sort")]));
		assert!(context.resolve_function(&Path::single(Identifier("sort"))).is_some());
		assert!(context.resolve_function(&Path::single(Identifier("local"))).is_some());
	}
}
