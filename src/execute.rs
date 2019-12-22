use std::collections::HashMap;

use crate::node::{Function, Identifier, Path, Statement, Type};
use crate::value::{self, concrete, Value};

pub type ExecutionResult<T> = Result<T, ExecutionError>;
pub type Field<'a> = Identifier<'a>;

#[derive(Debug)]
pub enum ExecutionError {
	InvalidReference,
	IndexBounds,
	VoidReturnValue,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FrameIndex(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Reference<'a>(pub FrameIndex, pub Path<'a>, pub Vec<Field<'a>>);

#[derive(Debug, Default)]
pub struct ExecutionContext<'a> {
	pub frames: HashMap<FrameIndex, ExecutionFrame<'a>>,
	pub stack: Vec<FrameIndex>,
	pub next_frame: usize,
}

impl<'a> ExecutionContext<'a> {
	pub fn variable(&self, path: &Path<'a>) -> Reference<'a> {
		self.stack.iter().rev().find(|frame| self.frames[frame].variables.contains_key(path))
			.map(|frame| Reference(frame.clone(), path.clone(), Vec::new())).unwrap_or_else(||
			panic!("Variable: {}, does not exist in context", path))
	}

	pub fn concrete(&mut self, value: Value<'a>) -> ExecutionResult<Value<'a>> {
		match value {
			Value::Reference(reference) => self.dereference(&reference),
			other => return Ok(other),
		}.map(|value| value.clone())
	}

	pub fn dereference(&mut self, reference: &Reference<'a>) -> ExecutionResult<&mut Value<'a>> {
		let Reference(frame, path, fields) = reference;
		let variable = self.frames.get_mut(frame)
			.and_then(|frame| frame.variables.get_mut(path))
			.ok_or(ExecutionError::InvalidReference)?;
		Ok(fields.iter().fold(variable, |variable, field| match variable {
			Value::Structure(structure) => structure.get_mut(field)
				.unwrap_or_else(|| panic!("Field: {}, does not exist on structure", field)),
			other => panic!("Cannot access field: {}, on value: {:?}", field, other),
		}))
	}

	pub fn insert(&mut self, path: Path<'a>, value: Value<'a>) {
		let frame = self.stack.last().expect("Execution context stack is empty");
		self.frames.get_mut(frame).unwrap().variables.insert(path, value);
	}

	pub fn scope<F, R>(&mut self, function: F) -> R where F: FnOnce(&mut Self) -> R {
		let frame = FrameIndex(self.next_frame);
		self.stack.push(frame.clone());
		self.next_frame += 1;

		self.frames.insert(frame.clone(), ExecutionFrame::default());
		let result = function(self);
		self.frames.remove(&frame);
		self.stack.pop();
		result
	}
}

#[derive(Debug, Default)]
pub struct ExecutionFrame<'a> {
	pub variables: HashMap<Path<'a>, Value<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Execution<'a> {
	Return(Value<'a>),
	Break,
	None,
}

macro_rules! execute {
    ($context:expr, $statement:expr) => {
        match execute($context, $statement)? {
            Execution::None => (),
            other => return Ok(other),
        }
    };
}

macro_rules! resume {
    ($context:expr, $statement:expr) => {
        match execute($context, $statement)? {
            Execution::None => (),
            Execution::Break => return Ok(Execution::None),
            other => return Ok(other),
        }
    }
}

pub fn function<'a>(context: &mut ExecutionContext<'a>,
                    function: &Function<'a>) -> ExecutionResult<Value<'a>> {
	let result = scope(context, &function.body)?;
	match (result, &function.return_type) {
		(Execution::Break, _) => panic!("Cannot break from function"),
		(Execution::None, structure) if structure == &Type::void() => Ok(Value::Void),
		(Execution::None, _) => Err(ExecutionError::VoidReturnValue),
		(Execution::Return(value), _) => Ok(value),
	}
}

pub fn execute<'a>(context: &mut ExecutionContext<'a>,
                   statement: &Statement<'a>) -> ExecutionResult<Execution<'a>> {
	match statement {
		Statement::Variable(structure, variables) =>
			variables.iter().try_for_each(|(identifier, arguments)| {
				let value = arguments.as_ref().map(|arguments| value::construct(context,
					structure, arguments)).unwrap_or(Ok(Value::Uninitialised))?;
				Ok(context.insert(Path::single(identifier.clone()), value))
			}).map(|_| Execution::None),
		Statement::Conditional(condition, branch, default) =>
			return match value::concrete(context, condition)?.boolean() {
				true => execute(context, branch),
				false => match default {
					Some(default) => execute(context, default),
					None => Ok(Execution::None),
				}
			},
		Statement::ForLoop((initializer, condition, iteration), statement) =>
			context.scope(|context| {
				execute!(context, initializer);
				while concrete(context, condition)?.boolean() {
					resume!(context, statement);
					execute!(context, iteration);
				}
				Ok(Execution::None)
			}),
		// TODO: Implement for range loops
		Statement::ForRange(_, _) => unimplemented!(),
		Statement::DoWhile(statement, condition) => {
			resume!(context, statement);
			while concrete(context, condition)?.boolean() {
				resume!(context, statement);
			}
			Ok(Execution::None)
		}
		Statement::While(condition, statement) => {
			while concrete(context, condition)?.boolean() {
				resume!(context, statement);
			}
			Ok(Execution::None)
		}
		Statement::Expression(expression) =>
			value::evaluate(context, expression).map(|_| Execution::None),
		Statement::Scope(statements) => scope(context, statements),
		Statement::Return(expression) => expression.as_ref().map(|expression|
			value::concrete(context, expression).map(Execution::Return))
			.unwrap_or(Ok(Execution::Return(Value::Void))),
		Statement::Break => Ok(Execution::Break),
		Statement::Empty => Ok(Execution::None)
	}
}

fn scope<'a>(context: &mut ExecutionContext<'a>,
             statements: &[Statement<'a>]) -> ExecutionResult<Execution<'a>> {
	context.scope(|context| statements.iter().map(|statement| execute(context, statement))
		.find(|result| result.as_ref().map(|execution| execution != &Execution::None)
			.unwrap_or(true)).unwrap_or(Ok(Execution::None)))
}
