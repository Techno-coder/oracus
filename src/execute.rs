use std::collections::HashMap;

use crate::node::{Function, Identifier, Path, Program, Statement, Type};
use crate::value::{self, Value};

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
pub struct ExecutionContext<'a, 'b> {
	pub frames: HashMap<FrameIndex, ExecutionFrame<'a, 'b>>,
	pub stack: Vec<FrameIndex>,
	pub next_frame: usize,
}

impl<'a, 'b> ExecutionContext<'a, 'b> {
	pub fn variable(&self, path: &Path<'a>) -> (FrameIndex, &Value<'a>, &Type<'a>) {
		self.stack.iter().rev().find_map(|frame| self.frames[frame].variables
			.get(path).map(|(value, structure)| (frame.clone(), value, structure)))
			.unwrap_or_else(|| panic!("Variable: {}, does not exist in context", path))
	}

	pub fn concrete(&mut self, value: Value<'a>) -> ExecutionResult<Value<'a>> {
		match value {
			Value::Reference(reference) => self.dereference(&reference),
			other => return Ok(other),
		}.map(|(value, _)| value.clone())
	}

	pub fn dereference(&mut self, reference: &Reference<'a>)
	                   -> ExecutionResult<(&mut Value<'a>, &Type<'a>)> {
		let Reference(frame, path, fields) = reference;
		let (variable, structure) = self.frames.get_mut(frame)
			.and_then(|frame| frame.variables.get_mut(path))
			.map(|(variable, structure)| (variable, &*structure))
			.ok_or(ExecutionError::InvalidReference)?;
		Ok(fields.iter().fold((variable, structure), |(variable, _), field| match variable {
			Value::Structure(structure) => structure.fields.get_mut(field)
				.map(|(value, structure)| (value, &*structure)).unwrap_or_else(||
				panic!("Field: {}, does not exist on structure", field)),
			other => panic!("Cannot access field: {}, on value: {:?}", field, other),
		}))
	}

	pub fn insert(&mut self, path: Path<'a>, value: Value<'a>, structure: Type<'a>) {
		self.frame().variables.insert(path, (value, structure));
	}

	pub fn scope<F, R>(&mut self, function: F) -> R where F: FnOnce(&mut Self) -> R {
		let node_function = self.frame().function;
		self.function(node_function, function)
	}

	pub fn function<F, R>(&mut self, node_function: &'b Function<'a>, function: F) -> R
		where F: FnOnce(&mut Self) -> R {
		let frame = FrameIndex(self.next_frame);
		self.stack.push(frame.clone());
		self.next_frame += 1;

		self.frames.insert(frame.clone(),
			ExecutionFrame::new(node_function));
		let result = function(self);
		self.frames.remove(&frame);
		self.stack.pop();
		result
	}

	pub fn frame(&mut self) -> &mut ExecutionFrame<'a, 'b> {
		let frame = self.stack.last().expect("Execution context stack is empty");
		self.frames.get_mut(frame).unwrap()
	}
}

#[derive(Debug)]
pub struct ExecutionFrame<'a, 'b> {
	pub variables: HashMap<Path<'a>, (Value<'a>, Type<'a>)>,
	pub function: &'b Function<'a>,
}

impl<'a, 'b> ExecutionFrame<'a, 'b> {
	pub fn new(function: &'b Function<'a>) -> Self {
		ExecutionFrame { variables: HashMap::new(), function }
	}
}

#[derive(Debug, PartialEq)]
pub enum Execution<'a> {
	Return(Value<'a>),
	Break,
	None,
}

macro_rules! execute {
    ($program:expr, $context:expr, $statement:expr) => {
        match execute($program, $context, $statement)? {
            Execution::None => (),
            other => return Ok(other),
        }
    };
}

macro_rules! resume {
    ($program:expr, $context:expr, $statement:expr) => {
        match execute($program, $context, $statement)? {
            Execution::None => (),
            Execution::Break => return Ok(Execution::None),
            other => return Ok(other),
        }
    }
}

pub fn function<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                        function: &'b Function<'a>, arguments: Vec<Value<'a>>) -> ExecutionResult<Value<'a>> {
	context.function(function, |context| {
		Iterator::zip(function.parameters.iter().cloned(),
			arguments.into_iter()).for_each(|((identifier, structure), value)|
			context.insert(Path::single(identifier), value, structure));
		match (scope(program, context, &function.body)?, &function.return_type) {
			(Execution::None, structure) if structure == &Type::void() => Ok(Value::Void),
			(Execution::None, _) => Err(ExecutionError::VoidReturnValue),
			(Execution::Break, _) => panic!("Cannot break from function"),
			(Execution::Return(value), _) => Ok(value),
		}
	})
}

pub fn execute<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                       statement: &Statement<'a>) -> ExecutionResult<Execution<'a>> {
	match statement {
		Statement::Variable(structure, variables) =>
			variables.iter().try_for_each(|(identifier, arguments)| {
				let value = arguments.as_ref().map(|arguments| value::construct(program,
					context, structure, arguments)).unwrap_or(Ok(Value::Uninitialised))?;
				Ok(context.insert(Path::single(identifier.clone()), value, structure.clone()))
			}).map(|_| Execution::None),
		Statement::Conditional(condition, branch, default) =>
			match value::concrete(program, context, condition)?.boolean() {
				true => execute(program, context, branch),
				false => match default {
					Some(default) => execute(program, context, default),
					None => Ok(Execution::None),
				}
			},
		Statement::ForLoop((initializer, condition, iteration), statement) =>
			context.scope(|context| {
				execute!(program, context, initializer);
				while value::concrete(program, context, condition)?.boolean() {
					resume!(program, context, statement);
					execute!(program, context, iteration);
				}
				Ok(Execution::None)
			}),
		// TODO: Implement for range loops
		Statement::ForRange(_, _) => unimplemented!(),
		Statement::DoWhile(statement, condition) => {
			resume!(program, context, statement);
			while value::concrete(program, context, condition)?.boolean() {
				resume!(program, context, statement);
			}
			Ok(Execution::None)
		}
		Statement::While(condition, statement) => {
			while value::concrete(program, context, condition)?.boolean() {
				resume!(program, context, statement);
			}
			Ok(Execution::None)
		}
		Statement::Expression(expression) => value::evaluate(program,
			context, expression).map(|_| Execution::None),
		Statement::Scope(statements) => scope(program, context, statements),
		Statement::Return(expression) => expression.as_ref().map(|expression| {
			let return_type = &context.frame().function.return_type;
			value::parameter(program, context, return_type, expression)
		}.map(Execution::Return)).unwrap_or(Ok(Execution::Return(Value::Void))),
		Statement::Break => Ok(Execution::Break),
		Statement::Empty => Ok(Execution::None)
	}
}

fn scope<'a, 'b>(program: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
                 statements: &[Statement<'a>]) -> ExecutionResult<Execution<'a>> {
	context.scope(|context| statements.iter().map(|statement| execute(program, context, statement))
		.find(|result| result.as_ref().map(|execution| execution != &Execution::None)
			.unwrap_or(true)).unwrap_or(Ok(Execution::None)))
}
