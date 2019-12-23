use std::collections::HashMap;

use crate::execute::{ExecutionContext, ExecutionResult};
use crate::node::{BinaryOperator, Identifier, Intrinsic, Path, Program, Type, ValueOperator};
use crate::symbol::SymbolContext;
use crate::value::Value;

/// Intrinsic for input and output operations.
#[derive(Debug)]
pub struct Stream {
	variables: HashMap<Path<'static>, Value<'static>>,
}

impl Stream {
	pub fn new() -> Stream {
		let mut variables = HashMap::new();
		super::variable(&mut variables, "cin");
		super::variable(&mut variables, "cout");
		super::variable(&mut variables, "endl");
		Stream { variables }
	}
}

impl Intrinsic for Stream {
	fn register<'a>(&self, context: &mut SymbolContext<'a>) {
		self.variables.keys().for_each(|variable| context.variable(variable.clone()));
		context.functions.insert(["std", "ios", "sync_with_stdio"].into());
	}

	fn variable<'a, 'b>(&self, _: &'b Program<'a>, _: &mut ExecutionContext<'a, 'b>,
	                    variable: &Path<'a>) -> ExecutionResult<Option<Value<'a>>> {
		Ok(self.variables.get(variable).cloned())
	}

	fn operation<'a, 'b>(&self, _: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
	                     operator: BinaryOperator, left: &Value<'a>, right: &Value<'a>)
	                     -> ExecutionResult<Option<Value<'a>>> {
		match operator {
			BinaryOperator::Value(ValueOperator::ShiftRight) => {
				if left == &self.variables[&["std", "cin"].into()] {
					let reference = &right.clone().reference();
					let (value, structure) = context.dereference(reference)?;
					if structure == &Type::single(["float"].into()) {
						*value = Value::Float(read!());
					} else if structure == &Type::single(["int"].into()) {
						*value = Value::Integer(read!());
					} else if structure == &Type::single(["bool"].into()) {
						let string: String = read!();
						*value = Value::Boolean(&string == "1");
					} else if structure == &Type::single(["char"].into()) {
						*value = Value::Character(read!());
					} else if structure == &Type::single(["string"].into()) {
						*value = Value::String(read!());
					} else {
						panic!("Cannot read into value: {:?}", value);
					}
					return Ok(Some(left.clone()));
				}
			}
			BinaryOperator::Value(ValueOperator::ShiftLeft) => {
				if left == &self.variables[&["std", "cout"].into()] {
					match &context.concrete(right.clone())? {
						Value::Float(float) => print!("{}", float),
						Value::Integer(integer) => print!("{}", integer),
						Value::Boolean(boolean) if *boolean => print!("1"),
						Value::Boolean(boolean) if !*boolean => print!("0"),
						Value::Character(character) => print!("{}", character),
						Value::String(string) => print!("{}", string),
						Value::Structure(structure) if structure.structure ==
							Type::single(["std", "endl"].into()) => println!(),
						other => panic!("Cannot display value: {:?}", other),
					}
					return Ok(Some(left.clone()));
				}
			}
			_ => (),
		}
		Ok(None)
	}

	fn function<'a, 'b>(&self, _: &'b Program<'a>, _: &mut ExecutionContext<'a, 'b>,
	                    function: &Path<'a>, _: &[Value<'a>]) -> ExecutionResult<Option<Value<'a>>> {
		if function == &["std", "ios", "sync_with_stdio"].into() {
			Ok(Some(Value::Void))
		} else {
			Ok(None)
		}
	}

	fn method<'a, 'b>(&self, _: &'b Program<'a>, _: &mut ExecutionContext<'a, 'b>,
	                  target: &Value<'a>, method: &Identifier<'a>, _: &[Value<'a>])
	                  -> ExecutionResult<Option<Value<'a>>> {
		if target == &self.variables[&["std", "cin"].into()] {
			if method == &Identifier("tie") {
				return Ok(Some(Value::Void));
			}
		}
		Ok(None)
	}
}
