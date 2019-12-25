use crate::execute::{ExecutionContext, ExecutionResult, Reference};
use crate::node::{Identifier, Intrinsic, Program, Type};
use crate::span::Spanned;
use crate::symbol::SymbolContext;
use crate::value::{Structure, Value};

/// Intrinsic for the utility header.
#[derive(Debug)]
pub struct Utility;

impl Intrinsic for Utility {
	fn register<'a>(&self, context: &mut SymbolContext<'a>) {
		context.structures.insert(["std", "pair"].into());
	}

	fn assign<'a, 'b>(&self, _: &'b Program<'a>, context: &mut ExecutionContext<'a, 'b>,
	                  target: &Spanned<Reference<'a>>, value: &Spanned<Value<'a>>)
	                  -> ExecutionResult<Option<Value<'a>>> {
		let (target, structure) = context.dereference(target)?;
		if let Type::Concrete(path, _) = structure {
			if path == &["std", "pair"].into() {
				pair_assign(target, &structure, &value.node);
				return Ok(Some(target.clone()));
			}
		}
		Ok(None)
	}

	fn construct<'a, 'b>(&self, _: &'b Program<'a>, _: &mut ExecutionContext<'a, 'b>,
	                     structure: &Spanned<Type<'a>>, arguments: &[Value<'a>])
	                     -> ExecutionResult<Option<Value<'a>>> {
		if let Type::Concrete(path, templates) = &structure.node {
			if path == &["std", "pair"].into() {
				if arguments.is_empty() {
					let structure = Type::single(["std", "pair"].into());
					let mut structure = Structure::new(structure);
					structure.fields.insert(Identifier("first"),
						(Value::Uninitialised, templates[0].clone()));
					structure.fields.insert(Identifier("second"),
						(Value::Uninitialised, templates[1].clone()));
					return Ok(Some(Value::Structure(structure)));
				}
			}
		}
		Ok(None)
	}
}

fn pair_assign<'a>(target: &mut Value<'a>, structure: &Type<'a>, value: &Value<'a>) {
	if let Type::Concrete(path, _) = structure {
		if path == &["std", "pair"].into() {
			if let Value::List(list) = value {
				let structure = target.structure();
				if list.len() != 2 { panic!("List must be pair of elements"); }
				let (first, first_type) = &mut structure[&Identifier("first")];
				pair_assign(first, first_type, &list[0]);
				let (second, second_type) = &mut structure[&Identifier("second")];
				pair_assign(second, second_type, &list[1]);
				return;
			}
		}
	}
	*target = value.clone();
}
