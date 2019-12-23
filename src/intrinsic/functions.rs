use std::collections::HashMap;

use crate::node::{Path, Type};
use crate::value::{Structure, Value};

pub fn variable(variables: &mut HashMap<Path, Value>, identifier: &'static str) {
	let structure = Structure::new(Type::single(["std", identifier].into()));
	variables.insert(["std", identifier].into(), Value::Structure(structure));
}
