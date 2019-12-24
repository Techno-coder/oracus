use oracus::execute::{self, ExecutionContext};
use oracus::intrinsic;
use oracus::node::{Identifier, Path};

fn main() {
	let text = r#"
#include <iostream>

using namespace std;

int& modify(int& x) {
	x += 5;
	return x;
}

int& bad() {
	int i = 0;
	return i;
}

int main() {
	cin.tie(0);
	ios::sync_with_stdio(false);
	cout << bad() << endl;

	int i = 0;
	for (; i < 10; ++i) {
		if (i == 5) {
			i = 0;
			break;
		}
	}

	while (i < 10) i++;
	auto& value = modify(i);
	value += 3;
	return i;
}
	"#;

	let mut program = oracus::parser::parse(&text)
		.map_err(|error| oracus::span::emit(text, error)).unwrap();
	program.intrinsics.push(Box::new(intrinsic::Stream::new()));
	let main = Path::single(Identifier("main"));

	let context = &mut ExecutionContext::default();
	let function = program.functions[&main].first().unwrap();
	println!("{:?}", execute::function(&program, context, function, Vec::new())
		.map_err(|error| oracus::span::emit(text, error)));
}
