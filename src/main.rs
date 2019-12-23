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

int main() {
	cin.tie(0);
	ios::sync_with_stdio(false);

	string test;
	cin >> test;
	cout << test << endl;

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

	let mut program = oracus::parser::parse(&text).unwrap();
	program.intrinsics.push(Box::new(intrinsic::Stream::new()));
	let main = Path::single(Identifier("main"));

	let context = &mut ExecutionContext::default();
	let function = program.functions[&main].first().unwrap();
	println!("{:?}", execute::function(&program, context, function, Vec::new()));
}
