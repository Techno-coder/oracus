use oracus::node::Identifier;

fn main() {
	let text = r#"
using namespace std;

int modify(int& x) {
	x += 5;
	return x + 10;
}

int main() {
	int i = 0;
	for (; i < 10; ++i) {
		if (i == 5) {
			i = 0;
			break;
		}
	}

	while (i < 10) i++;
	auto value = modify(i);
	return i;
}
	"#;

	let program = match oracus::parser::parse(&text) {
		Ok(roots) => roots,
		Err(error) => {
			println!("{:?}", error.node);
			println!("{}", &text[error.span.0..]);
			return;
		}
	};

	let context = &mut oracus::execute::ExecutionContext::default();
	match program.functions[&oracus::node::Path::single(Identifier("main"))].first() {
		Some(function) => println!("{:?}", oracus::execute::function(&program, context, function, Vec::new())),
		_ => panic!("Invalid"),
	}
}
