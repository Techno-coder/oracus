fn main() {
	let text = r#"
using namespace std;

int main() {
	int i = 0;
	for (; i < 10; ++i)
		if (i == 5) break;

	while (i < 10) i++;
	return i;
}
	"#;

	let roots = match oracus::parser::parse(&text) {
		Ok(roots) => roots,
		Err(error) => {
			println!("{:?}", error.node);
			println!("{}", &text[error.span.0..]);
			return;
		}
	};

	let context = &mut oracus::execute::ExecutionContext::default();
	match &roots[1] {
		oracus::node::RootNode::Function(function) => {
			println!("{:?}", oracus::execute::function(context, function));
		}
		_ => panic!("Invalid"),
	}
}
