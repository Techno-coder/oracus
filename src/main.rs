fn main() {
	let text = r#"
#include <iostream>
#include <utility>

using namespace std;

int main() {
	int i = 0;
}
	"#;

	let _program = oracus::parser::parse(&text)
		.map_err(|error| oracus::span::emit(text, error)).unwrap();
}
