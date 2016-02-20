#![feature(plugin, custom_attribute)]
#![plugin(async)]
// Run the following command to view expanded code
// cargo rustc --bin async_tests -- --pretty=expanded
// -Z unstable-options -o target/debug/main.expanded.rs

fn main() {}

#[async]
fn foo() -> Option<i32> {
//    let a = await!(test(1));
}

#[test]
fn test_async_fn_return_removed() {
	assert_eq!(foo(), ());
}

