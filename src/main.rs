#![feature(plugin, custom_attribute)]
#![plugin(async)]

// Run the following command to view expanded code
// cargo rustc --bin async_tests -- --pretty=expanded -Z unstable-options -o target/debug/main.expanded.rs

mod future;

use future::Future;

static mut race_i : i32 = 0;

// Create a race condition to make sure the code in the async
// function executes last
#[test]
fn async_works() {
	unsafe { foo(); }
	unsafe { race_i = 1 };
}

#[async]
unsafe fn foo() -> Future<i32> {
	assert_eq!(1, race_i);
	race_i = 2;
}
