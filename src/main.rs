#![feature(plugin, custom_attribute)]
#![plugin(async)]

// Run the following command to view expanded code
// cargo rustc --bin async_tests -- --pretty=expanded
// -Z unstable-options -o target/debug/main.expanded.rs

use std::vec::Vec;

static mut race_i: i32 = 0;

// Create a race condition to make sure the code in the async
// function executes last
// #[test]
// fn async_works() {
// 	unsafe { foo(); }
// 	unsafe { race_i = 1 };
// }

// #[async]
// unsafe fn foo() -> Future<i32> {
// 	assert_eq!(1, race_i);
// 	race_i = 2;
// }

// let (user1, user2) = await!(db.get_user(1), b.get_user(2));


//trait Future {
//	fn parallel(callbacks: Vec<&Self>);
//}

#[async]
fn foo() -> i32 {
    let mut a = 1;
    a = 2;

	while false {
		let d = 1;
		let g = 1;
	}
    let c = 1;
	a = 5;
}

// fn foo() {
// 	let user1 = db.get_user(1);
// 	let user2 = db.get_user(2);
// 	let user = db.get_user();
// }
