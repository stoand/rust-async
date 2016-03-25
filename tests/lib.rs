#![feature(plugin, custom_attribute, box_syntax)]
#![plugin(async)]
// Run the following command to view expanded code:
// cargo rustc --bin async_tests -- --pretty=expanded
// -Z unstable-options -o target/debug/main.expanded.rs

#[async]
fn simple_return() -> i32 {
	1
}

#[test]
#[async]
fn test_simple_return() {
    assert_eq!(await!(simple_return()), 1);
}

#[async]
#[allow(unused_variables)]
fn no_return() {
    let a = 1;
}

#[test]
// Async functions that don't return anything
// may be called within synchronous functions
fn test_no_return_in_sync() {
    assert_eq!(no_return(), ());
}

#[test]
// Sync functions may call async functions with return
// types by providing a callback
fn test_simple_return_in_sync() {
    simple_return(&|val| assert_eq!(val, (1)));
}


#[async]
fn add_one(i: i32) -> i32 {
	i + 1
}

#[test]
#[async]
// The innermost await statement will be processed first
fn test_nested_awaits() {
    assert_eq!(await!(add_one(await!(add_one(0)))), 2);
}

fn borrow_add_one(i: &mut i32) {
    *i += 1;
}

#[test]
fn test_borrow_mut() {
    let mut i = 0;
    borrow_add_one(&mut i);
    borrow_add_one(&mut i);

    assert_eq!(i, 2)
}

#[test]
#[async]
fn test_tup() {
	let (a, b) = (await!(simple_return()), await!(simple_return()));
	assert_eq!(a, b);
}
