#![feature(plugin, custom_attribute, box_syntax)]
#![plugin(async)]
// Run the following command to view expanded code:
// cargo rustc --bin async_tests -- --pretty=expanded
// -Z unstable-options -o target/debug/main.expanded.rs

// #[cfg(test)]

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
/// Async functions that don't return anything
/// may be called within synchronous functions
fn test_no_return_in_sync() {
    assert_eq!(no_return(), ());
}

#[test]
/// Async functions that don't return anything
/// may be called within synchronous functions
fn test_simple_return_in_sync() {
    simple_return(&|val| assert_eq!(val, (1)));
}
