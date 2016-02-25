# Rust Async

Compiler extension for Rust that will enable async code without callbacks.

![travis build status](https://travis-ci.org/Arubaruba/rust-async.svg)


### Syntax
```rust
#[async]
fn simple_return() -> i32 {
	1
}

#[async]
fn foo() {
    let bar = await!(simple_return());
    println!("{}", bar);
}
```

### Generated Output

```rust
fn simple_return(_final_cb: &Fn(i32)) -> () {
    _final_cb(1);
}

fn foo() -> () {
    simple_return(&mut (|_cb1| {
        let bar = _cb1;
        bar.x;
    }));
}
```