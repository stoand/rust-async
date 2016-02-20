# Rust Async

Compiler extension for rust that will enable async code without callbacks.

![travis build status](https://travis-ci.org/Arubaruba/rust-async.svg)

### Proposed Syntax
```rust
#[async]
fn foo() {
    let bar = await!(db.get_bar());
	println!(bar.x);
}
```
