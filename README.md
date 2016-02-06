# Rust Async

Compiler extension for rust that will enable async code without callbacks.

### Proposed Syntax
```rust
#[async]
fn foo() {
    let bar = await!(db.get_bar());
	println!(bar.x);
}
```