//! Automatically generates callbacks under the hood
//! allowing for async-await style asynchronous programming
//!
//! ```rust
//! #[async]
//! fn get_user_id() -> Future<User> {
//! 		let user = await!(db.query("SELECT .."));
//! 		user.id
//! }
//!
//! #[async]
//! fn print_id() {
//! 		println!("user id: {}", await!(get_user_id()));
//! }
//!
//! ```
//!
//! Under the hood
//!


#![feature(quote, plugin_registrar, rustc_private, plugin, custom_attribute, advanced_slice_patterns, slice_patterns, box_patterns)]
#![crate_type = "dylib"]
// #![feature(box_syntax)]

extern crate syntax;
extern crate rustc_plugin;

use rustc_plugin::Registry;
use std::boxed::Box;
use syntax::ast::*;
use syntax::codemap::Span;
use syntax::ext::base::{Annotatable, ExtCtxt, SyntaxExtension};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::ToTokens;
use syntax::parse::token;

mod await_to_cb;

use await_to_cb::AwaitToCb;
use await_to_cb::ConversionSess;

#[plugin_registrar]
pub fn registrar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("async"),
                                  SyntaxExtension::MultiModifier(Box::new(async_attribute)));
}

/// Marking a function with this attribute allows await calls within it to be processed
fn async_attribute(cx: &mut ExtCtxt,
                   span: Span,
                   _: &MetaItem,
                   annotable: Annotatable)
                   -> Annotatable {

    // The function item
    let item = annotable.clone().expect_item();

    // We cannot simply modify the function item
    // the item, and several of its substructures are wrapped in syntax pointers (syntax::ptr::P)
    // structs wrapped in these pointers need to be recreated by the AstBuilder
    if let ItemKind::Fn(dec, unsafety, constness, abi, generics, block) = item.node
                                                                              .clone() {

		let stmt = quote_stmt!(cx, let (a, b) = (await!(simple_return()), await!(simple_return())););
		println!("{:?}", stmt.unwrap());
        // Get function return type
        let (final_cb, inputs) = match dec.output.clone() {
            FunctionRetTy::Ty(ty) => {
                // Recreate the function declaration with an additional callback as an input
                // and a return type of ()
                let mut inputs = dec.inputs.clone();
                inputs.push(quote_arg!(cx, __rust_async_autogen_final_callback: &Fn($ty)));

                (true, inputs)
            }
            _ => (false, dec.inputs.clone()),
        };

        let dec = cx.fn_decl(inputs, quote_ty!(cx, ()));
        // Recreate the function with the new declaration and a modified block
        let item_fn = ItemKind::Fn(dec,
                                   unsafety,
                                   constness,
                                   abi,
                                   generics,
                                   block.await_to_cb(&mut ConversionSess::new(cx, final_cb)));

        Annotatable::Item(cx.item(item.span.clone(),
                                  item.ident.clone(),
                                  item.attrs.clone(),
                                  item_fn))
    } else {
        cx.span_err(span, "The async annotation only works on functions.");
        annotable
    }
}