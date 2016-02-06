//! Automatically generates callbacks under the hood
//! allowing for async-await style asynchronous programming
//!
//! ```rust
//! #[async]
//! fn get_user_id() -> Future<User> {
//!		let user = await!(db.query("SELECT .."));
//!		user.id
//! }
//!
//! #[async]
//! fn print_id() {
//!		println!("user id: {}", await!(get_user_id()));
//! }
//! 
//! ```

#![feature(quote, plugin_registrar, rustc_private, plugin, custom_attribute)]
#![crate_type = "dylib"]

extern crate syntax;
extern crate rustc_plugin;

pub mod future;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{Annotatable, ExtCtxt, SyntaxExtension};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::ToTokens;
use syntax::parse::token;
use syntax::ptr::P;

use std::vec::Vec;
use std::boxed::Box;

#[plugin_registrar]
pub fn registrar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("async"),
                                  SyntaxExtension::MultiModifier(Box::new(async_attribute)));
}

fn async_attribute(cx: &mut ExtCtxt,
                   span: Span,
                   _: &ast::MetaItem,
                   annotable: Annotatable)
                   -> Annotatable {

    // The function item
    let item = annotable.clone().expect_item();

    // We cannot simply modify the function item
    // the item, and several of its substructures are wrapped in syntax pointers (syntax::ptr::P)
    // structs wrapped in these pointers need to be recreated by the AstBuilder
    if let ast::Item_::ItemFn(dec, unsafety, constness, abi, generics, block) = item.node.clone() {
        let block = convert_block(cx, block);
        let item_fn = ast::Item_::ItemFn(dec, unsafety, constness, abi, generics, block);

        Annotatable::Item(cx.item(item.span.clone(),
                                  item.ident.clone(),
                                  item.attrs.clone(),
                                  item_fn))
    } else {
        cx.span_err(span, "The async annotation only works on functions.");
        annotable
    }
}

fn convert_block(cx: &ExtCtxt, block: P<ast::Block>) -> P<ast::Block> {
    let stmts = replace_faux_macro_await(cx, block.stmts.clone());

    let expr = quote_expr!(cx,
                           Box::new(move |callback: &Fn(i32)| {
                           		$stmts
                               callback(1);
                           })
    );

    cx.block_expr(expr)
}

fn replace_faux_macro_await(cx: &ExtCtxt, stmts: Vec<P<ast::Stmt>>) -> Vec<P<ast::Stmt>> {
	 for stmt in stmts.clone() {
//		 if let ast::Stmt_::StmtDecl(dec, _) = stmt.node.clone() {
//
//		 }
	 }
    stmts
}