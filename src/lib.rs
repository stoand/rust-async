#![feature(quote, plugin_registrar, rustc_private, plugin, custom_attribute)]
#![crate_type = "dylib"]

extern crate syntax;
extern crate rustc_plugin;

use rustc_plugin::Registry;
use syntax::ext::base::{Annotatable, ExtCtxt, SyntaxExtension};
use syntax::codemap::Span;
use syntax::parse::token;
use std::boxed::Box;
use syntax::feature_gate::AttributeType;
use syntax::ast;
use syntax::ptr::P;
use syntax::ext::build::AstBuilder;
use std::vec::Vec;

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

    if let ast::Item_::ItemFn(dec, unsafety, constness, abi, generics, block) = item.node.clone() {
        let block = cx.block(block.span, generate_callbacks(block.stmts.clone()), block.expr.clone());

        Annotatable::Item(cx.item(item.span.clone(),
                                  item.ident.clone(),
                                  item.attrs.clone(),
                                  ast::Item_::ItemFn(dec, unsafety, constness, abi, generics, block)))
    } else {
        cx.span_err(span, "The async annotation only works on functions.");
        annotable
    }
}

fn generate_callbacks(stmts: Vec<P<ast::Stmt>>) -> Vec<P<ast::Stmt>> {
    vec![]
}
