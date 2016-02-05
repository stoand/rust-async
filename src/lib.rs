#![feature(quote, plugin_registrar, rustc_private, plugin, custom_attribute)]
#![crate_type = "dylib"]

extern crate syntax;
extern crate rustc_plugin;

use rustc_plugin::Registry;
use syntax::ext::base::{Annotatable, ExtCtxt, SyntaxExtension};
use syntax::codemap::Span;
use syntax::ast;
use syntax::parse::token;
use std::boxed::Box;
use syntax::feature_gate::AttributeType;

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
//
//    match annotable {
//        Annotatable::Item(item) => {
//            Annotatable::Item(quote_item!(cx,
//            fn foo() -> u32 {
//                let sum = 2 + 2;
//                println!("{}", sum);
//                sum
//            })
//                                  .unwrap())
//        }
//        other => {
//            cx.span_err(span, "did not get item");
//            other.clone()
//        }
//    };


    Annotatable::Item(quote_item!(cx,
            fn foo() -> u32 {
                let sum = 2 + 2;
                println!("{}", sum);
                sum
            })
                          .unwrap())
}