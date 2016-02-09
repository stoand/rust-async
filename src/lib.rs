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

#![feature(quote, plugin_registrar, rustc_private, plugin, custom_attribute, advanced_slice_patterns, slice_patterns)]
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
        // Recursively modify statements
        let stmts = handle_statements(cx, block.stmts.clone());
        let block = cx.block(block.span, stmts, block.expr.clone());

        let ty = match dec.output.clone() {
            ast::FunctionRetTy::Return(ty) => ty,
            _ => quote_ty!(cx, ()),
        };

        let mut inputs = dec.inputs.clone();
        inputs.push(quote_arg!(cx, cb: &FnOnce($ty)));
        let dec = cx.fn_decl(inputs, quote_ty!(cx, ()));

        let item_fn = ast::Item_::ItemFn(dec, unsafety, constness, abi, generics, block);

        // cx.span_err(span, "read printed stuff");
        Annotatable::Item(cx.item(item.span.clone(),
                                  item.ident.clone(),
                                  item.attrs.clone(),
                                  item_fn))
    } else {
        cx.span_err(span, "The async annotation only works on functions.");
        annotable
    }
}

/// Convert statements that contain the await! macro into callbacks
fn handle_statements(cx: &ExtCtxt, stmts: Vec<P<ast::Stmt>>) -> Vec<P<ast::Stmt>> {
    if let Some((stmt, stmts_below)) = stmts.split_first() {
        // We only check for await in declaration statments
        // TODO check for await in other places
        if let ast::Stmt_::StmtDecl(_, _) = stmt.node.clone() {
            // If this is the last async statement we invoke the Future's callback
            let stmts_inside_cb = if stmts_below.is_empty() {
                handle_statements(cx, stmts_below.to_vec())
            } else {
                vec![
//                quote_stmt!(cx,
//                                 cb({
//                                     1234
//                                 }))
//                         .unwrap()
                         ]
            };

            vec![quote_stmt!(cx, {
				$stmt
				if (true) {
					$stmts_inside_cb
				}
            	})
                     .unwrap()]
        } else {
            // An expression statement may contain statements within itself depending
            // on the expression type
            let stmt: P<ast::Stmt> = match stmt.node.clone() {
                ast::Stmt_::StmtExpr(expr, _) => cx.stmt_expr(handle_expression(cx, expr)),
                ast::Stmt_::StmtSemi(expr, _) => cx.stmt_expr(handle_expression(cx, expr)),
                _ => stmt.clone(),
            };

            // No await macro found, carry on normally and look for more await! macros
            match stmts_below.is_empty() {
                false => {
                    let mut stmts = Vec::new();
                    stmts.push(stmt.clone());
                    stmts.extend(handle_statements(cx, stmts_below.to_vec()));

                    stmts
                }
                true => vec![quote_stmt!(cx, cb({$stmt})).unwrap()],
            }
        }
    } else {
        vec![]
    }
}

fn handle_expression(cx: &ExtCtxt, expr: P<ast::Expr>) -> P<ast::Expr> {
    use syntax::ast::Expr_::*;

    let node = match expr.node.clone() {
        // 		ExprWhile(expr, block, indent) => {
        // 			ExprWhile(expr, cx.block(), indent);
        // 		},
        n @ _ => n.clone(),
    };

    cx.expr(expr.span, node)
}
