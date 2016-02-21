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
use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::{Annotatable, ExtCtxt, SyntaxExtension, MacResult, DummyResult, MacEager};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::ToTokens;
use syntax::parse::token;
use syntax::ptr::P;
use std::vec::Vec;
use std::iter::Iterator;

use std::boxed::Box;

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
        // Recursively modify statements
        let stmts = handle_statements(cx, block.stmts.clone());
        let block = cx.block(block.span, stmts, block.expr.clone());

        // Get function return type
        let ty = match dec.output.clone() {
            FunctionRetTy::Ty(ty) => ty,
            _ => quote_ty!(cx, ()),
        };


        // Recreate the function declaration with an additional callback as an input
        // and a return type of ()
        let mut inputs = dec.inputs.clone();
        inputs.push(quote_arg!(cx, _gen_async_fn_final_callback: &FnOnce($ty)));

        let dec = cx.fn_decl(inputs, quote_ty!(cx, ()));

        // Recreate the function with the new declaration and a modified block
        let item_fn = ItemKind::Fn(dec, unsafety, constness, abi, generics, block);

        Annotatable::Item(cx.item(item.span.clone(),
                                  item.ident.clone(),
                                  item.attrs.clone(),
                                  item_fn))
    } else {
        cx.span_err(span, "The async annotation only works on functions.");
        annotable
    }
}

/// Convert statements that contain the await function into callbacks
fn handle_statements(cx: &ExtCtxt, stmts: Vec<Stmt>) -> Vec<Stmt> {
    // Try to take first statement from given statements
    // if there is no first statement we were given an empty vector
    // so we stop handeling statements
    if let Some((stmt_span, stmts_below)) = stmts.split_first() {
        // Replace await calls inside statements and retrieve the function call
        // syntax that was inside them

        let (stmt, await_functions) = match stmt_span.node.clone() {
            StmtKind::Decl(decl_span, node_id) => {
                match decl_span.node.clone() {
                    // Let declarations
                    DeclKind::Local(local) => {
                        if let (Some(expr),
                                PatKind::Ident(binding_mode, Spanned {node: ident, .. }, _)) =
                               (local.init.clone(), local.pat.node.clone()) {
                            // retrieve function call from inside awaits in expression
                            let (expr, await_functions) = handle_expression(cx, expr);

                            // Is the variable mutable?
                            let mutable = match binding_mode {
                                BindingMode::ByRef(Mutability::Mutable) => true,
                                BindingMode::ByValue(Mutability::Mutable) => true,

                                BindingMode::ByRef(Mutability::Immutable) => false,
                                BindingMode::ByValue(Mutability::Immutable) => false,
                            };

                            // Does the declaration specify a type?
                            // Here we finally create the new statements
                            let stmt = match local.ty.clone() {
                                Some(ty) => {
                                    cx.stmt_let_typed(decl_span.span,
                                                      mutable,
                                                      ident.clone(),
                                                      ty,
                                                      expr)
                                      .node
                                      .clone()
                                }
                                None => {
                                    cx.stmt_let(decl_span.span, mutable, ident.clone(), expr)
                                      .node
                                      .clone()
                                }
                            };

                            (stmt, await_functions)
                        } else {
                            // If we don't have an 'Ident' pattern there is nothing we can do
                            (StmtKind::Decl(decl_span, node_id), Vec::new())
                        }
                    }
                    // Const declarations
                    DeclKind::Item(item) => {
                        let (item, await_functions) = match item.node.clone() {
                            ItemKind::Const(ty, expr) => {
                                let (expr, await_functions) = handle_expression(cx, expr);
                                let item = cx.item_const(item.span, item.ident, ty, expr);
                                (item, await_functions)
                            }
                            _ => (item, Vec::new()),
                        };
                        (cx.stmt_item(decl_span.span, item).node, await_functions)
                    }
                }
            }
            StmtKind::Expr(expr, node_id) => {
                let (expr, await_functions) = handle_expression(cx, expr);
                (StmtKind::Expr(expr, node_id), await_functions)
            }
            StmtKind::Semi(expr, node_id) => {
                let (expr, await_functions) = handle_expression(cx, expr);
                (StmtKind::Semi(expr, node_id), await_functions)
            }
            StmtKind::Mac(mac, _, _) => {
                let (expr, await_functions) = handle_macro(cx, mac.span, mac.node.clone());
                (StmtKind::Expr(expr.clone(), expr.id), await_functions)
            }
        };

        if await_functions.is_empty() {
            // TODO add callbacks
            let mut stmts = Vec::new();
            stmts.push(Spanned {
                node: stmt,
                span: stmt_span.span,
            });
            stmts.append(&mut handle_statements(cx, stmts_below.to_vec()));

            stmts
        } else {
            let mut stmts = Vec::new();
            stmts.push(Spanned {
                node: stmt,
                span: stmt_span.span,
            });
            stmts.append(&mut handle_statements(cx, stmts_below.to_vec()));

            stmts
        }
    } else {
        Vec::new()
    }
}



/// Takes and expression and if it's an await call it adds
/// it to a vector in which all async function call syntax is are stored.
/// If the expression is not an await call it checks expressions
/// within the expression for the same (if the ExprKind has any)
///
/// Returns a tuple containing the modified expression and a vector of
/// all found async functions
fn handle_expression(cx: &ExtCtxt, expr: P<Expr>) -> (P<Expr>, Vec<Expr>) {

    // (quote_expr!(cx, [$expr]), Vec::new())
    match expr.node.clone() {
        // await may still be in macro form or already converted
        // (depends on it's location in code)
        ExprKind::Mac(mac) => handle_macro(cx, mac.span, mac.node),
//        ExprKind::Call(_, _) => (quote_expr!(cx, call1), Vec::new()),
        _ => (expr, Vec::new()),
    }
}

fn handle_macro(cx: &ExtCtxt, span: Span, mac: Mac_) -> (P<Expr>, Vec<Expr>) {
    // Macro should always have one path segment
    let identifier = mac.path.segments.first().unwrap().identifier;

    let inner_tokens = mac.tts.clone();
    // If this macro in our await macro we completely replace it with a variable
    // if not, we put the variable in the foreign macro wherever there are await macros within it
    if cx.ident_of("await").name == identifier.name {
        // The inner function must also be checked for "await"
        let (inner_tokens_expr, mut await_functions) =
            handle_expression(cx, quote_expr!(cx, $inner_tokens));
        // Add this function to the list of function that will be converted to callbacks
        await_functions.push((*inner_tokens_expr).clone());

        let var_ident = cx.ident_of(&format!("_autogen{}", await_functions.len()));

        (quote_expr!(cx, $var_ident), await_functions)
    } else {
        // Parse macro arguments into as a tuple
        // then search expressions inside tuple for await functions
        if let ExprKind::Tup(expressions) = quote_expr!(cx, ($inner_tokens)).node.clone() {
        	let mut processed_expressions = Vec::new();
        	let mut all_await_functions = Vec::new();

			for expr in expressions {
				let (expr, await_functions) = handle_expression(cx, expr);
				all_await_functions.append(&mut await_functions.clone());

				// Add comma if we already have and expression
				if !processed_expressions.is_empty() {
					processed_expressions.push(TokenTree::Token(span, token::Token::Comma));
				}

				processed_expressions.append(&mut expr.to_tokens(cx).clone());
			}

            (quote_expr!(cx, $identifier!($processed_expressions)), Vec::new())
        } else {
            (quote_expr!(cx, $identifier!($inner_tokens)), Vec::new())
        }
    }
}
