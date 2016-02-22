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
use std::vec::Vec;
use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::{Annotatable, ExtCtxt, SyntaxExtension};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::ToTokens;
use syntax::parse::token;
use syntax::ptr::P;

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

        // Get function return type
        let (ty, final_cb, inputs) = match dec.output.clone() {
            FunctionRetTy::Ty(ty) => {
                // Recreate the function declaration with an additional callback as an input
                // and a return type of ()
                let mut inputs = dec.inputs.clone();
                inputs.push(quote_arg!(cx, _final_callback: &Fn($ty)));

                (ty, true, inputs)
            }
            _ => (quote_ty!(cx, ()), false, dec.inputs.clone()),
        };

        let dec = cx.fn_decl(inputs, quote_ty!(cx, ()));

        let mut stmts = block.stmts.clone();
        // If there is a final expr, convert it to a return statement
        if let Some(expr) = block.expr.clone() {
            stmts.push(cx.stmt_expr(cx.expr(expr.span, ExprKind::Ret(Some(expr)))));
        }

        // Recursively modify statements
        let stmts = handle_statements(cx, stmts, final_cb);
        let block = cx.block(block.span, stmts, None);

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
fn handle_statements(cx: &ExtCtxt, stmts: Vec<Stmt>, final_cb: bool) -> Vec<Stmt> {
    // Try to take first statement from given statements
    // if there is no first statement we were given an empty vector
    // so we stop handeling statements
    if let Some((stmt, stmts_below)) = stmts.split_first() {
        let span = stmt.span;

        // Replace await calls inside statements and retrieve the function call
        // syntax that was inside them
        let (stmt_kind, async_functions) = match stmt.node.clone() {
            StmtKind::Decl(decl_span, node_id) => {
                match decl_span.node.clone() {
                    // Let declarations
                    DeclKind::Local(local) => {
                        if let (Some(expr),
                                PatKind::Ident(binding_mode, Spanned {node: ident, .. }, _)) =
                               (local.init.clone(), local.pat.node.clone()) {
                            // retrieve function call from inside awaits in expression
                            let (expr, async_functions) = handle_expr(cx, expr, false);

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

                            (stmt, async_functions)
                        } else {
                            // If we don't have an 'Ident' pattern there is nothing we can do
                            (StmtKind::Decl(decl_span, node_id), Vec::new())
                        }
                    }
                    // Const declarations
                    DeclKind::Item(item) => {
                        let (item, async_functions) = match item.node.clone() {
                            ItemKind::Const(ty, expr) => {
                                let (expr, async_functions) = handle_expr(cx, expr, false);
                                let item = cx.item_const(item.span, item.ident, ty, expr);
                                (item, async_functions)
                            }
                            _ => (item, Vec::new()),
                        };
                        (cx.stmt_item(decl_span.span, item).node, async_functions)
                    }
                }
            }
            StmtKind::Expr(expr, node_id) => {
                let (expr, async_functions) = handle_expr(cx, expr, final_cb);
                (StmtKind::Expr(expr, node_id), async_functions)
            }
            StmtKind::Semi(expr, node_id) => {
                let (expr, async_functions) = handle_expr(cx, expr, final_cb);
                (StmtKind::Semi(expr, node_id), async_functions)
            }
            StmtKind::Mac(mac, _, _) => {
                let (expr, async_functions) = handle_macro(cx, mac.span, mac.node.clone());
                (StmtKind::Expr(expr.clone(), expr.id), async_functions)
            }
        };

        let mut stmt = Spanned {
            node: stmt_kind,
            span: span,
        };

        let stmts_below = handle_statements(cx, stmts_below.to_vec(), final_cb);

        if async_functions.is_empty() {
            // Call the final callback if we are on the last statement
            if stmts_below.is_empty() {
                vec![stmt]
            } else {
                let mut stmts = Vec::new();
                stmts.push(stmt);
                stmts.append(&mut handle_statements(cx, stmts_below, final_cb));

                stmts
            }
        } else {
            // Wrap stmt in callbacks
            for (i, expr) in async_functions.iter().enumerate().rev() {
                let var_ident = cx.ident_of(&format!("_autogen{}", i));

                if let ExprKind::Call(func, args) = expr.node.clone() {
                    // Add callback as final argument
                    let mut args_with_cb = args.clone();

                    let mut stmts_in_cb = Vec::new();

                    // Call the final callback if we are on the last statement
                    if stmts_below.is_empty() {
                        stmts_in_cb.push(stmt);
                        // If this is the last callback we're processing we append the remaining statements
                    } else if i == async_functions.len() - 1 {
                        stmts_in_cb.push(stmt);
                        stmts_in_cb.append(&mut stmts_below.clone());
                    } else {
                        stmts_in_cb.push(stmt);
                    };

                    args_with_cb.push(quote_expr!(cx, &|$var_ident| {
						 $stmts_in_cb
					}));

                    stmt = cx.stmt_expr(cx.expr_call(span, func, args_with_cb));
                }
            }

            vec![stmt]
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
fn handle_expr(cx: &ExtCtxt, expr: P<Expr>, ret_to_cb: bool) -> (P<Expr>, Vec<Expr>) {
    let span = expr.span;

    // Search for await macros in all expression types
    match expr.node.clone() {
        ExprKind::Box(expr) => {
            let (expr, async_functions) = handle_expr(cx, expr, false);
            (cx.expr(span, ExprKind::Box(expr)), async_functions)
        }
        ExprKind::Ret(Some(expr)) => {
            if ret_to_cb {
                let (expr, async_functions) = handle_expr(cx, expr, false);
                (quote_expr!(cx, _final_callback($expr)), async_functions)
            } else {
                let (expr, async_functions) = handle_expr(cx, expr, false);
                (cx.expr(span, ExprKind::Ret(Some(expr))), async_functions)
            }
        }
        ExprKind::Ret(None) => {
            if ret_to_cb {
                (expr, Vec::new())
            } else {
                (quote_expr!(cx, _final_callback()), Vec::new())
            }
        }
        ExprKind::Tup(expressions) => {
            let mut all_async_functions = Vec::new();
            let mut processed_expressions = Vec::new();

            for expr in expressions {
                let (processed_expr, async_functions) = handle_expr(cx, expr, false);
                processed_expressions.push(processed_expr);
                all_async_functions.append(&mut async_functions.clone());
            }

            (cx.expr(span, ExprKind::Tup(processed_expressions)),
             all_async_functions)
        }
        ExprKind::Call(func, args) => {
            let mut all_async_functions = Vec::new();
            let mut processed_args = Vec::new();

            for expr in args {
                let (processed_expr, async_functions) = handle_expr(cx, expr, false);
                processed_args.push(processed_expr);
                all_async_functions.append(&mut async_functions.clone());
            }

            (cx.expr(span, ExprKind::Call(func, processed_args)),
             all_async_functions)
        }
        // 		ExprKind::InPlace(expr1, expr2) => {
        // 			ExprKind::InPlace(handle_expr(cx, expr1), handle_expr(cx, expr2))
        // 			let (expr1, async_functions) = handle_expr(cx, expr);
        // 			let (expr1, async_functions) = handle_expr(cx, expr);
        // 		}
        ExprKind::Mac(mac) => handle_macro(cx, span, mac.node),
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
        let (inner_tokens_expr, mut async_functions) = handle_expr(cx,
                                                                   quote_expr!(cx, $inner_tokens),
                                                                   false);

        let var_ident = cx.ident_of(&format!("_autogen{}", async_functions.len()));
        // Add this function to the list of function that will be converted to callbacks
        async_functions.push((*inner_tokens_expr).clone());

        (quote_expr!(cx, $var_ident), async_functions)
    } else {
        // Parse macro arguments into as a tuple
        // then search expressions inside tuple for await functions
        if let ExprKind::Tup(expressions) = quote_expr!(cx, ($inner_tokens)).node.clone() {
            let mut processed_expressions = Vec::new();
            let mut all_async_functions = Vec::new();

            for expr in expressions {
                let (expr, async_functions) = handle_expr(cx, expr, false);
                all_async_functions.append(&mut async_functions.clone());

                // Add comma if we already have and expression
                if !processed_expressions.is_empty() {
                    processed_expressions.push(TokenTree::Token(span, token::Token::Comma));
                }

                processed_expressions.append(&mut expr.to_tokens(cx).clone());
            }

            (quote_expr!(cx, $identifier!($processed_expressions)),
             all_async_functions)
        } else {
            (quote_expr!(cx, $identifier!($inner_tokens)), Vec::new())
        }
    }
}
