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

use std::boxed::Box;

#[plugin_registrar]
pub fn registrar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("async"),
                                  SyntaxExtension::MultiModifier(Box::new(async_attribute)));

    reg.register_macro("await", await_mac_to_fn);
}

/* FIXME: allows users to get a concrete error when a macro is not properly processed by #[async]*/

/// Await is more legible for programmers if it is marked
/// by a macro instead of a function with an arbirary name.
/// Macros are processed before syntax extensions however, so we 
/// need to define this macro and so it can replace itself with a
/// specifically named function which the #[async] syntax extension can process
fn await_mac_to_fn(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {

    if args.len() != 2 {
        cx.span_err(sp,
                    &"Argument should be a function name with parameters, ie: test(1, 2)");
        return DummyResult::any(sp);
    }

    let fn_name = args[0].clone();
    let fn_params = args[1].clone();

    MacEager::expr(quote_expr!(cx, __AWAIT_($fn_name$fn_params)))
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
                            let (expr, await_functions) = handle_expression(cx, &expr);

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
                                let (expr, await_functions) = handle_expression(cx, &expr);
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
                let (expr, await_functions) = handle_expression(cx, &expr);
                (StmtKind::Expr(expr, node_id), await_functions)
            }
            StmtKind::Semi(expr, node_id) => {
                let (expr, await_functions) = handle_expression(cx, &expr);
                (StmtKind::Semi(expr, node_id), await_functions)
            }
            other @ _ => (other, Vec::new()), 
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
fn handle_expression<'a>(cx: &ExtCtxt, expr: &'a Expr) -> (P<Expr>, Vec<Expr>) {
    let expr = cx.expr(expr.span, expr.node.clone());
    (quote_expr!(cx, ($expr)), vec![])
}
