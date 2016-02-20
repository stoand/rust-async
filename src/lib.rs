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
/*
Under the hood
*/

#![feature(quote, plugin_registrar, rustc_private, plugin, custom_attribute, advanced_slice_patterns, slice_patterns)]
#![crate_type = "dylib"]

extern crate syntax;
extern crate rustc_plugin;

pub mod future;

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

/// It is more natural for await to marked for later processing
/// by a macro instead of a function with an arbirary name.
/// Macros are processed before syntax extensions however, so we 
/// need to define this macro and so it can replace itself with a
/// specifically named function which the #[async] syntax extension can process
fn await_mac_to_fn(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {

    if args.len() != 2 {
        cx.span_err(
            sp,
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
//	let async_uses : Vec<Expr>  = handle_expressions(cx, expr);

	// Try to take first statement from given statements
	// if there is no first statement we were given an empty vector
	// so we stop handeling statements
    if let Some((stmt_span, stmts_below)) = stmts.split_first() {
		// Replace await calls inside statements and retrieve the function call
		// syntax that was inside them
        let (stmt, await_functions) = match stmt_span.node.clone() {
        	StmtKind::Decl(decl_span, node_id) => {
        		// Only support "local" declarations for now
//        			Spanned(DeclKind::Local(), span) => 
        		let decl = if let DeclKind::Local (local_decl) = decl_span.node.clone() {
					if let Some(expr) = local_decl.init.clone() {
						// Todo 
//						DeclKind::Local(quote_local!(cx, Local {init: Some(cx.expr(decl_span.span, expr.node)), .. local_decl}))
						decl_span.node.clone()
					} else {
						decl_span.node.clone()
					}
        		} else {
        			decl_span.node.clone()
        		};

        		(StmtKind::Decl(decl_span, node_id), Vec::new())
        	},
        	StmtKind::Expr(expr, node_id) => {
        		let (expr, await_functions) = handle_expression(cx, &expr);
        		(StmtKind::Expr(cx.expr(expr.span, expr.node.clone()), node_id), await_functions)
        	},
        	StmtKind::Semi(expr, node_id) => {
        		let (expr, await_functions) = handle_expression(cx, &expr);
        		(StmtKind::Semi(cx.expr(expr.span, expr.node.clone()), node_id), await_functions)
        	},
        	other @ _ => (other, Vec::new()), 
        };

		if await_functions.is_empty() {
			vec![Spanned { node: stmt, span: stmt_span.span}]
		} else {
			// TODO add callbacks
			Vec::new()
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
fn handle_expression<'a>(cx: &ExtCtxt, expr: &'a Expr) -> (&'a Expr, Vec<Expr>) {
	(&expr, vec![])
}


/// Convert statements that contain the await function into callbacks
fn handle_statements_old(cx: &ExtCtxt, stmts: Vec<Stmt>) -> Vec<Stmt> {
	return vec![];
    if let Some((stmt, stmts_below)) = stmts.split_first() {
        // We only check for await in declaration statments
        // TODO check for await in other places
        if let StmtKind::Decl(_, _) = stmt.node.clone() {
            // If this is the last async statement we invoke the Future's callback
            let stmts_inside_cb = if stmts_below.is_empty() {
                vec![quote_stmt!(cx,
                                 _gen_async_fn_final_callback({
                                     1234
                                 }))
                         .unwrap()]
            } else {
                handle_statements(cx, stmts_below.to_vec())
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
            let stmt: Stmt = match stmt.node.clone() {
                StmtKind::Expr(expr, _) => cx.stmt_expr(handle_expression1(cx, expr)),
                StmtKind::Semi(expr, _) => cx.stmt_expr(handle_expression1(cx, expr)),
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
                true => vec![quote_stmt!(cx, _gen_async_fn_final_callback({$stmt})).unwrap()],
            }
        }
    } else {
        vec![]
    }
}

fn handle_expression1(cx: &ExtCtxt, expr: P<Expr>) -> P<Expr> {
    let node = match expr.node.clone() {
        ExprKind::While(expr, block, indent) => {
            ExprKind::While(expr,
                            cx.block(block.span,
                                     handle_statements(cx, block.stmts.clone()),
                                     block.expr.clone()),
                            indent)
        }
        ExprKind::Call(func, args) => {
            // if quote_path!(cx, async) == func.node.clone() {
            //     println!("ok");
            // }

            ExprKind::Call(func, args)
        }
        n @ _ => n.clone(),
    };

    cx.expr(expr.span, node)
}