use super::{AwaitToCb, ConversionSess};
use syntax::ast::*;
use syntax::ext::build::AstBuilder;
use syntax::codemap::Spanned;

impl AwaitToCb for Stmt {
    fn await_to_cb(self, con: &mut ConversionSess) -> Stmt {
        let stmt_kind = match self.node {
            StmtKind::Expr(expr, node_id) => StmtKind::Expr(expr.await_to_cb(con), node_id),
            StmtKind::Semi(expr, node_id) => StmtKind::Semi(expr.await_to_cb(con), node_id),
            StmtKind::Mac(mac, _, _) => {
                let expr = con.cx.expr(self.span, ExprKind::Mac((*mac).clone()));
                let node_id = expr.id;
                StmtKind::Expr(expr.await_to_cb(con), node_id)
            }
            StmtKind::Decl(decl, node_id) => {
                match decl.node.clone() {
                    DeclKind::Local(local) => {
                    	let pat = local.pat.clone();
                    	
                    	let stmt = match (local.ty.clone(), local.init.clone().await_to_cb(con)) {
                    		(None, None) => quote_stmt!(con.cx, let $pat),
                    		(Some(ty), None) => quote_stmt!(con.cx, let $pat:$ty),
                    		(None, Some(init)) => quote_stmt!(con.cx, let $pat = $init),
                    		(Some(ty), Some(init)) => quote_stmt!(con.cx, let $pat:$ty = $init)
                    	};
                    	
                    	stmt.unwrap().node.clone()
                    }
                    DeclKind::Item(_) => StmtKind::Decl(decl, node_id),
                }
            }
        };

        let mut stmt = Spanned {
            node: stmt_kind,
            span: self.span,
        };

		let num_await_functions = con.await_functions.len();

        for (i, await_function) in con.await_functions.drain(..).rev().enumerate() {
            // Statements that go inside callback closure
            let mut inner_stmts = vec![stmt.clone()];
            // If this is the last callback in the statement we
            // place the remaining statements below inside it
            if i == 0 {
				let mut con = ConversionSess {
					remaining_stmts: con.remaining_stmts.drain(..).collect(),
					await_functions: Vec::new(),
					cx: con.cx,
					final_cb: con.final_cb,
				};

				let mut remaining_stmts = Vec::new();

				while let Some(stmt) = con.remaining_stmts.pop() {
					remaining_stmts.push(stmt.await_to_cb(&mut con));
				}

                inner_stmts.extend_from_slice(&mut remaining_stmts);
            }
            
            let var_ident = con.cx.ident_of(&format!("__rust_async_autogen_callback{}", num_await_functions - i));

            // Create callback closure
            let callback =
                quote_expr!(con.cx, &mut |$var_ident| {$inner_stmts});

            // Add the callback to the await functions arguments then converted
            // the function into a statement
            match await_function {
                ExprKind::Call(func, mut args) => {
                    args.push(callback);
                    stmt = con.cx.stmt_expr(con.cx.expr(self.span, ExprKind::Call(func, args)));
                }
                ExprKind::MethodCall(ident, func, mut args) => {
                    args.push(callback);
                    stmt = con.cx.stmt_expr(con.cx.expr(self.span,
                                                        ExprKind::MethodCall(ident, func, args)));
                }
                _ => {
                    con.cx.span_err(self.span,
                                    "Error creating callbacks - wrong expr kind in await_functions")
                }
            }
        }

        stmt
    }
}
