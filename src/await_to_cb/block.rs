use super::{AwaitToCb, ConversionSess};
use syntax::ext::build::AstBuilder;
use syntax::ast::{Block, ExprKind};
use syntax::ptr::P;
use std::vec::Vec;

impl AwaitToCb for P<Block> {
    fn await_to_cb<'a>(self,mut con: &'a mut ConversionSess) -> P<Block> {
        // Convert all statements inside the block
        let mut stmts = self.stmts.clone();
        // If there is an extra expression at the end of the block
        // it is converted into a return statement
        if let Some(expr) = self.expr.clone() {
            let expr = con.cx.stmt_expr(con.cx.expr(expr.span, ExprKind::Ret(Some(expr))));
            stmts.push(expr.await_to_cb(con));
        }
	
		// Since elements will be popped from the vector, the last ones will be handled first
		stmts.reverse();
		
		
		// Create new con
		let mut con = ConversionSess::new(con.cx, con.final_cb);
		con.remaining_stmts = stmts;

        let mut stmts = Vec::new();

		while let Some(stmt) = con.remaining_stmts.pop() {
			stmts.push(stmt.clone().await_to_cb(&mut con));
		}

		// If no callback was triggered we return the remaining stmts to the super conversion sess
//		con.remaining_stmts.append(&mut sub_con.remaining_stmts.drain(..).rev().collect());


        // Deref remaining stmts (if no callbacks were created) and collect
        //        let stmts: Vec<Stmt> = con.remaining_stmts.map(|stmt| (*stmt).clone()).collect();

        con.cx.block(self.span, stmts, None)
    }
}
