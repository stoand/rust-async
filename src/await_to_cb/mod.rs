use std::iter::Iterator;
use std::slice;
use std::vec::Vec;
use syntax::ast::*;
use syntax::ext::base::ExtCtxt;

mod stmt;
mod block;
mod expr;

pub struct ConversionSess<'a> {
    pub cx: &'a ExtCtxt<'a>,
    /// Function returns something
    /// if it doesn't return anything we don't give it a final callback
    pub final_cb: bool,
    pub await_functions: Vec<ExprKind>,
    /// When a callback is triggered every statement
    /// below it needs to go inside the generated closure
    pub remaining_stmts: Vec<Stmt>,
}

impl<'a> ConversionSess<'a> {
	pub fn new(cx: &'a ExtCtxt<'a>, final_cb: bool) -> Self {
		ConversionSess {cx: cx, final_cb: final_cb, await_functions: Vec::new(), remaining_stmts: Vec::new()}
	}
}

/// Converts await macros contained within itself to callbacks
pub trait AwaitToCb {
    fn await_to_cb(self, con: &mut ConversionSess) -> Self;
}

impl<T> AwaitToCb for Option<T>
    where T: AwaitToCb
{
    fn await_to_cb(self: Option<T>, con: &mut ConversionSess) -> Option<T> {
        self.map_or(None, |expr_kind| Some(expr_kind.await_to_cb(con)))
    }
}

impl<T> AwaitToCb for Vec<T>
    where T: AwaitToCb
{
    fn await_to_cb(self, con: &mut ConversionSess) -> Vec<T> {
        self.into_iter().map(|item| item.await_to_cb(con)).collect()
    }
}
