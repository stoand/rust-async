use syntax::ext::quote::rt::ToTokens;
use super::{AwaitToCb, ConversionSess};
use syntax::ext::build::AstBuilder;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::parse::token;

impl AwaitToCb for P<Expr> {
    fn await_to_cb(self, con: &mut ConversionSess) -> Self {
        let Expr {node: expr_kind, span, ..} = (*self).clone();

        let expr_kind = match expr_kind {
            ExprKind::Box(expr) => ExprKind::Box(expr.await_to_cb(con)),
            ExprKind::InPlace(expr1, expr2) => {
                ExprKind::InPlace(expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::Vec(exprs) => ExprKind::Vec(exprs.await_to_cb(con)),
            ExprKind::Call(func, args) => ExprKind::Call(func, args.await_to_cb(con)),
            ExprKind::MethodCall(ident, func, args) => {
                ExprKind::MethodCall(ident, func, args.await_to_cb(con))
            }
            ExprKind::Tup(exprs) => ExprKind::Tup(exprs.await_to_cb(con)),
            ExprKind::Binary(op, expr1, expr2) => {
                ExprKind::Binary(op, expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::Unary(un_op, expr) => ExprKind::Unary(un_op, expr.await_to_cb(con)),
            ExprKind::Cast(expr, ty) => ExprKind::Cast(expr.await_to_cb(con), ty),
            ExprKind::Type(expr, ty) => ExprKind::Type(expr.await_to_cb(con), ty),
            ExprKind::If(comparison_expr, block, extra_expr) => {
                ExprKind::If(comparison_expr.await_to_cb(con),
                             block.await_to_cb(con),
                             extra_expr.await_to_cb(con))
            }
            ExprKind::IfLet(pat, comparison_expr, block, extra_expr) => {
                ExprKind::IfLet(pat,
                                comparison_expr.await_to_cb(con),
                                block.await_to_cb(con),
                                extra_expr.await_to_cb(con))
            }
            ExprKind::While(expr, block, ident) => {
                ExprKind::While(expr.await_to_cb(con), block.await_to_cb(con), ident)
            }
            ExprKind::WhileLet(pat, expr, block, ident) => {
                ExprKind::WhileLet(pat, expr.await_to_cb(con), block.await_to_cb(con), ident)
            }
            ExprKind::ForLoop(pat, expr, block, ident) => {
                ExprKind::ForLoop(pat, expr.await_to_cb(con), block.await_to_cb(con), ident)
            }
            ExprKind::Loop(block, ident) => ExprKind::Loop(block.await_to_cb(con), ident),
            ExprKind::Match(expr, arms) => {
                let arms = arms.into_iter()
                               .map(|arm| Arm { body: arm.body.await_to_cb(con), ..arm })
                               .collect();
                ExprKind::Match(expr.await_to_cb(con), arms)
            }
            ExprKind::Closure(capture_by, fn_decl, block) => {
                ExprKind::Closure(capture_by, fn_decl, block.await_to_cb(con))
            }
            ExprKind::Block(block) => ExprKind::Block(block.await_to_cb(con)),
            ExprKind::Assign(expr1, expr2) => {
                ExprKind::Assign(expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::AssignOp(bin_op, expr1, expr2) => {
                ExprKind::AssignOp(bin_op, expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::Field(expr, ident) => ExprKind::Field(expr.await_to_cb(con), ident),
            ExprKind::TupField(expr, size) => ExprKind::TupField(expr.await_to_cb(con), size),
            ExprKind::Index(expr1, expr2) => {
                ExprKind::Index(expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::Range(expr1, expr2) => {
                ExprKind::Range(expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::AddrOf(mutability, expr) => ExprKind::AddrOf(mutability, expr.await_to_cb(con)),
            ExprKind::InlineAsm(inline_asm) => {
                let inputs = inline_asm.inputs.into_iter()
                               .map(|(interned_str, expr)| (interned_str, expr.await_to_cb(con)))
                               .collect();
             	ExprKind::InlineAsm(InlineAsm {inputs: inputs, .. inline_asm})                  
            }
            ExprKind::Struct(path, fields, expr) => {
                let fields = fields.into_iter()
                               .map(|field| Field {expr: field.expr.await_to_cb(con), .. field})
                               .collect();
            	ExprKind::Struct(path, fields, expr.await_to_cb(con))
            }
            ExprKind::Repeat(expr1, expr2) => {
                ExprKind::Repeat(expr1.await_to_cb(con), expr2.await_to_cb(con))
            }
            ExprKind::Ret(Some(expr)) => {
                let expr = expr.await_to_cb(con);
                // If this function returns something it will have a final callback
                // that should be called instead of a sync return
                if con.final_cb {
                    quote_expr!(con.cx, __rust_async_autogen_final_callback($expr)).node.clone()
                } else {
                    ExprKind::Ret(Some(expr))
                }
            }
            ExprKind::Paren(expr) => ExprKind::Paren(expr.await_to_cb(con)),
            ExprKind::Mac(mac) => {
                // Macros should always have one path segment
                let identifier = mac.node.path.segments.first().unwrap().identifier;

                let inner_tokens = mac.node.tts.clone();

                if con.cx.ident_of("await").name == identifier.name {
                    let expr = quote_expr!(con.cx, $inner_tokens).await_to_cb(con);

                    // Was a single function provided inside await?
                    match expr.node.clone() {
                        expr_kind @ ExprKind::Call(..) => con.await_functions.push(expr_kind),
                        expr_kind @ ExprKind::MethodCall(..) => con.await_functions.push(expr_kind),
                        _ => {
                            con.cx.span_err(expr.span,
                                            "await macro expects a single function call as a \
                                             parameter\nlike: await!(get_user(1))")
                        }
                    };
					let var_ident = con.cx.ident_of(&format!("__rust_async_autogen_callback{}", con.await_functions.len()));

                    quote_expr!(con.cx, $var_ident).node.clone()
                } else {
                    // Parse macro arguments into as a tuple
                    // then search expressions inside tuple for await functions
                    if let ExprKind::Tup(exprs) = quote_expr!(con.cx, ($inner_tokens))
                                                      .node
                                                      .clone() {
                        let mut processed_exprs = Vec::new();

                        for expr in exprs {
                            // Add comma if we already have and expression
                            if !processed_exprs.is_empty() {
                                processed_exprs.push(TokenTree::Token(span, token::Token::Comma));
                            }

                            processed_exprs.append(&mut expr.await_to_cb(con).to_tokens(con.cx));
                        }

                        quote_expr!(con.cx, $identifier!($processed_exprs)).node.clone()
                    } else {
                        quote_expr!(con.cx, $identifier!($inner_tokens)).node.clone()
                    }
                }
            }
            _ => expr_kind,
        };

        con.cx.expr(span, expr_kind)
    }
}
