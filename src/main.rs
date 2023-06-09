#[macro_use]
extern crate swc_common;
extern crate swc_ecma_ast;
use ast::{VarDecl, ParenExpr};
use swc_ecma_ast as ast;

use std::{collections::{HashMap, HashSet}, path::Path};

mod parser;
use parser::parse_file;


type Ident = String;
type IdentRaw = str;

type Scope = HashMap<Ident, JsPrim>;

#[derive(Debug, Clone, PartialEq)]
enum JsOp {
    Nop,
    DeclareVar { name: IrIdent, init: IrVal },
    SetVar { target: IrIdent, value: IrVal },
    DoBinOp { target: IrIdent, op: BinOp, lhs: IrVal, rhs: IrVal },
}

#[derive(Debug, Clone, PartialEq)]
enum IrVal {
    Ident(IrIdent),
    Const(JsPrim),
}
impl From<&PrimExprVal> for IrVal {
    fn from(src: &PrimExprVal) -> Self {
        match src {
            PrimExprVal::Const(v) => IrVal::Const(v.clone()),
            PrimExprVal::Ident(id) => IrVal::Ident(id.into()),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum IrIdent {
    Ident(Ident),
    Tmp(usize),
}
// TODO: is separating these two even necessary?
impl From<&PrimExprIdent> for IrIdent {
    fn from(src: &PrimExprIdent) -> Self {
        match src {
            PrimExprIdent::Tmp(id) => IrIdent::Tmp(*id),
            PrimExprIdent::Ident(name) => IrIdent::Ident(name.clone()), // TODO nasty String clone
        }
    }
}



type JsNumber = f64;

#[derive(Debug, Clone, PartialEq)]
enum JsPrim {
    Number(JsNumber),
    Undefined,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum BinOp {
    Add,
    Mul,
    Div,
}

fn main() {
    let module_ast = parse_file(Path::new("test/test.js"))
        .expect("Error while loading source file, exiting");
    println!("{:#?}", module_ast);

    let code = module_to_ir(module_ast)
        .expect("Error compiling source file, exiting");
    println!("{:#?}", code);
    match run_block(code) {
        Ok(js_val) => println!("==> {:#?}", js_val),
        Err(err) => println!("error: {:#?}", err),
    }
}

fn run_block(code: Vec<JsOp>) -> Result<JsVal, RuntimeError> {
    let mut frame = ExecutionFrame {
        code: &code,
        instruction_ix: 0,
        tmp: vec![JsVal::Undefined; 8],
        vars: HashMap::new()
    };
    while frame.instruction_ix < code.len() {
        try_step_frame(&mut frame)?;
    }
    let last_value = frame.tmp[0];
    Ok(last_value)
}


//==============================
// Compiler
//==============================

#[derive(Debug, Clone)]
enum CompileError {
    Unsupported(String)
}

fn module_to_ir(module_ast: ast::Module) -> Result<Vec<JsOp>, CompileError> {
    let mut ctx = StmtToIrCtx::new();
    for item in &module_ast.body {
        let stmt_ast = item.as_stmt()
            .ok_or(CompileError::Unsupported("Unsupported form".into()))?;
            // .ok_or(CompileError::Unsupported(format!("Unsupported form: {:#?}", item)))?;
        stmt_to_ir(stmt_ast, &mut ctx)?;
    }
    Ok(ctx.block)
}

#[derive(Debug, Clone, PartialEq)]
struct StmtToIrCtx {
    uniq: usize,
    block: Vec<JsOp>,
}

impl StmtToIrCtx {
    fn new() -> Self {
        StmtToIrCtx { uniq: 0, block: vec![] }
    }
    fn emit_op(&mut self, stmt: JsOp) {
        self.block.push(stmt)
    }

    fn get_tmp_ident(&mut self) -> PrimExprIdent {
        let res = PrimExprIdent::Tmp(self.uniq);
        self.uniq += 1;
        res
    }
}

fn stmt_to_ir(stmt_ast: &ast::Stmt, ctx: &mut StmtToIrCtx) -> Result<(), CompileError> {
    println!("got stmt: {:#?}", stmt_ast);
    fn handle_expression(expr_ast: &ast::Expr, ctx: &mut StmtToIrCtx) -> Result<IrVal, CompileError> {
         // NOTE: we're being a bit loose with our tmp ids here.
        // expr_to_ir will count up from 0, so each statement will re-use the same tmp vars.
        // this should be fine (since tmps shouldn't escape outside their context)
        // and in fact SHOULD sorta work like we're using a stack machine for expressions.
        let (expr_ctx, result) = expr_to_ir(&expr_ast, 0)?;
        // TODO: we're likely to emit a superfluous tmp here.
        // maybe emit a SetVar directly into `name` if result is the last assignment target?
        for op in primexpr_setup_to_ir(&expr_ctx.setup) {
            ctx.emit_op(op)
        }
        Ok((&result).into())
    }
    match stmt_ast {
        ast::Stmt::Decl(ast::Decl::Var(boxed)) => {
            // TODO: deal with `kind`
            for decl in &boxed.decls {
                // ctx.emit_op()
                let name = ast_ident_to_name(
                    decl.name.as_ident()
                        .ok_or(CompileError::Unsupported("Only supporting assignment to var names for now".to_string()))?
                );
                
                let init: IrVal = match &decl.init {
                    Some(init_ast) => {
                       handle_expression(init_ast, ctx)?
                    },
                    None => IrVal::Const(JsPrim::Undefined),
                };
                ctx.emit_op(JsOp::DeclareVar { name: IrIdent::Ident(name), init })
            }
            Ok(())
        },
        ast::Stmt::Expr(expr_stmt) => {
            let result = handle_expression(&expr_stmt.expr, ctx)?;
            // NOTE: shouldn't clobber tmp0, but also... is this the right thing to do?
            // (we can't omit it, so we have to do SOMETHING...)
            ctx.emit_op(JsOp::SetVar { target: IrIdent::Tmp(0), value: result });
            Ok(())
        }
        _ => Err(CompileError::Unsupported("oops".into()))
    }
    // Ok(vec![JSOp::Nop])
}






fn expr_to_ir(expr_ast: &ast::Expr, uniq: usize) -> Result<(PrimExprCtx, PrimExprVal), CompileError> {
    let mut ctx = PrimExprCtx { setup: vec![], uniq };
    let final_prim = expr_to_prims(expr_ast, &mut ctx);
    Ok((ctx, final_prim))
}

fn primexpr_setup_to_ir(setup: &PrimExprs) -> Vec<JsOp> {
    setup.into_iter().map(|(target, primexpr)| {
        match primexpr {
            PrimExpr::Val { val } => {
                JsOp::SetVar { target: target.into(), value: val.into() }
            },
            PrimExpr::BinOp { op, lhs, rhs } => {
                JsOp::DoBinOp { target: target.into(), op: *op, lhs: lhs.into(), rhs: rhs.into() }
            },
        }
    }).collect()
}


#[derive(Debug, Clone, PartialEq)]
enum PrimExpr {
    Val { val: PrimExprVal },
    BinOp { op: BinOp, lhs: PrimExprVal, rhs: PrimExprVal }
}

#[derive(Debug, Clone, PartialEq)]
enum PrimExprVal {
    Const(JsPrim),
    Ident(PrimExprIdent),
}

#[derive(Debug, Clone, PartialEq)]
enum PrimExprIdent { Tmp(usize), Ident(Ident) }


type PrimExprs = Vec<(PrimExprIdent, PrimExpr)>;

#[derive(Debug, Clone, PartialEq)]
struct PrimExprCtx {
    setup: PrimExprs,
    // referenced_idents: HashSet<Ident>,
    uniq: usize,
}

impl PrimExprCtx {
    fn emit_setup(&mut self, ident: PrimExprIdent, expr: PrimExpr) {
        self.setup.push((ident, expr))
    }

    fn get_tmp_ident(&mut self) -> PrimExprIdent {
        let res = PrimExprIdent::Tmp(self.uniq);
        self.uniq += 1;
        res
    }
}

fn expr_to_prims(expr_ast: &ast::Expr, ctx: &mut PrimExprCtx) -> PrimExprVal {
    match expr_ast {
        ast::Expr::Lit(lit) => {
            let val = match lit {
                ast::Lit::Num(num) => JsPrim::Number(num.value),
                ast::Lit::Str(_) => todo!(),
                ast::Lit::Bool(_) => todo!(),
                ast::Lit::Null(_) => todo!(),
                ast::Lit::BigInt(_) => todo!(),
                ast::Lit::Regex(_) => todo!(),
                ast::Lit::JSXText(_) => unimplemented!("JSX is not supported"),
            };
            PrimExprVal::Const(val)
        },
        ast::Expr::Bin(ast::BinExpr { op, left, right, .. }) => {
            let ident = ctx.get_tmp_ident();
            let left_primexpr = expr_to_prims(left, ctx);
            let right_primexpr = expr_to_prims(right, ctx);
            // TODO: logical ops need more care
            let ir_op = match op {
                ast::BinaryOp::Add => BinOp::Add,
                ast::BinaryOp::Mul => BinOp::Mul,
                ast::BinaryOp::Div => BinOp::Div,
                _ => todo!(),
            };
            ctx.emit_setup(
                ident.clone(),
                PrimExpr::BinOp {
                    op: ir_op,
                    lhs: left_primexpr,
                    rhs: right_primexpr,
                }
            );
            PrimExprVal::Ident(ident)
        },
        ast::Expr::Paren(inner) => expr_to_prims(&*(inner.expr), ctx),
        ast::Expr::Ident(ast_ident) => {
            PrimExprVal::Ident(PrimExprIdent::Ident(ast_ident_to_name(ast_ident)))
        },

        ast::Expr::This(_) => todo!(),
        ast::Expr::Array(_) => todo!(),
        ast::Expr::Object(_) => todo!(),
        ast::Expr::Fn(_) => todo!(),
        ast::Expr::Unary(_) => todo!(),
        ast::Expr::Update(_) => todo!(),
        ast::Expr::Assign(_) => todo!(),
        ast::Expr::Member(_) => todo!(),
        ast::Expr::SuperProp(_) => todo!(),
        ast::Expr::Cond(_) => todo!(),
        ast::Expr::Call(_) => todo!(),
        ast::Expr::New(_) => todo!(),
        ast::Expr::Seq(_) => todo!(),
        ast::Expr::Tpl(_) => todo!(),
        ast::Expr::TaggedTpl(_) => todo!(),
        ast::Expr::Arrow(_) => todo!(),
        ast::Expr::Class(_) => todo!(),
        ast::Expr::Yield(_) => todo!(),
        ast::Expr::MetaProp(_) => todo!(),
        ast::Expr::Await(_) => todo!(),
        ast::Expr::PrivateName(_) => todo!(),
        ast::Expr::OptChain(_) => todo!(),
        ast::Expr::Invalid(_) => todo!(),

        ast::Expr::JSXMember(_)
            | ast::Expr::JSXNamespacedName(_)
            | ast::Expr::JSXEmpty(_)
            | ast::Expr::JSXElement(_)
            | ast::Expr::JSXFragment(_) => unimplemented!("JSX is not supported"),
        ast::Expr::TsTypeAssertion(_)
            | ast::Expr::TsConstAssertion(_)
            | ast::Expr::TsNonNull(_)
            | ast::Expr::TsAs(_)
            | ast::Expr::TsInstantiation(_)
            | ast::Expr::TsSatisfaction(_) => unimplemented!("TypeScript syntax is not supported"),
        _ => todo!()
    }
}

fn ast_ident_to_name(ident: &ast::Ident) -> String {
    ident.sym.to_string()
}



//==============================
// VM
//==============================

#[derive(Debug, PartialEq, Clone, Copy)]
enum JsVal {
    Number(JsNumber),
    Undefined,
}

impl From<&JsPrim> for JsVal {
    fn from(src: &JsPrim) -> Self {
        match src {
            JsPrim::Number(num) => JsVal::Number(*num),
            JsPrim::Undefined => JsVal::Undefined,
        }
    }
}



#[derive(Debug)]
enum RuntimeError {
    UndefinedVariable(String),
    UnsupportedOperand(BinOp),
    DivByZero,
    Internal(InternalError),
}

#[derive(Debug)]
enum InternalError {
    InvalidTmpVar(usize),
}

struct ExecutionFrame<'a> {
    vars: HashMap<&'a IdentRaw, JsVal>,
    tmp: Vec<JsVal>,
    instruction_ix: usize,
    code: &'a [JsOp],
}

fn frame_set_ident<'a>(frame: &mut ExecutionFrame<'a>, ident: &'a IrIdent, val: JsVal) -> Result<(), RuntimeError> {
    match ident {
        IrIdent::Ident(name) => {
            // TODO: error checking!!!
            frame.vars.insert(name, val);
            Ok(())
        }
        IrIdent::Tmp(id) => {
            let ix = *id;
            // FIXME: THIS FEELS LIKE A REALLY DUMB WAY TO GO ABOUT IT
            // at least, like, round to nearest power of two or something
            if ix > frame.tmp.len() {
                frame.tmp.resize(ix, JsVal::Undefined);
            }
            frame.tmp[ix] = val;
            Ok(())
        },
    }
}

fn frame_get_val(frame: &ExecutionFrame, ir_val: &IrVal) -> Result<JsVal, RuntimeError> {
    match ir_val {
        IrVal::Ident(ident) => frame_get_ident(frame, ident),
        IrVal::Const(prim) => {
            Ok(prim.into())
        }
    }
}

fn frame_get_ident(frame: &ExecutionFrame, ident: &IrIdent) -> Result<JsVal, RuntimeError> {
    match ident {
        IrIdent::Ident(name) => {
            match frame.vars.get(name.as_str()) {
                Some(val) => Ok(*val),
                None => Err(RuntimeError::UndefinedVariable(name.clone())),
            }
        }
        IrIdent::Tmp(id) => {
            match frame.tmp.get(*id) {
                Some(val) => Ok(*val),
                None => Err(RuntimeError::Internal(InternalError::InvalidTmpVar(*id)))
            }
        },
    }
}

fn frame_advance_instr(frame: &mut ExecutionFrame) {
    frame.instruction_ix += 1;
}

fn try_step_frame<'a>(frame: &mut ExecutionFrame<'a>) -> Result<(), RuntimeError> {
    let instruction = &frame.code[frame.instruction_ix];
    match instruction {
        JsOp::DoBinOp { target, op, lhs, rhs } => {
            let lhs_val = frame_get_val(frame, lhs)?;
            let rhs_val = frame_get_val(frame, rhs)?;
            let res = do_binop(*op, lhs_val, rhs_val)?;
            frame_set_ident(frame, target, res)?;
            frame_advance_instr(frame);
            Ok(())
        },
        JsOp::Nop => {
            frame_advance_instr(frame);
            Ok(())
        },
        JsOp::DeclareVar { name, init: ir_val } => {
            // TODO: track variables declared in scope
            // (ignoring for now)
            let val = frame_get_val(frame, ir_val)?;
            frame_set_ident(frame, name, val)?;
            frame_advance_instr(frame);
            Ok(())
        },
        JsOp::SetVar { target, value: ir_val } => {
            let val = frame_get_val(frame, ir_val)?;
            frame_set_ident(frame, target, val)?;
            frame_advance_instr(frame);
            Ok(())
        },
    }
}

fn do_binop(op: BinOp, lhs: JsVal, rhs: JsVal) -> Result<JsVal, RuntimeError> {
    match (lhs, rhs) {
        (JsVal::Number(lhs_num), JsVal::Number(rhs_num)) => {
            let res = do_binop_number(op, lhs_num, rhs_num)?;
            Ok(JsVal::Number(res))
        },
        _ => Err(RuntimeError::UnsupportedOperand(BinOp::Add))
    }
}

fn do_binop_number(op: BinOp, lhs: JsNumber, rhs: JsNumber) -> Result<JsNumber, RuntimeError> {
    match op {
        BinOp::Add => Ok(lhs + rhs),
        BinOp::Mul => Ok(lhs * rhs),
        BinOp::Div => {
            if rhs == 0.0 {
                Err(RuntimeError::DivByZero)
            } else {
                Ok(lhs / rhs)
            }
        },
    }
}
