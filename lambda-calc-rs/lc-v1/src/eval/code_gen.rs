use std::{collections::HashMap, marker::PhantomData, mem::replace, mem::take};

use crate::{
    ast::a_tree::{ADecl, AExpr, ARoot, Ast},
    syntax::syntax_token::SyntaxToken,
};

type GResult<T> = Result<T, String>;

#[derive(Debug)]
pub(crate) struct Label(usize);

#[derive(Copy, Clone, Debug)]
pub(crate) struct Reg(usize);

#[derive(Debug)]
enum GTerm<'a> {
    Bool(bool),
    Int(i64),
    StaticVar(StaticVarId),
    LocalVar(LocalVarId),
    Fn(FnId),
    Reg(Reg),
    #[allow(unused)]
    Unused(PhantomData<&'a ()>),
}

/// Immediate value.
#[derive(Debug)]
pub(crate) enum Imm {
    Bool(bool),
    Int(i64),
    Fn(FnId),
}

#[derive(Debug)]
pub(crate) enum Code<'a> {
    Mov(Reg, Reg),
    MovImm(Reg, Imm),
    LoadStaticVar(Reg, StaticVarId),
    LoadLocalVar(Reg, LocalVarId),
    StoreLocalVar(LocalVarId, Reg),
    // Jump(Label),
    // JumpIf(Label, Reg),
    // callee, arity
    BeginCall(Reg, usize),
    EndCall(Reg),
    Return,
    Exit,
    PrintVal(&'a str, Reg),
}

#[derive(Debug, Default)]
pub(crate) struct Program<'a> {
    reg_count: usize,
    labels: Vec<usize>,
    codes: Vec<Code<'a>>,
}

type StaticVarId = usize;
type LocalVarId = usize;
type FnId = usize;

#[derive(Copy, Clone)]
pub(crate) enum Name {
    StaticVar(StaticVarId),
    LocalVar(LocalVarId),
    // upvar
}

struct FnData<'a> {
    local_var_count: usize,
    codes: Vec<Code<'a>>,
}

#[derive(Default)]
struct CodeGenerator<'a> {
    current_fn: Option<FnId>,
    static_vars: Vec<&'a SyntaxToken<'a>>,
    fns: Vec<FnData<'a>>,
    map_stack: Vec<HashMap<&'a str, Name>>,
    program: Program<'a>,
}

impl<'a> CodeGenerator<'a> {
    fn in_global(&self) -> bool {
        self.current_fn.is_none()
    }

    fn enter_scope(&mut self) -> &mut HashMap<&'a str, Name> {
        self.map_stack.push(HashMap::new());
        self.map_stack.last_mut().unwrap()
    }

    fn leave_scope(&mut self) {
        self.map_stack.pop();
    }

    fn resolve_value(&self, name: &str) -> Option<Name> {
        self.map_stack
            .iter()
            .rev()
            .find_map(|map| map.get(name))
            .copied()
    }

    fn set_to_reg(&mut self, reg: Reg, term: &GTerm<'a>) {
        match *term {
            GTerm::Bool(value) => self.emit(Code::MovImm(reg, Imm::Bool(value))),
            GTerm::Int(value) => self.emit(Code::MovImm(reg, Imm::Int(value))),
            GTerm::StaticVar(id) => self.emit(Code::LoadStaticVar(reg, id)),
            GTerm::LocalVar(id) => self.emit(Code::LoadLocalVar(reg, id)),
            GTerm::Fn(id) => self.emit(Code::MovImm(reg, Imm::Fn(id))),
            GTerm::Reg(src) => self.emit(Code::Mov(reg, src)),
            GTerm::Unused(..) => unreachable!(),
        }
    }

    fn alloc_static_var(&mut self, token: &'a SyntaxToken<'a>) -> StaticVarId {
        let id = self.static_vars.len();
        self.static_vars.push(token);
        id
    }

    fn alloc_reg(&mut self, term: &GTerm<'a>) -> Reg {
        let reg = Reg(self.program.reg_count);
        self.program.reg_count += 1;

        self.set_to_reg(reg, term);
        reg
    }

    fn emit(&mut self, code: Code<'a>) {
        self.program.codes.push(code);
    }

    fn on_block(&mut self, decls: &'a [ADecl<'a>]) -> GResult<GTerm<'a>> {
        let (decls, last_opt) = match decls.split_last() {
            Some((ADecl::Expr(last), decls)) => (decls, Some(last)),
            _ => (decls, None),
        };

        for decl in decls {
            self.on_decl(decl)?;
        }

        match last_opt {
            Some(last) => self.on_expr(last),
            None => {
                // unit?
                Ok(GTerm::Bool(false))
            }
        }
    }

    fn on_expr(&mut self, expr: &'a AExpr<'a>) -> GResult<GTerm<'a>> {
        let term = match expr {
            AExpr::True(_) => GTerm::Bool(true),
            AExpr::False(_) => GTerm::Bool(false),
            AExpr::Number(token) => {
                let value = token
                    .text
                    .parse::<i64>()
                    .map_err(|err| format!("invalid int constant {:?}", err))?;
                GTerm::Int(value)
            }
            AExpr::Var(token) => {
                let name = match self.resolve_value(token.text) {
                    Some(it) => it,
                    None => return Err(format!("undefined value {:?}", token.text)),
                };

                // グローバル変数か、外側の関数のローカル変数か、現在の関数のローカル変数か、プリミティブの名前か、未定義か
                match name {
                    Name::StaticVar(id) => GTerm::StaticVar(id),
                    Name::LocalVar(id) => GTerm::LocalVar(id),
                }
            }
            AExpr::Call(expr) => {
                let callee = self.on_expr(&expr.callee)?;
                let callee = self.alloc_reg(&callee);

                let args = expr
                    .args
                    .iter()
                    .map(|arg| -> GResult<Reg> {
                        let term = self.on_expr(arg)?;
                        let reg = self.alloc_reg(&term);
                        Ok(reg)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                self.emit(Code::BeginCall(callee, args.len()));
                for (i, arg) in args.into_iter().enumerate() {
                    self.emit(Code::StoreLocalVar(i, arg));
                }
                self.emit(Code::EndCall(callee));
                GTerm::Reg(Reg(0)) // 関数の結果は 0 番レジスタに入る。
            }
            AExpr::Block(decls) => {
                self.enter_scope();
                let result = self.on_block(decls);
                self.leave_scope();
                result?
            }
            AExpr::If(..) => todo!(),
            AExpr::Fn(expr) => {
                let fn_id = self.fns.len();
                self.fns.push(FnData {
                    local_var_count: 0,
                    codes: vec![],
                });

                let parent_fn = replace(&mut self.current_fn, Some(fn_id));
                let codes = take(&mut self.program.codes);
                let local_map = self.enter_scope();

                for (i, param) in expr.params.iter().enumerate() {
                    local_map.insert(param.text, Name::LocalVar(i));
                }
                let result = match &expr.body_opt {
                    Some(body) => self.on_expr(body)?,
                    None => return Err("missing function body".into()),
                };
                self.set_to_reg(Reg(0), &result);
                self.emit(Code::Return);

                self.leave_scope();
                let codes = replace(&mut self.program.codes, codes);
                self.current_fn = parent_fn;

                self.fns[fn_id] = FnData {
                    local_var_count: expr.params.len(),
                    codes,
                };
                GTerm::Fn(fn_id)
            }
        };
        Ok(term)
    }

    fn on_decl(&mut self, decl: &'a ADecl<'a>) -> GResult<()> {
        match decl {
            ADecl::Expr(expr) => {
                let name = "it";
                let term = self.on_expr(expr)?;

                if self.in_global() {
                    let reg = self.alloc_reg(&term);
                    self.emit(Code::PrintVal(name, reg));
                }
            }
            ADecl::Let(decl) => {
                let name = match decl.name_opt {
                    Some(name) => name.text,
                    None => "it",
                };

                let term = match &decl.init_opt {
                    Some(init) => self.on_expr(init)?,
                    None => return Err("missing init expression".into()),
                };

                let reg = self.alloc_reg(&term);
                match (&decl.name_opt, self.current_fn) {
                    (Some(name), None) => {
                        let id = self.alloc_static_var(name);
                        self.map_stack
                            .last_mut()
                            .unwrap()
                            .insert(name.text, Name::StaticVar(id));
                    }
                    (Some(name), Some(fn_id)) => {
                        let fn_ref = &mut self.fns[fn_id];
                        let id = fn_ref.local_var_count;
                        fn_ref.local_var_count += 1;

                        self.map_stack
                            .last_mut()
                            .unwrap()
                            .insert(name.text, Name::LocalVar(id));
                    }
                    (None, _) => {}
                }

                if self.in_global() {
                    self.emit(Code::PrintVal(name, reg));
                }
            }
        };
        Ok(())
    }

    fn on_root(&mut self, root: &'a ARoot<'a>) -> GResult<()> {
        for decl in &root.decls {
            self.on_decl(decl)?;
        }
        self.emit(Code::Exit);

        Ok(())
    }
}

pub(crate) fn code_gen<'a>(ast: &'a Ast<'a>) -> GResult<Program<'a>> {
    let mut generator = CodeGenerator::default();
    generator.program.reg_count += 1; // 関数の結果を入れるレジスタを確保
    generator.enter_scope();
    generator.on_root(&ast.root)?;

    let mut program = generator.program;

    for fn_data in generator.fns {
        let pc = program.codes.len();
        program.labels.push(pc);

        program.codes.extend(fn_data.codes);
    }

    Ok(program)
}
