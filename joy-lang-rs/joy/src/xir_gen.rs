use crate::internals::*;
use joy_syntax::{ast, AModule, Token};

#[derive(Debug)]
pub(crate) struct XLocal<'b> {
    name: &'b str,
    alive: bool,
}

#[derive(Debug)]
pub(crate) enum XConst<'b> {
    Bool(bool),
    Int(i64),
    String(&'b str),
}

#[derive(Debug)]
pub(crate) enum XArg<'b> {
    Const(XConst<'b>),
    Local(&'b XLocal<'b>),
}

#[derive(Debug)]
pub(crate) enum XTerminator<'b> {
    Exit,
    Return(&'b XLocal<'b>),
}

#[derive(Debug)]
pub(crate) struct XBody<'b> {
    pub(crate) name: &'b str,
    pub(crate) args: BumpVec<'b, ast::AName<'b>>,
    pub(crate) blocks: RefCell<BumpVec<'b, XBlock<'b>>>,
}

impl<'b> XBody<'b> {
    pub(crate) fn new(name: &'b str, bump: &'b Bump) -> Self {
        Self {
            name,
            args: bumpalo::vec![in bump],
            blocks: RefCell::new(bumpalo::vec![in bump]),
        }
    }
}

#[derive(Debug)]
pub(crate) struct XDebugStmt<'b> {
    pub(crate) arg: XArg<'b>,
    pub(crate) pos: usize,
}

#[derive(Debug)]
pub(crate) enum XStmt<'b> {
    Debug(XDebugStmt<'b>),
    SetImmediateString {
        target: &'b XLocal<'b>,
        value: &'b str,
    },
}

#[derive(Debug)]
pub(crate) struct XBlock<'b> {
    pub(crate) stmts: BumpVec<'b, XStmt<'b>>,
    pub(crate) terminator: XTerminator<'b>,
}

pub(crate) struct XModule<'b> {
    pub(crate) name: &'b str,
    pub(crate) path: &'b str,
    pub(crate) source_code: &'b str,
    // pub(crate) symbols: HashMap<&'b str, MDefRef<'b>>,
}

pub(crate) struct XProgram<'b> {
    pub(crate) modules: BumpVec<'b, XModule<'b>>,
    pub(crate) bodies: BumpVec<'b, &'b XBody<'b>>,
}

impl<'b> XProgram<'b> {
    pub(crate) fn new_in(bump: &'b Bump) -> Self {
        Self {
            modules: BumpVec::new_in(bump),
            bodies: BumpVec::new_in(bump),
        }
    }
}

pub(crate) enum Decl<'b> {
    Fn(&'b ast::AFnStmt<'b>, &'b XBody<'b>),
}

pub(crate) enum Ref<'b> {
    Local(&'b XLocal<'b>),
    Body(&'b XBody<'b>),
}

pub(crate) struct XirGen<'a, 'b> {
    program: &'a mut XProgram<'b>,
    toplevel: &'b XBody<'b>,

    /// Stack of bodies being generated.
    bodies: Vec<&'b XBody<'b>>,

    env: HashMap<&'b str, Ref<'b>>,
    body: &'b XBody<'b>,
    locals: Vec<&'b XLocal<'b>>,
    stmts: Vec<XStmt<'b>>,

    decls: Vec<Decl<'b>>,

    bump: &'b Bump,
}

impl<'a, 'b: 'a> XirGen<'a, 'b> {
    pub(crate) fn new(program: &'a mut XProgram<'b>, bump: &'b Bump) -> Self {
        let toplevel = bump.alloc(XBody::new("toplevel", bump)) as &_;
        Self {
            program,
            toplevel,
            bodies: vec![],
            env: HashMap::new(),
            body: toplevel,
            stmts: vec![],
            decls: vec![],
            locals: vec![],
            bump,
        }
    }

    fn push_local(&mut self, name: &'b ast::AName<'b>) -> &'b XLocal<'b> {
        let local = self.bump.alloc(XLocal {
            name: name.text,
            alive: true,
        });
        self.locals.push(local);
        local
    }

    fn push_local_killed(&mut self) -> &'b XLocal<'b> {
        let local = self.bump.alloc(XLocal {
            name: "_",
            alive: false,
        });
        self.locals.push(local);
        local
    }

    fn set_const(&mut self, local: &'b XLocal<'b>, init: XConst<'b>) {
        // set init value to local.
        eprintln!("{} <- {:?}", local.name, init);
    }

    fn set_im_str(&mut self, target: &'b XLocal<'b>, value: &'b str) {
        self.stmts.push(XStmt::SetImmediateString { target, value });
    }

    fn terminate(&mut self, terminator: XTerminator<'b>) {
        let stmts = BumpVec::from_iter_in(self.stmts.drain(..), self.bump);
        self.body
            .blocks
            .borrow_mut()
            .push(XBlock { stmts, terminator });
    }

    // -------------------------------------------
    // early expr
    // -------------------------------------------

    fn declare_fn_stmt(&mut self, stmt: &'b ast::AFnStmt<'b>) {
        self.bodies
            .push(self.bump.alloc(XBody::new(stmt.name.text, self.bump)));
        let body = self.bodies.last().unwrap();
        self.env.insert(stmt.name.text, Ref::Body(body));
        self.decls.push(Decl::Fn(stmt, body));
    }

    fn declare_stmt(&mut self, stmt: &'b ast::AStmt<'b>) {
        match stmt {
            ast::AStmt::Expr(_) | ast::AStmt::Let(_) => {}
            ast::AStmt::Fn(stmt) => self.declare_fn_stmt(stmt),
        }
    }

    // -------------------------------------------
    // gen expr
    // -------------------------------------------

    fn gen_expr(&mut self, expr: &'b ast::AExpr<'b>, target: &'b XLocal<'b>) {
        match expr {
            ast::AExpr::Lit(lit) => match lit.token {
                Token::True => self.set_const(target, XConst::Bool(true)),
                Token::False => self.set_const(target, XConst::Bool(false)),
                _ => {
                    // FIXME:
                    self.set_im_str(target, lit.text)
                }
            },
            ast::AExpr::Name(name) => {
                // FIXME:
                self.set_im_str(target, name.text);
            }
            ast::AExpr::Call(expr) => {
                self.gen_expr(&expr.args[0], target);
                self.stmts.push(XStmt::Debug(XDebugStmt {
                    arg: XArg::Local(target),
                    pos: expr.pos.index(),
                }));
            }
            ast::AExpr::Binary(_) => todo!(),
        }
    }

    fn gen_expr_stmt(&mut self, stmt: &'b ast::AExprStmt<'b>) {
        let local = self.push_local_killed();
        self.gen_expr(&stmt.0, local);
    }

    fn gen_let_stmt(&mut self, stmt: &'b ast::ALetStmt<'b>) {
        let local = self.push_local(&stmt.name);
        self.gen_expr(&stmt.init, local);
        self.env.insert(stmt.name.text, Ref::Local(local));
    }

    fn gen_fn_stmt(&mut self, stmt: &'b ast::AFnStmt<'b>, body: &'b XBody<'b>) {
        let old_body = replace(&mut self.body, body);
        self.bodies.push(old_body);

        let result = self.push_local(&stmt.name);
        self.gen_expr(&stmt.body, result);
        self.terminate(XTerminator::Return(result));
        // eprintln!("fn {} {:?}", stmt.name.text, &self.body);

        let body = replace(&mut self.body, self.bodies.pop().unwrap());
        self.program.bodies.push(body);
    }

    fn gen_stmt(&mut self, stmt: &'b ast::AStmt<'b>) {
        match stmt {
            ast::AStmt::Expr(stmt) => self.gen_expr_stmt(stmt),
            ast::AStmt::Let(stmt) => self.gen_let_stmt(stmt),
            ast::AStmt::Fn(_) => {}
        }
    }

    fn gen_decl(&mut self, decl: Decl<'b>) {
        match decl {
            Decl::Fn(stmt, body) => self.gen_fn_stmt(stmt, body),
        }
    }

    // -------------------------------------------
    // push module
    // -------------------------------------------

    pub(crate) fn push_module(
        &mut self,
        name: &'b str,
        source_code: &'b str,
        ast: &'b ast::ARoot<'b>,
    ) {
        self.program.modules.push(XModule {
            name,
            path: name,
            source_code,
        });

        let old_body = replace(&mut self.body, self.bump.alloc(XBody::new(name, self.bump)));

        for stmt in &ast.stmts {
            self.declare_stmt(stmt);
        }
        for stmt in &ast.stmts {
            self.gen_stmt(stmt);
        }
        let decls = take(&mut self.decls);
        for decl in decls {
            self.gen_decl(decl);
        }
        self.terminate(XTerminator::Exit);

        let body = replace(&mut self.body, old_body);
        self.program.bodies.push(body);
    }
}
