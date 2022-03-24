#![allow(unused)]

use super::*;

pub struct XName<'b> {
    pub id: usize,
    pub name: &'b str,
}

impl<'b> Display for XName<'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.id)
    }
}

pub enum XConst<'b> {
    Bool(bool),
    Int(i64),
    String(&'b str),
}

pub struct XLocal<'b> {
    pub name: &'b str,
    // alive
}

impl<'b> XLocal<'b> {
    pub fn new(name: &'b str, bump: &'b Bump) -> &'b XLocal<'b> {
        bump.alloc(Self { name })
    }
}

pub enum XArg<'b> {
    Const(XConst<'b>),
    Local(&'b XLocal<'b>),
}

impl<'b> XConst<'b> {
    fn into_arg(self) -> XArg<'b> {
        XArg::Const(self)
    }

    fn into_rval(self) -> XRval<'b> {
        XRval::Use(self.into_arg())
    }
}

pub enum XRval<'b> {
    Use(XArg<'b>),
    Bin([XArg<'b>; 2]),
}

pub enum XTerminator<'b> {
    Exit,
    Return(&'b XLocal<'b>),
}

pub struct XBody<'b> {
    pub name: XName<'b>,
    pub params: BumpVec<'b, XName<'b>>,
    pub locals: RefCell<BumpVec<'b, &'b XLocal<'b>>>,
    pub blocks: RefCell<BumpVec<'b, XBlock<'b>>>,
}

impl<'b> XBody<'b> {
    pub fn new(name: XName<'b>, bump: &'b Bump) -> &'b XBody<'b> {
        bump.alloc(Self {
            name,
            params: bumpalo::vec![in bump],
            locals: RefCell::new(bumpalo::vec![in bump]),
            blocks: RefCell::new(bumpalo::vec![in bump]),
        })
    }
}

pub enum XStmt<'b> {
    Assign {
        dest: &'b XLocal<'b>,
        init: XRval<'b>,
    },
}

pub struct XBlock<'b> {
    pub stmts: BumpVec<'b, XStmt<'b>>,
    pub terminator: XTerminator<'b>,
}

pub enum Ref<'b> {
    Local(&'b XLocal<'b>),
    Body(&'b XBody<'b>),
}

pub struct XModule<'b> {
    pub name: XName<'b>,
    // pub path: &'b str,
    // pub source_code: &'b str,
    pub toplevel: &'b XBody<'b>,
    // pub symbols: HashMap<&'b str, MDefRef<'b>>,
}

pub struct XProgram<'b> {
    pub modules: BumpVec<'b, XModule<'b>>,
    pub bodies: BumpVec<'b, &'b XBody<'b>>,
}

impl<'b> XProgram<'b> {
    pub fn new_in(bump: &'b Bump) -> Self {
        Self {
            modules: BumpVec::new_in(bump),
            bodies: BumpVec::new_in(bump),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    fn xn(s: &'static str) -> XName<'static> {
        thread_local! {
            static ID: Cell<usize> = Cell::new(0);
        }

        ID.with(|cell| {
            let id = cell.get() + 1;
            cell.set(id);
            XName { id, name: s }
        })
    }

    #[test]
    fn it_works() {
        let bump = &Bump::new();

        // fn f() { let x = 2 + 2; }

        let f = XBody::new(xn("f"), bump);
        let x = XLocal::new("x", bump);
        f.locals.borrow_mut().push(x);
        let rval = XRval::Bin([XConst::Int(2).into_arg(), XConst::Int(4).into_arg()]);
        let block = XBlock {
            stmts: bumpalo::vec![in bump;
                XStmt::Assign { dest: x, init: rval }
            ],
            terminator: XTerminator::Exit,
        };
        f.blocks.borrow_mut().push(block);

        let mut program = XProgram::new_in(bump);
        program.bodies.push(f);

        // FIXME: assert
    }
}
