use crate::internals::*;
use joy_syntax::{ast, AModule};

pub(crate) struct XBody<'b> {
    pub(crate) name: &'b str,
    pub(crate) args: BumpVec<'b, ast::AName<'b>>,
    pub(crate) last: &'b str,
}

// pub(crate) enum MDefRef<'b> {
//     Fn(&'b MFnDef<'b>),
// }

pub(crate) struct XModule<'b> {
    pub(crate) name: &'b str,
    pub(crate) path: &'b str,
    pub(crate) source_code: &'b str,
    // pub(crate) symbols: HashMap<&'b str, MDefRef<'b>>,
}

pub(crate) struct XProgram<'b> {
    pub(crate) modules: BumpVec<'b, XModule<'b>>,
    pub(crate) bodies: BumpVec<'b, XBody<'b>>,
    bump: &'b Bump,
}

impl<'b> XProgram<'b> {
    pub(crate) fn new_in(bump: &'b Bump) -> Self {
        Self {
            modules: BumpVec::new_in(bump),
            bodies: BumpVec::new_in(bump),
            bump,
        }
    }

    fn push_decl(&mut self, decl: &ast::ADecl<'b>) {
        // match decl {
        //     ast::ADecl::Expr(_) => {}
        //     ast::ADecl::Let(_) => {}
        //     ast::ADecl::Fn(decl) => {
        //         self.bodies.push(XBody {
        //             name: self.bump.alloc_str(decl.name.text),
        //             last: bumpalo::format!(in self.bump, "{:?}", decl.body).into_bump_str(),
        //         });
        //     }
        // }
    }

    fn push_toplevel_decl(&mut self, decl: &ast::ADecl<'b>) {
        // match decl {
        //     ast::ADecl::Expr(expr) => {
        //         // toplevel expr
        //         let last = bumpalo::format!(in self.bump, "{:?}", expr).into_bump_str();
        //         self.bodies.push(XBody { name: "", last });
        //     }
        //     ast::ADecl::Let(decl) => {
        //         // toplevel let
        //     }
        //     _ => self.push_decl(decl),
        // }
    }

    pub(crate) fn push(&mut self, m: AModule<'b>) {
        let i = self.modules.len();
        self.modules.push(XModule {
            name: m.name,
            path: m.path,
            source_code: m.source_code,
        });

        for decl in &m.root.decls {
            self.push_toplevel_decl(decl);
        }
    }
}
