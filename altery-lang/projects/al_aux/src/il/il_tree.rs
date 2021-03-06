//! 中間言語の抽象構文木

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IlKind {
    // 宣言:
    Root,
    CodeSection,
    Globals,
    Labels,
    LabelDef,

    // 文:
    Semi,
    Jump,
    JumpUnless,
    Call,
    Ret,
    Pop,
    Assert,
    CellSet,

    // 式:
    Bool(bool),
    Int(i64),
    GlobalGet,
    LabelGet,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,

    // その他:
    Ident(usize),
}

#[derive(Clone, Copy, Debug)]
pub struct Il {
    kind: IlKind,
    start: usize,
    end: usize,
    comment: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct IlTree {
    ils: Vec<Il>,
    children: Vec<usize>,
    strings: Vec<String>,
    root: usize,
}

static IL_KIND_TEXTS: &[(&str, IlKind)] = &[
    ("root", IlKind::Root),
    ("code_section", IlKind::CodeSection),
    ("globals", IlKind::Globals),
    ("labels", IlKind::Labels),
    ("label_def", IlKind::LabelDef),
    ("semi", IlKind::Semi),
    ("jump", IlKind::Jump),
    ("jump_unless", IlKind::JumpUnless),
    ("call", IlKind::Call),
    ("ret", IlKind::Ret),
    ("pop", IlKind::Pop),
    ("assert", IlKind::Assert),
    ("cell_set", IlKind::CellSet),
    ("false", IlKind::Bool(false)),
    ("true", IlKind::Bool(true)),
    ("global_get", IlKind::GlobalGet),
    ("label_get", IlKind::LabelGet),
    ("+", IlKind::OpAdd),
    ("-", IlKind::OpSub),
    ("*", IlKind::OpMul),
    ("/", IlKind::OpDiv),
    ("==", IlKind::OpEq),
];

impl IlKind {
    pub fn texts() -> &'static [(&'static str, IlKind)] {
        IL_KIND_TEXTS
    }
}

impl Il {
    pub fn new(kind: IlKind, start: usize, end: usize) -> Self {
        Il {
            kind,
            start,
            end,
            comment: None,
        }
    }

    pub fn kind(self) -> IlKind {
        self.kind
    }

    pub fn start(self) -> usize {
        self.start
    }

    pub fn end(self) -> usize {
        self.end
    }

    pub fn len(self) -> usize {
        self.end - self.start
    }
}

impl IlTree {
    pub fn new() -> IlTree {
        Self {
            ils: Vec::with_capacity(128),
            children: Vec::with_capacity(256),
            strings: vec![],
            root: 0,
        }
    }

    pub fn kind(&self, il: usize) -> IlKind {
        self.ils[il].kind()
    }

    pub fn child_len(&self, il: usize) -> usize {
        self.ils[il].len()
    }

    pub fn child(&self, il: usize, child_index: usize) -> usize {
        let ci = self.ils[il].start + child_index;
        self.children[ci]
    }

    pub fn comment(&self, il: usize) -> Option<&str> {
        self.ils[il].comment.map(|comment| self.get_string(comment))
    }

    pub fn len(&self) -> usize {
        self.ils.len()
    }

    pub fn root(&self) -> usize {
        self.root
    }

    pub fn get_string(&self, string_id: usize) -> &str {
        &self.strings[string_id]
    }

    pub fn set_root(&mut self, root: usize) {
        self.root = root;
    }

    fn new_il(&mut self, il: Il) -> usize {
        let node_id = self.ils.len();
        self.ils.push(il);
        node_id
    }

    pub fn new_leaf(&mut self, kind: IlKind) -> usize {
        self.new_il(Il::new(kind, 0, 0))
    }

    pub fn new_bool(&mut self, value: bool) -> usize {
        self.new_leaf(IlKind::Bool(value))
    }

    pub fn new_int(&mut self, value: i64) -> usize {
        self.new_leaf(IlKind::Int(value))
    }

    pub fn new_ident(&mut self, ident: String) -> usize {
        let string_id = self.add_string(ident);
        self.new_leaf(IlKind::Ident(string_id))
    }

    pub fn new_node(&mut self, kind: IlKind, children: &[usize]) -> usize {
        let start = self.children.len();
        self.children.extend(children);
        let end = self.children.len();

        self.new_il(Il::new(kind, start, end))
    }

    pub fn new_label_def(&mut self, ident: String) -> usize {
        let label = self.new_ident(ident);
        self.new_node(IlKind::LabelDef, &[label])
    }

    pub fn new_label_get(&mut self, ident: String) -> usize {
        let label = self.new_ident(ident);
        self.new_node(IlKind::LabelGet, &[label])
    }

    pub fn add_string(&mut self, string: String) -> usize {
        let string_id = self.strings.len();
        self.strings.push(string);
        string_id
    }

    pub fn set_comment(&mut self, il: usize, comment: String) {
        let string_id = self.add_string(comment);
        self.ils[il].comment = Some(string_id);
    }
}
