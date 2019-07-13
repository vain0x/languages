#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IlKind {
    Exit,
    Semi,
    Assert,
    Bool(bool),
    Int(i64),
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
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
    comments: Vec<String>,
    root: usize,
}

impl Il {
    pub fn new(kind: IlKind, start: usize, end: usize) -> Self {
        Il { kind, start, end, comment: None }
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
            comments: vec![],
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
        self.ils[il].comment.map(|comment| self.comments[comment].as_ref())
    }

    pub fn len(&self) -> usize {
        self.ils.len()
    }

    pub fn root(&self) -> usize {
        self.root
    }

    pub fn set_root(&mut self, root: usize) {
        self.root = root;
    }

    fn add_il(&mut self, il: Il) -> usize {
        let node_id = self.ils.len();
        self.ils.push(il);
        node_id
    }

    pub fn add_leaf(&mut self, kind: IlKind) -> usize {
        self.add_il(Il::new(kind, 0, 0))
    }

    pub fn add_bool(&mut self, value: bool) -> usize {
        self.add_leaf(IlKind::Bool(value))
    }

    pub fn add_int(&mut self, value: i64) -> usize {
        self.add_leaf(IlKind::Int(value))
    }

    pub fn add_node(&mut self, kind: IlKind, children: &[usize]) -> usize {
        let start = self.children.len();
        self.children.extend(children);
        let end = self.children.len();

        self.add_il(Il::new(kind, start, end))
    }

    pub fn set_comment(&mut self, il: usize, comment: String) {
        let comment_id = self.comments.len();
        self.comments.push(comment);
        self.ils[il].comment = Some(comment_id);
    }
}
