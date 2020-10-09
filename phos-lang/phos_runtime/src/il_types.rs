use super::*;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct PilSymbol {
    text: &'static str,
}

impl PilSymbol {
    fn new(s: &str) -> Self {
        // TODO: インターン化
        Self {
            text: Box::leak(s.to_string().into_boxed_str()),
        }
    }

    pub(crate) fn as_str(self) -> &'static str {
        self.text
    }
}

impl Debug for PilSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub(crate) fn as_symbol(s: &str) -> PilSymbol {
    PilSymbol::new(s)
}

pub(crate) struct PilFnData {
    pub(crate) name: PilSymbol,

    /// codes
    pub(crate) codes: Vec<Vec<&'static str>>,
}

impl PilFnData {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: as_symbol(name),
            codes: vec![],
        }
    }
}

#[derive(Default)]
pub(crate) struct PilCheck {
    pub(crate) stdout: Option<Cell<&'static str>>,
    pub(crate) exit_code: Option<i32>,
}

#[derive(Default)]
pub(crate) struct PilModData {
    #[allow(unused)]
    pub(crate) name: String,

    #[allow(unused)]
    pub(crate) full_name: String,

    pub(crate) check: PilCheck,
    pub(crate) fns: HashMap<PilSymbol, PilFnData>,
}

impl PilModData {
    pub(crate) fn new(name: String, full_name: String) -> Self {
        Self {
            name,
            full_name,
            check: PilCheck::default(),
            fns: HashMap::default(),
        }
    }
}
