#[derive(Clone)]
pub(crate) enum Val {
    Str(String),
    Int(i64),
}

impl Val {
    pub(crate) fn to_str(&self) -> String {
        match self {
            Val::Str(value) => value.to_string(),
            Val::Int(value) => value.to_string(),
        }
    }

    pub(crate) fn to_int(&self) -> i64 {
        match self {
            Val::Str(value) => value
                .parse()
                .unwrap_or_else(|_| panic!("can't parse as int: {}", value)),
            Val::Int(value) => *value,
        }
    }
}
