use super::*;

#[derive(Clone)]
pub(crate) enum Val {
    Str(String),
    Int(i64),
    Obj(PilSymbol, Vec<(PilSymbol, Val)>),
}

impl Val {
    pub(crate) fn to_str(&self) -> String {
        match self {
            Val::Str(value) => value.to_string(),
            Val::Int(value) => value.to_string(),
            Val::Obj(tag, fields) => {
                let mut s = tag.as_str().to_string();
                if !fields.is_empty() {
                    let fields = fields
                        .iter()
                        .map(|(name, val)| match name {
                            PilSymbol::Str(name) => format!("{}: {}", name, val.to_str()),
                            PilSymbol::Index(_) => val.to_str(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    s += &format!("({})", fields);
                }
                s
            }
        }
    }

    pub(crate) fn to_int(&self) -> i64 {
        match self {
            Val::Str(value) => value
                .parse()
                .unwrap_or_else(|_| panic!("can't parse as int: {}", value)),
            Val::Int(value) => *value,
            Val::Obj(tag, _) => panic!("can't convert object to int: {}", tag),
        }
    }
}
