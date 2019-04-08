#[derive(Clone, Copy, PartialEq, Debug)]
pub(crate) enum Pun {
    ParenL,
    ParenR,
    BracketL,
    BracketR,
    BraceL,
    BraceR,
    Comma,
    Semi,
}

const PUNS: &[(Pun, &str)] = &[
    (Pun::ParenL, "("),
    (Pun::ParenR, ")"),
    (Pun::BracketL, "["),
    (Pun::BracketR, "]"),
    (Pun::BraceL, "{"),
    (Pun::BraceR, "}"),
    (Pun::Comma, ","),
    (Pun::Semi, ";"),
];

impl Pun {
    pub(crate) fn get_all() -> Vec<Pun> {
        PUNS.iter().map(|&(pun, _)| pun).collect()
    }

    pub(crate) fn text(self) -> &'static str {
        PUNS.iter()
            .filter_map(|&(pun, text)| if pun == self { Some(text) } else { None })
            .next()
            .unwrap_or_else(|| panic!("Unknown pun {:?}", self))
    }
}
