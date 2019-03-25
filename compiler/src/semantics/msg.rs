use super::*;
use crate::syntax::*;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Clone, Debug)]
pub(crate) struct DocMsg {
    level: MsgLevel,
    message: String,
    l: Pos,
    r: Pos,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum MsgLevel {
    Error,
}

#[derive(Clone, Debug)]
pub(crate) enum MsgKind {
    SyntaxError(String),
    Undefined,
    TypeMismatch(Ty, Ty),
    InvalidUseOfRange,
    OutOfLoop,
    Unimplemented(String),
    Unexpected(String),
}

#[derive(Clone, Debug)]
pub(crate) struct Msg {
    kind: MsgKind,
    exp_id: ExpId,
}

pub(crate) type Msgs = BTreeMap<MsgId, Msg>;

pub(crate) trait BorrowMutMsgs {
    fn msgs_mut(&mut self) -> &mut Msgs;

    fn next_msg_id(&mut self) -> MsgId {
        MsgId::new(self.msgs_mut().len())
    }

    fn add_msg(&mut self, msg: Msg) {
        let msg_id = self.next_msg_id();
        self.msgs_mut().insert(msg_id, msg);
    }

    fn add_err_msg(&mut self, kind: MsgKind, exp_id: ExpId) {
        self.add_msg(Msg { kind, exp_id });
    }
}

impl fmt::Display for MsgKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MsgKind::SyntaxError(message) => write!(f, "{}", message),
            MsgKind::Undefined => write!(f, "Undefined name"),
            MsgKind::TypeMismatch(..) => write!(f, "Type Error"),
            MsgKind::InvalidUseOfRange => write!(f, "Invalid use of range"),
            MsgKind::OutOfLoop => write!(f, "Out of loop"),
            MsgKind::Unimplemented(message) => write!(f, "{}", message),
            MsgKind::Unexpected(message) => write!(f, "{}", message),
        }
    }
}

impl Msg {
    pub(crate) fn is_successful(&self) -> bool {
        // Currently all messages are error.
        false
    }

    fn to_doc_msg(&self, syntax: &Syntax) -> DocMsg {
        let (l, r) = syntax.locate_exp(self.exp_id);
        DocMsg {
            l,
            r,
            level: MsgLevel::Error,
            message: format!("{}", self.kind),
        }
    }

    pub(crate) fn summarize<'a, I: Iterator<Item = &'a Msg>>(
        msgs: I,
        syntax: &Syntax,
    ) -> (bool, Vec<DocMsg>) {
        let mut success = true;
        let mut result = vec![];
        for msg in msgs {
            result.push(msg.to_doc_msg(syntax));
            success = success && msg.is_successful();
        }
        (success, result)
    }
}

impl DocMsg {
    pub(crate) fn message(&self) -> &str {
        &self.message
    }

    pub(crate) fn start_pos(&self) -> Pos {
        self.l
    }

    pub(crate) fn end_pos(&self) -> Pos {
        self.r
    }

    pub(crate) fn to_text(msgs: &[DocMsg]) -> String {
        use std::fmt::Write as _;

        let mut buffer = String::new();
        for msg in msgs {
            let (ly, lx) = msg.l;
            let (ry, rx) = msg.r;
            let (ly, lx, ry, rx) = (ly + 1, lx + 1, ry + 1, rx + 1);
            writeln!(buffer, "At {}:{}..{}:{} {}", ly, lx, ry, rx, msg.message).unwrap();
        }
        buffer
    }
}
