use super::*;
use crate::syntax::*;
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub struct DocMsg {
    level: MsgLevel,
    message: String,
    l: Pos,
    r: Pos,
}

#[derive(Clone, Debug)]
pub(crate) struct Msg {
    level: MsgLevel,
    message: String,
    exp_id: ExpId,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MsgLevel {
    Err,
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

    fn add_err_msg(&mut self, message: String, exp_id: ExpId) {
        self.add_msg(Msg::err(message, exp_id));
    }
}

impl Msg {
    pub(crate) fn err(message: String, exp_id: ExpId) -> Self {
        Msg {
            level: MsgLevel::Err,
            message,
            exp_id,
        }
    }

    pub fn is_successful(&self) -> bool {
        self.level != MsgLevel::Err
    }

    fn to_doc_msg(&self, syntax: &Syntax) -> DocMsg {
        let (l, r) = syntax.locate_exp(self.exp_id);
        DocMsg {
            l,
            r,
            level: self.level,
            message: self.message.to_string(),
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
            success = success && msg.level != MsgLevel::Err;
        }
        (success, result)
    }
}

impl DocMsg {
    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn start_pos(&self) -> Pos {
        self.l
    }

    pub fn end_pos(&self) -> Pos {
        self.r
    }

    pub fn to_text(msgs: &[DocMsg]) -> String {
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
