use crate::*;

#[derive(Clone, Debug)]
pub struct Msg {
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
        MsgId(self.msgs_mut().len())
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
    pub fn err(message: String, exp_id: ExpId) -> Self {
        Msg {
            level: MsgLevel::Err,
            message,
            exp_id,
        }
    }

    pub fn is_successful(&self) -> bool {
        self.level != MsgLevel::Err
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub(crate) fn summarize<'a, I: Iterator<Item = &'a Msg>>(
        msgs: I,
        syntax: &Syntax,
    ) -> (bool, String) {
        use std::fmt::Write;

        let mut success = true;
        let mut stderr = String::new();
        for msg in msgs {
            let ((ly, lx), (ry, rx)) = syntax.locate_exp(msg.exp_id);
            let (ly, lx, ry, rx) = (ly + 1, lx + 1, ry + 1, rx + 1);
            writeln!(stderr, "At {}:{}..{}:{} {}", ly, lx, ry, rx, msg.message).unwrap();
            success = success && msg.level != MsgLevel::Err;
        }
        (success, stderr)
    }
}
