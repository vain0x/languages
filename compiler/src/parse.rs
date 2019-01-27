use crate::*;

#[derive(Default)]
pub struct Parser {
    pub toks: Toks,
    pub cur: usize,
    pub syns: Vec<Syn>,
}

impl Parser {
    fn next(&self) -> &Tok {
        if self.cur >= self.toks.len() {
            return EOF;
        }
        &self.toks[self.cur].0
    }

    fn next_is_opening(&self) -> bool {
        match self.next() {
            &Tok::Pun("(") => true,
            _ => false,
        }
    }

    fn next_is_closing(&self) -> bool {
        match self.next() {
            &Tok::Pun(")") => true,
            _ => false,
        }
    }

    fn push(&mut self, syn: Syn) -> SynId {
        let syn_id = self.syns.len();
        self.syns.push(syn);
        syn_id
    }

    fn read_exp(&mut self) -> SynId {
        if self.next_is_opening() {
            self.cur += 1;
            let mut children = vec![];
            while !self.next_is_closing() && *self.next() != Tok::Eof {
                children.push(self.read_exp());
            }
            if *self.next() != Tok::Eof {
                self.cur += 1;
            }
            return self.push(Syn::App(children));
        }
        if self.next_is_closing() {
            self.cur += 1;
            let tok_id = self.cur;
            return self.push(Syn::Err("Unmatched bracket".into(), tok_id));
        }

        let tok_id = self.cur;
        self.cur += 1;
        self.push(Syn::Val(tok_id))
    }

    pub fn parse(mut self) -> Vec<Syn> {
        self.read_exp();
        self.syns
    }
}
