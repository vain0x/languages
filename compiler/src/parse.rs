use crate::*;

#[derive(Default)]
pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub nodes: Vec<Node>,
}

impl Parser {
    fn next(&self) -> &Token {
        if self.current >= self.tokens.len() {
            return EOF;
        }
        &self.tokens[self.current]
    }

    fn next_is_opening(&self) -> bool {
        match self.next() {
            &Token::Pun("(") => true,
            _ => false,
        }
    }

    fn next_is_closing(&self) -> bool {
        match self.next() {
            &Token::Pun(")") => true,
            _ => false,
        }
    }

    fn add_node(&mut self, node: Node) -> NodeId {
        let node_id = self.nodes.len();
        self.nodes.push(node);
        node_id
    }

    fn parse_node(&mut self) -> NodeId {
        if self.next_is_opening() {
            self.current += 1;
            let mut children = vec![];
            while !self.next_is_closing() && *self.next() != Token::Eof {
                children.push(self.parse_node());
            }
            if *self.next() != Token::Eof {
                self.current += 1;
            }
            return self.add_node(Node::App(children));
        }
        if self.next_is_closing() {
            self.current += 1;
            let token_id = self.current;
            return self.add_node(Node::Err("Unmatched bracket".into(), token_id));
        }

        let token_id = self.current;
        self.current += 1;
        self.add_node(Node::Value(token_id))
    }

    pub fn parse(mut self) -> Vec<Node> {
        self.parse_node();
        self.nodes
    }
}
