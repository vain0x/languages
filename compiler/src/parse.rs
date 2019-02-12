use crate::*;

#[derive(Default)]
struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    nodes: Vec<Node>,
}

impl Parser<'_> {
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

    fn parse(&mut self) {
        self.parse_node();
    }
}

pub fn parse(src: String) -> Syntax {
    let (tokens, spans) = tokenize::tokenize(src);

    let mut parser = Parser {
        tokens: &tokens,
        ..Parser::default()
    };
    parser.parse();

    let nodes = parser.nodes;
    Syntax {
        tokens,
        spans,
        nodes,
    }
}
