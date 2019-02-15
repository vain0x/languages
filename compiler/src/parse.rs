use crate::*;

#[derive(Default)]
struct Parser<'a> {
    tokens: &'a [Token],
    token_spans: &'a [Span],
    current: usize,
    nodes: Vec<Node>,
    node_spans: Vec<Span>,
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

    fn add_node(&mut self, node: Node, l: TokenId) -> NodeId {
        assert!(l < self.current, "l={} r={}", l, self.current);
        let span = (self.token_spans[l].0, self.token_spans[self.current - 1].1);

        let node_id = self.nodes.len();
        self.nodes.push(node);
        self.node_spans.push(span);
        node_id
    }

    fn parse_ann(&mut self, node_id: NodeId, l: usize) -> NodeId {
        if let Token::Pun(":") = self.next() {
            self.current += 1;
            let ty_node_id = self.parse_node();
            return self.add_node(Node::Ann(node_id, ty_node_id), l);
        }

        node_id
    }

    fn parse_node(&mut self) -> NodeId {
        let l = self.current;

        if self.next_is_opening() {
            self.current += 1;
            let mut children = vec![];
            while !self.next_is_closing() && *self.next() != Token::Eof {
                children.push(self.parse_node());
            }
            if *self.next() != Token::Eof {
                self.current += 1;
            }
            let node_id = self.add_node(Node::App(children), l);
            return self.parse_ann(node_id, l);
        }
        if self.next_is_closing() {
            self.current += 1;
            let token_id = self.current;
            return self.add_node(Node::Err("Unmatched bracket".into(), token_id), l);
        }

        let token_id = self.current;
        self.current += 1;
        let node_id = self.add_node(Node::Value(token_id), l);
        self.parse_ann(node_id, l)
    }

    fn parse(&mut self) {
        self.parse_node();
    }
}

pub fn parse(src: String) -> Syntax {
    let (tokens, token_spans) = tokenize::tokenize(&src);

    let (nodes, node_spans) = {
        let mut parser = Parser {
            tokens: &tokens,
            token_spans: &token_spans,
            ..Parser::default()
        };
        parser.parse();
        (parser.nodes, parser.node_spans)
    };

    Syntax {
        src,
        tokens,
        token_spans,
        nodes,
        node_spans,
    }
}
