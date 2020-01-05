//! 字句解析の汎用ライブラリ

use std::cell::Cell;

pub trait TokenKindTrait: Sized {
    fn error() -> Self;

    fn eof() -> Self;

    /// トークンがトリビア (空白やコメントなど、構文解析に使用しないもの) か？
    fn is_trivia(&self) -> bool;
}

pub trait TokenFactoryTrait: Sized {
    type Token;
    type Kind: TokenKindTrait;

    fn new_token(&self, kind: Self::Kind, start: usize, end: usize) -> Self::Token;
}

pub struct Tokenizer<'a, T: TokenFactoryTrait> {
    /// トークンを生成するもの
    token_factory: T,

    /// 字句解析の対象となるテキスト
    text: &'a str,

    /// 直前に解析が完了したトークンの終端位置 (= 次のトークンの開始位置)
    last: usize,

    /// 現在位置
    current: usize,

    /// 解析済みのトークンのリスト
    tokens: Vec<T::Token>,

    /// 無限ループ対策のカウンター。一定値を超えるとパニックする。
    tick: Cell<usize>,

    /// 行き詰まり状態 (適用できるルールがない状態) のとき true
    /// これが true のままループが一周したら次の文字はエラーとして読み飛ばす。
    stuck: bool,
}

impl<'a, T: TokenFactoryTrait> Tokenizer<'a, T> {
    pub fn new(token_factory: T, text: &'a str) -> Self {
        Tokenizer {
            token_factory,
            text,
            last: 0,
            current: 0,
            tokens: Vec::with_capacity(512),
            tick: Cell::default(),
            stuck: false,
        }
    }

    /// 前進する。末尾に達しているときは何もしない。
    pub fn bump(&mut self) {
        if self.at_eof() {
            self.detect_infinite_loop();
            return;
        }

        self.current += self.text[self.current..].chars().next().unwrap().len_utf8();
    }

    /// トークンを生成する。
    /// 直前に生成したトークンの終端位置から現在位置までの範囲を、新しく作られるトークンの範囲とする。
    pub fn add_token(&mut self, kind: T::Kind) {
        assert!(self.last <= self.current && self.current <= self.text.len());

        if !T::Kind::is_trivia(&kind) {
            let token = self.token_factory.new_token(kind, self.last, self.current);
            self.tokens.push(token);
        }

        self.last = self.current;
        self.stuck = false;
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get() + 1;

        assert!(tick < 10_000_000);
        self.tick.set(tick);
    }

    pub fn at_eof(&self) -> bool {
        self.current >= self.text.len()
    }

    /// 次の文字
    pub fn next(&self) -> u8 {
        self.detect_infinite_loop();

        if self.at_eof() {
            return 0;
        }

        self.text.as_bytes()[self.current]
    }

    /// 次に作るトークンにいずれかの文字が含まれるか？
    fn ate(&self) -> bool {
        self.last < self.current
    }

    /// 次に作るトークンの文字列
    pub fn current_text(&self) -> &str {
        &self.text[self.last..self.current]
    }

    /// 条件を満たすかぎり前進する。
    pub fn eat_while(&mut self, pred: impl Fn(u8) -> bool) -> bool {
        while !self.at_eof() && pred(self.next()) {
            self.bump();
        }
        self.ate()
    }

    /// 特定の文字列を読み飛ばす。
    /// 現在位置の直後に特定の文字列が後続している場合、その範囲だけ前進する。
    pub fn eat(&mut self, prefix: &str) -> bool {
        if self.text[self.current..].starts_with(prefix) {
            self.current += prefix.len();
        }
        self.ate()
    }

    fn error(&mut self) {
        if self.stuck {
            self.bump();
            self.add_token(T::Kind::error());
        }
        self.stuck = true;
    }

    fn eof(&mut self) {
        self.add_token(T::Kind::eof());
    }

    pub fn tokenize(mut self, any: impl Fn(&mut Self)) -> Vec<T::Token> {
        while !self.at_eof() {
            any(&mut self);
            self.error();
        }
        self.eof();
        self.tokens
    }
}
