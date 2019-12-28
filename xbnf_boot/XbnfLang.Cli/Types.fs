module rec XbnfLang.Types

open System.Collections.Generic

type HashMap<'Key, 'Value> = Dictionary<'Key, 'Value>

type Location = int * int

type IsNullableFun = string -> bool

type Comment = string

type Token =
  | EofToken

  | StrToken
    of string

  /// LIKE_THIS
  | LoudToken
    of string

  /// like_this
  | SnakeToken
    of string

  | PunToken
    of string

type TokenData =
  Token * int * int

type Term =
  | TokenTerm
    of string

  | SymbolTerm
    of string

  | OptTerm
    of TermData

  | ManyTerm
    of TermData

  | Many1Term
    of TermData

  | SepTerm
    of item:TermData * sep:string

  | Sep1Term
    of item:TermData * sep:string

  | ConcatTerm
    of TermData * TermData

  | OrTerm
    of TermData * TermData

type TermData =
  Term * Location

type StmtTerm =
  | RuleStmtTerm
    of string * TermData * Comment list * Location

type Node =
  | TokenNode
    of string

  | SymbolNode
    of string

  | EmptyNode

  | Many1Node
    of NodeData

  | ConcatNode
    of NodeData * NodeData

  | OrNode
    of NodeData * NodeData

type NodeData =
  Node * Location

type Rule =
  | Rule
    of string * NodeData * Comment list * Location
