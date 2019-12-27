module XbnfLang.Types

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

type Term =
  | StrTerm
    of string * Location

  | TokenTerm
    of string * Location

  | SymbolTerm
    of string * Location

  | OptTerm
    of Term * Location

  | ManyTerm
    of Term * Location

  | Many1Term
    of Term * Location

  | SepTerm
    of item:Term * sep:string * Location

  | Sep1Term
    of item:Term * sep:string * Location

  | ConcatTerm
    of Term * Term * Location

  | OrTerm
    of Term * Term * Location

type StmtTerm =
  | RuleStmtTerm
    of string * Term * Location

type Node =
  | StrNode
    of string * Location

  | TokenNode
    of string * Location

  | SymbolNode
    of string * Location

  | EmptyNode
    of Location

  | Many1Node
    of Node * Location

  | ConcatNode
    of Node * Node * Location

  | OrNode
    of Node * Node * Location

type Rule =
  | Rule
    of string * Node * Location
