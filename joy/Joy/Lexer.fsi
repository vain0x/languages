module Lexer

open FSharp.Text.Lexing
open Parser/// Rule read
val read: lexbuf: LexBuffer<char> -> token
