module rec JoyLang.Program

open System

// -----------------------------------------------
// Helpers
// -----------------------------------------------

let expectSome opt =
    match opt with
    | Some x ->
        x

    | None ->
        failwithf "Expected Some but None"

type Vec<'T> = ResizeArray<'T>

let vecNew () =
    Vec()

let vecPush x (v: Vec<'T>) =
    v.Add(x)

let vecPop (v: Vec<'T>) =
    assert (v.Count >= 1)
    let n = v.Count
    let x = v.[n - 1]
    v.RemoveAt(n - 1) |> ignore
    x

let vecLength (v: Vec<'T>) =
    v.Count

let strSlice (l: int) (r: int) (s: string) =
    if l < r then
        s.[l..r - 1]
    else
        ""

let strTakeWhileCount (i: int) pred (s: string) =
    let rec go n i =
        if i < s.Length && pred s.[i] then
            go (n + 1) (i + 1)
        else
            n

    go 0 i

// -----------------------------------------------
// Sources
// -----------------------------------------------

type SFileId =
    | SFileId
        of int

type SLoc =
    | SLoc of SFileId * int * int

// -----------------------------------------------
// Tokens
// -----------------------------------------------

type TKind =
    | TEof
    | TIdent
    | TInt
    | TFn
    | TLet
    | TMut
    | TRef
    | TReturn
    | TLeftParen
    | TRightParen
    | TLeftBrace
    | TRightBrace
    | TLeftAngle
    | TRightAngle
    | TLeftSlimArrow
    | TRightSlimArrow
    | TEqual
    | TEqualEqual
    | TPlus
    | TPlusEqual
    | TMinus
    | TSemi

type TData =
    | TData
        of TKind * string * SLoc

let tKindParse s =
    match s with
    | "fn" ->
        TFn

    | "let" ->
        TLet

    | "mut" ->
        TMut

    | "ref" ->
        TRef

    | "return" ->
        TReturn

    | _ ->
        TIdent

let tDataToKind (TData (kind, _, _)) = kind

let tDataToText (TData (_, text, _)) = text

let tDataToLoc (TData (_, _, loc)) = loc

let tokenize (fileId: SFileId) (text: string) =
    let tokens = Vec()
    let mutable i = 0

    let nth d =
        if i + d < text.Length then
            text.[i + d]
        else
            '\x00'

    let skipToken (len: int) =
        i <- i + len

    let addToken kind (len: int) =
        let s = text |> strSlice i (i + len)
        let loc = SLoc (fileId, i, i + len)
        tokens.Add(TData (kind, s, loc))
        i <- i + len

    let rec go () =
        if i >= text.Length then () else

        match nth 0 with
        | ' '
        | '\t'
        | '\r'
        | '\n' ->
            let len = text |> strTakeWhileCount i Char.IsWhiteSpace
            skipToken len

        | '(' ->
            addToken TLeftParen 1

        | ')' ->
            addToken TRightParen 1

        | '{' ->
            addToken TLeftBrace 1

        | '}' ->
            addToken TRightBrace 1

        | '<' ->
            match nth 1 with
            | '-' ->
                addToken TLeftSlimArrow 2

            | _ ->
                addToken TLeftAngle 1

        | '=' ->
            match nth 1 with
            | '=' ->
                addToken TEqualEqual 2

            | _ ->
                addToken TEqual 1

        | '-' ->
            match nth 1 with
            | '>' ->
                addToken TRightSlimArrow 2

            | _ ->
                addToken TMinus 1

        | '+' ->
            match nth 1 with
            | '=' ->
                addToken TPlusEqual 2

            | _ ->
                addToken TPlus 1

        | ';' ->
            addToken TSemi 1

        | c when Char.IsDigit(c) ->
            let len = text |> strTakeWhileCount i Char.IsDigit
            addToken TInt len

        | c when Char.IsLetter(c) || c = '_' ->
            let len = text |> strTakeWhileCount i (fun c -> Char.IsLetterOrDigit(c) || c = '_')
            let ident = text |> strSlice i (i + len)
            let kind = tKindParse ident
            addToken kind len

        | c ->
            failwithf "unknown char %A" c

        go ()

    go ()

    for _ in 0..10 do
        addToken TEof 0

    tokens

// -----------------------------------------------
// Parsing
// -----------------------------------------------

type BinOp =
    | BinAssignOp
    | BinAddAssignOp
    | BinAddOp
    | BinSubOp

/// Parser context.
type Px(tokens: Vec<TData>) =
    let mutable i = 0

    member _.Error(message) =
        let t = tokens.[i]
        failwithf "Parse error at (%A): %s" t message

    member _.Nth(n) =
        tokens.[i + n] |> tDataToKind

    member _.Next =
        tokens.[i] |> tDataToKind

    member _.Bump() =
        let t = tokens.[i]
        i <- i + 1
        eprintfn "bump %A" t
        t

    member this.Eat(kind) =
        if this.Next = kind then
            this.Bump() |> Some
        else
            None

    member this.Expect(kind) =
        if this.Next = kind then
            this.Bump()
        else
            this.Error(sprintf "Expected %A" kind)

let parseIntLit (p: Px) =
    let (TData (_, text, loc)) = p.Expect(TInt)
    AIntLit (text, loc)

let parseName (p: Px) =
    let (TData (_, text, loc)) = p.Expect(TIdent)
    AName (text, loc)

let parseMutExpr (p: Px) =
    let mutLoc = p.Expect(TMut) |> tDataToLoc
    let name = parseName p
    AMutExpr (name, mutLoc)

let parseBlock onContent onEmpty (p: Px) =
    match p.Eat(TLeftBrace) with
    | Some leftBrace ->
        let content = onContent leftBrace

        while p.Next <> TEof && p.Next <> TRightBrace do
            p.Bump() |> ignore

        p.Eat(TRightBrace) |> ignore
        content

    | None ->
        match p.Eat(TSemi) with
        | Some _ ->
            onEmpty ()

        | None ->
            p.Error "Expected {...} or ;"

let parseLetExpr (p: Px) =
    let letLoc = p.Expect(TLet) |> tDataToLoc
    let name = parseName p
    ALetExpr (name, letLoc)

let parseAtomicExpr (p: Px) =
    match p.Next with
    | TInt ->
        parseIntLit p |> ALitExpr

    | TIdent ->
        parseName p |> ANameExpr

    | TMut ->
        parseMutExpr p

    | TLet ->
        parseLetExpr p

    | _ ->
        p.Error "Expected an expr"

let parseAdditiveBinExpr (p: Px) =
    let left = parseAtomicExpr p

    let rec go left =
        let gogo op =
            let loc = p.Bump() |> tDataToLoc
            let right = parseAtomicExpr p
            go (ABinExpr (op, left, right, loc))

        match p.Next with
        | TPlus ->
            gogo BinAddOp

        | TMinus ->
            gogo BinSubOp

        | _ ->
            left

    go left

let parseAssignExpr (p: Px) =
    let left = parseAdditiveBinExpr p

    let rec go left =
        let gogo op =
            let loc = p.Bump() |> tDataToLoc
            let right = parseAdditiveBinExpr p
            go (ABinExpr (op, left, right, loc))

        match p.Next with
        | TEqual ->
            gogo BinAssignOp

        | TPlusEqual ->
            gogo BinAddAssignOp

        | _ ->
            left

    go left

let parseExpr (p: Px) =
    parseAssignExpr p

let parseExprStmt (p: Px) =
    parseExpr p |> AExprStmt

let parseReturnStmt (p: Px) =
    let returnLoc = p.Expect(TReturn) |> tDataToLoc

    p.Expect(TSemi) |> ignore

    AReturnStmt returnLoc

let parseFnStmt (p: Px) =
    let fnLoc = p.Expect(TFn) |> tDataToLoc

    let fnName = parseName p

    p.Eat(TLeftParen) |> ignore
    p.Eat(TRightParen) |> ignore

    let body =
        let onContent leftBrace =
            let blockLoc = leftBrace |> tDataToLoc
            parseSemiStmt blockLoc p

        let onEmpty () =
            ASemiStmt (Vec(), fnLoc)

        p |> parseBlock onContent onEmpty

    AFnStmt (fnName, body, fnLoc)

let parseSemiStmt semiLoc (p: Px) =
    let stmts = Vec()

    let rec go () =
        match p.Next with
        | TRightBrace
        | TEof ->
            ASemiStmt (stmts, semiLoc)

        | TSemi ->
            p.Bump() |> ignore
            go ()

        | _ ->
            stmts.Add(parseStmt p)
            go ()

    go ()

let parseStmt (p: Px) =
    match p.Next with
    | TReturn ->
        parseReturnStmt p

    | TFn ->
        parseFnStmt p

    | _ ->
        parseExprStmt p

let parse (tokens: Vec<TData>) =
    let p = Px(tokens)

    let rootLoc =
        match tokens.[tokens.Count - 1] with
        | TData (TEof, _, SLoc (fileId, _, len)) ->
            SLoc (fileId, 0, len)

        | _ ->
            failwith "NEVER: EOF must exist"

    parseSemiStmt rootLoc p

// -----------------------------------------------
// Abstract syntax trees
// -----------------------------------------------

type AName =
    | AName
        of string * SLoc

type ALit =
    | AIntLit
        of string * SLoc

type AExpr =
    | ALitExpr
        of ALit

    | ANameExpr
        of AName

    | AMutExpr
        of AName * SLoc

    | ALetExpr
        of AName * SLoc

    | ABinExpr
        of BinOp * AExpr * AExpr * SLoc

type AStmt =
    | AExprStmt
        of AExpr

    | AReturnStmt
        of SLoc

    | ASemiStmt
        of Vec<AStmt> * SLoc

    | AFnStmt
        of AName * AStmt * SLoc

// -----------------------------------------------
// CPS
// -----------------------------------------------

type Mutability =
    | DefaultPass
    | Mut
    | RefMut

type KPrim =
    | KAddPrim
    | KSubPrim
    | KAssignPrim
    | KAddAssignPrim

type KTerm =
    | KName
        of string * SLoc

    | KIntTerm
        of string * SLoc

type KParam =
    | KParam
        of string * SLoc

type KFix =
    | KFix
        of KParam list * KNode

type KNode =
    | KPrimNode
        of KPrim * KNode

    | KJumpNode
        of KPrim * KNode

    | KFixNode
        of KFix list * KNode

// -----------------------------------------------
// Use cases
// -----------------------------------------------

let compile (text: string) =
    let fileId = SFileId 1
    let tokens = tokenize fileId text
    parse tokens

[<EntryPoint>]
let main _ =
    let text = """
        fn main() {
            let mut a = 2 + 3 + 5;
            mut a += 1;
            return;
        }
    """
    printfn "%A" (compile text)
    0
