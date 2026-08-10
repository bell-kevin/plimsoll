// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Recursive-descent / precedence-climbing parser for the Plimsoll language.
///
///     dimension money
///     unit USD = money
///     unit request
///
///     given price   = 40 .. 60 [USD]
///     given traffic = 2 .. 5 [request/s]
///     unknown revenue [USD]
///
///     revenue = price * volume
///     revenue >= 100_000 [USD]
///
///     plot price, volume
///
/// A statement ends at a line break. Expressions may be continued onto the next
/// line by leaving the operator at the end of the previous line.
module Plimsoll.Core.Parser

open Plimsoll.Core.Diagnostics
open Plimsoll.Core.Lexer
open Plimsoll.Core.Ast

type private State =
    { Toks: Token[]
      Lines: string[]
      mutable Pos: int
      /// Bracket nesting depth. Inside brackets, line breaks are insignificant.
      mutable Depth: int
      Warnings: ResizeArray<Diag> }

let private peek (s: State) = s.Toks.[s.Pos]

let private advance (s: State) =
    let t = s.Toks.[s.Pos]
    if s.Pos < s.Toks.Length - 1 then s.Pos <- s.Pos + 1
    t

let private isPunct (str: string) (t: Token) =
    match t.Kind with
    | TPunct p -> p = str
    | _ -> false

let private expectPunct (s: State) (str: string) =
    let t = peek s

    if isPunct str t then
        advance s |> ignore
    else
        fail t.Line (sprintf "expected '%s' but found %s" str (describe t.Kind))

let private expectIdent (s: State) (what: string) =
    let t = advance s

    match t.Kind with
    | TIdent n -> n
    | _ -> fail t.Line (sprintf "expected %s but found %s" what (describe t.Kind))

/// Source text of a statement's line, with any trailing comment removed. Used
/// verbatim in conflict reports, because a user recognises their own line
/// faster than a pretty-printed reconstruction of it.
let private sourceText (s: State) (line: int) =
    if line >= 1 && line <= s.Lines.Length then
        let raw = s.Lines.[line - 1]

        let cut =
            [ raw.IndexOf '#'; raw.IndexOf "//" ]
            |> List.filter (fun i -> i >= 0)
            |> function
                | [] -> raw.Length
                | xs -> List.min xs

        raw.Substring(0, cut).Trim()
    else
        ""

// ------------------------------------------------------------- expressions --

let private precOf =
    function
    | "+"
    | "-" -> 1
    | "*"
    | "/" -> 2
    | _ -> 0

let private binOf =
    function
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | o -> failwithf "not a binary operator: %s" o

/// Fold a parsed exponent down to an exact rational. Exponents must be
/// constants: `x^n` can only be dimensionally checked if `n` is known here.
let rec private constRat (line: int) (e: Expr) =
    let noDiv0 f =
        try
            f ()
        with :? System.DivideByZeroException ->
            fail line "division by zero in an exponent"

    match e with
    | Num v ->
        match Rational.tryOfFloat v with
        | Some r -> r
        | None -> fail line (sprintf "exponent %g is not a simple rational" v)
    | Neg x -> Rational.neg (constRat line x)
    | Bin(Add, a, b) -> Rational.add (constRat line a) (constRat line b)
    | Bin(Sub, a, b) -> Rational.sub (constRat line a) (constRat line b)
    | Bin(Mul, a, b) -> Rational.mul (constRat line a) (constRat line b)
    | Bin(Div, a, b) -> noDiv0 (fun () -> Rational.div (constRat line a) (constRat line b))
    | _ ->
        failWith
            line
            "an exponent must be a constant rational"
            "write x^2, x^-1 or x^(1/2); a variable exponent has no fixed dimension"

let rec private parseExpr (s: State) (minPrec: int) : Expr =
    let mutable left = parseUnary s
    let mutable go = true

    while go do
        let t = peek s

        match t.Kind with
        | TPunct p when precOf p > 0 && precOf p >= minPrec ->
            // A line that *begins* with an operator starts a new statement,
            // unless we are inside brackets.
            if t.StartsLine && s.Depth = 0 then
                go <- false
            else
                advance s |> ignore
                let right = parseExpr s (precOf p + 1)
                left <- Bin(binOf p, left, right)
        | _ -> go <- false

    left

and private parseUnary (s: State) : Expr =
    let t = peek s

    if isPunct "-" t then
        advance s |> ignore
        Neg(parseUnary s)
    elif isPunct "+" t then
        advance s |> ignore
        parseUnary s
    else
        parsePower s

and private parsePower (s: State) : Expr =
    let b = parsePrimary s
    let t = peek s

    if isPunct "^" t then
        advance s |> ignore
        let e = parseUnary s
        Pow(b, constRat t.Line e)
    else
        b

/// A bracketed unit expression: `[kW*h/m^2]`.
and private parseUnitAnnotation (s: State) : Expr =
    expectPunct s "["
    s.Depth <- s.Depth + 1
    let u = parseExpr s 1
    s.Depth <- s.Depth - 1
    expectPunct s "]"
    u

and private parsePrimary (s: State) : Expr =
    let t = advance s

    match t.Kind with
    | TNum v ->
        // A unit annotation binds to the literal it follows.
        if isPunct "[" (peek s) then Quantity(v, parseUnitAnnotation s) else Num v
    | TIdent name ->
        if isPunct "(" (peek s) then
            advance s |> ignore
            s.Depth <- s.Depth + 1

            let args =
                if isPunct ")" (peek s) then
                    []
                else
                    let first = parseExpr s 1
                    let rest = ResizeArray<Expr>()

                    while isPunct "," (peek s) do
                        advance s |> ignore
                        rest.Add(parseExpr s 1)

                    first :: List.ofSeq rest

            s.Depth <- s.Depth - 1
            expectPunct s ")"
            Call(name, args)
        else
            Name name
    | TPunct "(" ->
        s.Depth <- s.Depth + 1
        let e = parseExpr s 1
        s.Depth <- s.Depth - 1
        expectPunct s ")"
        e
    | k -> fail t.Line (sprintf "expected a value but found %s" (describe k))

// ------------------------------------------------------------------ ranges --

let rec private isBareLiteral =
    function
    | Num _ -> true
    | Neg x -> isBareLiteral x
    | _ -> false

let rec private unitOf =
    function
    | Quantity(_, u) -> Some u
    | Neg x -> unitOf x
    | _ -> None

let rec private attachUnit (u: Expr) (e: Expr) =
    match e with
    | Num v -> Quantity(v, u)
    | Neg x -> Neg(attachUnit u x)
    | other -> other

/// `40 .. 60 [USD]` is what people write, so a bare literal bound inherits the
/// unit of the bound that has one. Explicit units on both sides also work.
let private parseRange (s: State) =
    let lo = parseExpr s 1

    if isPunct ".." (peek s) then
        advance s |> ignore
        let hi = parseExpr s 1

        match isBareLiteral lo, unitOf hi, unitOf lo, isBareLiteral hi with
        | true, Some u, _, _ -> attachUnit u lo, hi
        | _, _, Some u, true -> lo, attachUnit u hi
        | _ -> lo, hi
    else
        lo, lo

// -------------------------------------------------------------- statements --

let private parseStmt (s: State) : Stmt option =
    let t = peek s

    match t.Kind with
    | TEof -> None
    | TKeyword "dimension" ->
        advance s |> ignore
        Some(DimensionDecl(expectIdent s "a dimension name", t.Line))
    | TKeyword "unit" ->
        advance s |> ignore
        let name = expectIdent s "a unit name"

        if isPunct "=" (peek s) then
            advance s |> ignore
            Some(UnitDecl(name, Some(parseExpr s 1), t.Line))
        else
            Some(UnitDecl(name, None, t.Line))
    | TKeyword "given" ->
        advance s |> ignore
        let name = expectIdent s "a variable name"
        expectPunct s "="
        let lo, hi = parseRange s
        Some(Given(name, lo, hi, t.Line))
    | TKeyword "unknown" ->
        advance s |> ignore
        let name = expectIdent s "a variable name"

        let ann =
            if isPunct "[" (peek s) then Some(parseUnitAnnotation s) else None

        Some(Unknown(name, ann, t.Line))
    | TKeyword "plot" ->
        advance s |> ignore
        let x = expectIdent s "a variable name"
        expectPunct s ","
        let y = expectIdent s "a variable name"
        Some(Plot(x, y, t.Line))
    | _ ->
        let lhs = parseExpr s 1
        let opTok = peek s

        let op =
            match opTok.Kind with
            | TPunct "=" -> Eq
            | TPunct "<=" -> Le
            | TPunct ">=" -> Ge
            | TPunct "<" ->
                s.Warnings.Add(
                    warningWith
                        opTok.Line
                        "'<' is treated as '<='"
                        "a closed interval cannot exclude its own endpoint, so strict inequalities are relaxed"
                )

                Le
            | TPunct ">" ->
                s.Warnings.Add(
                    warningWith
                        opTok.Line
                        "'>' is treated as '>='"
                        "a closed interval cannot exclude its own endpoint, so strict inequalities are relaxed"
                )

                Ge
            | k ->
                failWith
                    opTok.Line
                    (sprintf "expected a relation (=, <= or >=) but found %s" (describe k))
                    "a bare expression is not a statement; relate it to something"

        advance s |> ignore
        let rhs = parseExpr s 1
        Some(Relate(op, lhs, rhs, t.Line, sourceText s t.Line))

/// Parse a whole model. Returns the model plus any warnings, or the first error.
let parse (src: string) : Result<Model * Diag list, Diag> =
    try
        let toks = tokenize src |> Array.ofList

        let s =
            { Toks = toks
              Lines = src.Replace("\r\n", "\n").Split('\n')
              Pos = 0
              Depth = 0
              Warnings = ResizeArray() }

        let stmts = ResizeArray<Stmt>()
        let mutable go = true

        while go do
            match parseStmt s with
            | Some st -> stmts.Add st
            | None -> go <- false

        Ok({ Statements = List.ofSeq stmts }, List.ofSeq s.Warnings)
    with PlimError d ->
        Error d
