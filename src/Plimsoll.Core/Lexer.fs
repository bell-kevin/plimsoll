// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Tokeniser for the Plimsoll model language.
module Plimsoll.Core.Lexer

open System
open System.Text
open Plimsoll.Core.Diagnostics

type TokKind =
    | TNum of float
    | TIdent of string
    | TKeyword of string
    | TPunct of string
    | TEof

type Token =
    { Kind: TokKind
      Line: int
      /// True when this is the first token on its source line. The parser uses
      /// this to end a statement at a line break, so that a line beginning with
      /// `-` reads as a new statement rather than a subtraction continuing the
      /// previous one.
      StartsLine: bool }

let keywords =
    set [ "dimension"; "unit"; "given"; "unknown"; "plot" ]

let private isIdentStart (c: char) = Char.IsLetter c || c = '_'
let private isIdentChar (c: char) = Char.IsLetterOrDigit c || c = '_'

/// Single characters that act as complete identifiers, so that `%` and `$` can
/// be used as unit symbols.
let private soloIdents = set [ '%'; '$' ]

let tokenize (src: string) : Token list =
    let toks = ResizeArray<Token>()
    let mutable i = 0
    let mutable line = 1
    let mutable atLineStart = true
    let n = src.Length

    let emit kind =
        toks.Add
            { Kind = kind
              Line = line
              StartsLine = atLineStart }

        atLineStart <- false

    while i < n do
        let c = src.[i]

        if c = '\n' then
            line <- line + 1
            atLineStart <- true
            i <- i + 1
        elif c = '\r' || c = ' ' || c = '\t' then
            i <- i + 1
        elif c = '#' || (c = '/' && i + 1 < n && src.[i + 1] = '/') then
            // Comment to end of line.
            while i < n && src.[i] <> '\n' do
                i <- i + 1
        elif Char.IsDigit c || (c = '.' && i + 1 < n && Char.IsDigit src.[i + 1]) then
            let sb = StringBuilder()
            let start = i
            let mutable seenDot = false
            let mutable go = true

            while go && i < n do
                let d = src.[i]

                if Char.IsDigit d then
                    sb.Append d |> ignore
                    i <- i + 1
                elif d = '_' then
                    // Digit grouping: 1_000_000.
                    i <- i + 1
                elif d = '.' && not seenDot && i + 1 < n && src.[i + 1] <> '.' then
                    seenDot <- true
                    sb.Append d |> ignore
                    i <- i + 1
                elif d = '.' && not seenDot && i + 1 >= n then
                    seenDot <- true
                    sb.Append d |> ignore
                    i <- i + 1
                elif (d = 'e' || d = 'E') && i + 1 < n && (Char.IsDigit src.[i + 1] || ((src.[i + 1] = '+' || src.[i + 1] = '-') && i + 2 < n && Char.IsDigit src.[i + 2])) then
                    sb.Append 'e' |> ignore
                    i <- i + 1

                    if src.[i] = '+' || src.[i] = '-' then
                        sb.Append src.[i] |> ignore
                        i <- i + 1
                else
                    go <- false

            let text = sb.ToString()

            match Double.TryParse(text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> emit (TNum v)
            | _ -> fail line (sprintf "cannot read '%s' as a number" (src.Substring(start, i - start)))
        elif isIdentStart c then
            let start = i

            while i < n && isIdentChar src.[i] do
                i <- i + 1

            let word = src.Substring(start, i - start)
            if keywords.Contains word then emit (TKeyword word) else emit (TIdent word)
        elif soloIdents.Contains c then
            emit (TIdent(string c))
            i <- i + 1
        else
            // Two-character operators must be tried before single characters.
            let two = if i + 1 < n then src.Substring(i, 2) else ""

            match two with
            | ".." | "<=" | ">=" | "==" ->
                emit (TPunct(if two = "==" then "=" else two))
                i <- i + 2
            | _ ->
                match c with
                | '+' | '-' | '*' | '/' | '^' | '(' | ')' | '[' | ']' | ',' | '=' | '<' | '>' ->
                    emit (TPunct(string c))
                    i <- i + 1
                | '·' // MIDDLE DOT, so `kg·m` can be written directly
                | '×' -> // MULTIPLICATION SIGN
                    emit (TPunct "*")
                    i <- i + 1
                | '−' -> // MINUS SIGN
                    emit (TPunct "-")
                    i <- i + 1
                | '≤' ->
                    emit (TPunct "<=")
                    i <- i + 1
                | '≥' ->
                    emit (TPunct ">=")
                    i <- i + 1
                | _ -> fail line (sprintf "unexpected character '%c'" c)

    toks.Add
        { Kind = TEof
          Line = line
          StartsLine = true }

    List.ofSeq toks

let describe (k: TokKind) =
    match k with
    | TNum v -> sprintf "number %g" v
    | TIdent s -> sprintf "'%s'" s
    | TKeyword s -> sprintf "keyword '%s'" s
    | TPunct s -> sprintf "'%s'" s
    | TEof -> "end of input"
