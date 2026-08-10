// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// The syntax tree of a Plimsoll model.
///
/// One tree shape serves two purposes: value expressions (`price * volume`)
/// and unit expressions (`kW*h/m^2`). They are syntactically identical -- both
/// are products of named things with rational exponents -- and reusing the tree
/// means the unit sublanguage gets the full expression parser for free.
/// `Name` resolves to a variable in the first context and to a unit in the
/// second.
module Plimsoll.Core.Ast

open Plimsoll.Core.Rational

type BinOp =
    | Add
    | Sub
    | Mul
    | Div

type Expr =
    /// A bare number, dimensionless.
    | Num of float
    /// A number with a unit annotation, e.g. `9.81 [m/s^2]`.
    | Quantity of float * Expr
    /// A variable (value context) or a unit symbol (unit context).
    | Name of string
    | Neg of Expr
    | Bin of BinOp * Expr * Expr
    /// Exponents are exact rationals, never expressions: dimensional soundness
    /// requires knowing the exponent at check time.
    | Pow of Expr * Rat
    | Call of string * Expr list

type RelOp =
    | Eq
    | Le
    | Ge

let relOpText =
    function
    | Eq -> "="
    | Le -> "<="
    | Ge -> ">="

type Stmt =
    /// `dimension money` -- introduces a new base dimension.
    | DimensionDecl of name: string * line: int
    /// `unit USD = money` / `unit kW = 1000 [W]` / `unit request` (fresh base).
    | UnitDecl of name: string * defn: Expr option * line: int
    /// `given price = 40 .. 60 [USD]`
    | Given of name: string * lo: Expr * hi: Expr * line: int
    /// `unknown thrust [N]`
    | Unknown of name: string * unitAnn: Expr option * line: int
    /// `revenue = price * volume`, `margin >= 0.3`
    | Relate of op: RelOp * lhs: Expr * rhs: Expr * line: int * text: string
    /// `plot price, volume` -- names the two axes for feasible-region paving.
    | Plot of x: string * y: string * line: int

type Model = { Statements: Stmt list }

let lineOf =
    function
    | DimensionDecl(_, l)
    | UnitDecl(_, _, l)
    | Given(_, _, _, l)
    | Unknown(_, _, l)
    | Relate(_, _, _, l, _)
    | Plot(_, _, l) -> l

/// Every `Name` occurring in an expression, in order of appearance.
let rec namesIn (e: Expr) : string list =
    match e with
    | Num _ -> []
    | Quantity _ -> [] // the unit part is not a value-level name
    | Name n -> [ n ]
    | Neg x -> namesIn x
    | Bin(_, a, b) -> namesIn a @ namesIn b
    | Pow(x, _) -> namesIn x
    | Call(_, args) -> args |> List.collect namesIn

/// Render an expression back to source-like text, for reports and messages.
let rec toString (e: Expr) : string =
    let prec =
        function
        | Bin(Add, _, _)
        | Bin(Sub, _, _) -> 1
        | Bin(Mul, _, _)
        | Bin(Div, _, _) -> 2
        | Neg _ -> 3
        | _ -> 4

    let wrap parent child =
        let s = toString child
        if prec child < parent then "(" + s + ")" else s

    match e with
    | Num v -> Interval.toString (Interval.point v)
    | Quantity(v, u) -> sprintf "%s [%s]" (Interval.toString (Interval.point v)) (toString u)
    | Name n -> n
    | Neg x -> "-" + wrap 3 x
    | Bin(op, a, b) ->
        let sym =
            match op with
            | Add -> " + "
            | Sub -> " - "
            | Mul -> "*"
            | Div -> "/"

        let p = prec e
        wrap p a + sym + wrap (p + 1) b
    | Pow(x, r) -> wrap 4 x + "^" + Rational.toString r
    | Call(f, args) -> f + "(" + (args |> List.map toString |> String.concat ", ") + ")"
