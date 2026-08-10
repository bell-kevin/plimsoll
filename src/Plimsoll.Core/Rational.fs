// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Exact rational arithmetic, used for dimension exponents.
///
/// Dimension exponents must be exact: `sqrt(area)` has exponent 1/2 and
/// `1/2 + 1/2` must be *exactly* 1 or dimensional checking would drift.
/// Floats cannot promise that, so we carry num/den in lowest terms.
module Plimsoll.Core.Rational

open System

[<CustomEquality; CustomComparison>]
type Rat =
    { Num: int64
      Den: int64 } // invariant: Den > 0, gcd(|Num|, Den) = 1

    override this.Equals(o) =
        match o with
        | :? Rat as r -> this.Num = r.Num && this.Den = r.Den
        | _ -> false

    override this.GetHashCode() = hash (this.Num, this.Den)

    interface IComparable with
        member this.CompareTo(o) =
            match o with
            | :? Rat as r -> compare (this.Num * r.Den) (r.Num * this.Den)
            | _ -> invalidArg "o" "cannot compare Rat with other types"

let rec private gcd (a: int64) (b: int64) = if b = 0L then abs a else gcd b (a % b)

/// Build a rational in lowest terms with a positive denominator.
let create (n: int64) (d: int64) =
    if d = 0L then
        raise (DivideByZeroException "rational with zero denominator")

    let s = if d < 0L then -1L else 1L
    let n, d = n * s, d * s
    let g = gcd n d
    let g = if g = 0L then 1L else g
    { Num = n / g; Den = d / g }

let ofInt (n: int) = { Num = int64 n; Den = 1L }

let zero = ofInt 0
let one = ofInt 1

let add a b = create (a.Num * b.Den + b.Num * a.Den) (a.Den * b.Den)
let neg a = { a with Num = -a.Num }
let sub a b = add a (neg b)
let mul a b = create (a.Num * b.Num) (a.Den * b.Den)
let div a b = create (a.Num * b.Den) (a.Den * b.Num)

let isZero a = a.Num = 0L
let isInt a = a.Den = 1L

/// The integer value, when `isInt` holds.
let toInt a = int a.Num
let toFloat a = float a.Num / float a.Den

/// Superscript rendering so dimensions read like `kg·m·s⁻²` rather than `kg*m*s^-2`.
let private superscript (s: string) =
    s
    |> String.map (fun c ->
        match c with
        | '0' -> '⁰'
        | '1' -> '¹'
        | '2' -> '²'
        | '3' -> '³'
        | '4' -> '⁴'
        | '5' -> '⁵'
        | '6' -> '⁶'
        | '7' -> '⁷'
        | '8' -> '⁸'
        | '9' -> '⁹'
        | '-' -> '⁻'
        | '/' -> 'ᐟ'
        | c -> c)

let toString (a: Rat) =
    if a.Den = 1L then string a.Num else sprintf "%d/%d" a.Num a.Den

/// Rendered as an exponent, e.g. `⁻²` or `^(1/2)`.
let toExponentString (a: Rat) = superscript (toString a)

/// Parse "3", "-2", or "1/2". Returns None when the text is not a rational.
let tryParse (s: string) =
    let s = s.Trim()

    match s.Split('/') with
    | [| n |] ->
        match Int64.TryParse n with
        | true, v -> Some(create v 1L)
        | _ -> None
    | [| n; d |] ->
        match Int64.TryParse n, Int64.TryParse d with
        | (true, nv), (true, dv) when dv <> 0L -> Some(create nv dv)
        | _ -> None
    | _ -> None

/// Convert a float to a rational when it is one we can represent exactly
/// enough to use as a dimension exponent (integers and simple halves/thirds).
let tryOfFloat (x: float) =
    if Double.IsNaN x || Double.IsInfinity x then
        None
    else
        let candidates = [ 1L; 2L; 3L; 4L; 5L; 6L; 8L; 10L; 12L; 100L ]

        candidates
        |> List.tryPick (fun d ->
            let n = x * float d
            let r = Math.Round n

            if abs (n - r) < 1e-9 && abs r < 1e15 then
                Some(create (int64 r) d)
            else
                None)
