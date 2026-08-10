// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// HC4-revise: the contractor that makes relations run in every direction.
///
/// A spreadsheet formula is a one-way function: inputs on the right, answer on
/// the left. A Plimsoll relation is a *set* -- the points where it holds -- and
/// contracting means shrinking the current box to the smallest box the relation
/// still permits. That is what lets you constrain an output and watch the
/// inputs narrow.
///
/// One contraction is two walks over the tape:
///
///   forward   evaluate every node bottom-up from the current variable domains,
///             then intersect the root with the relation's target set;
///   backward  walk the tape in reverse, and at each node use the (now
///             narrowed) result to narrow its operands by inverting the
///             operation -- `z = x + y` gives `x ∈ z - y`, `z = x·y` gives
///             `x ∈ z / y`, and so on. Leaves write back into the domains.
///
/// The inverse of multiplication is where the extended division of
/// `Interval.divList` earns its keep: dividing by an interval that straddles
/// zero yields two disjoint pieces, and intersecting each piece separately with
/// the operand's current domain keeps information that hulling them would throw
/// away.
///
/// Reference: Benhamou, Goualard, Granvilliers & Puget, "Revising hull and box
/// consistency" (ICLP 1999).
module Plimsoll.Core.Contractor

open System.Collections.Generic
open Plimsoll.Core.Interval
open Plimsoll.Core.Diagnostics
open Plimsoll.Core.Ast
open Plimsoll.Core.Types

// ------------------------------------------------------------ tape building --

/// Compile a relation into `tape[root] ∈ target`.
let buildConstraint (reg: Units.Registry) (varIndex: Map<string, int>) (rel: RelInfo) : Constraint =
    let instrs = ResizeArray<Instr>()
    let touched = HashSet<int>()

    let emit (i: Instr) =
        instrs.Add i
        instrs.Count - 1

    let rec compile (e: Expr) : int =
        match e with
        | Num v -> emit (IConst(point v))
        | Quantity(v, u) ->
            // Constants are folded into SI base units at compile time, so the
            // solver never has to think about units again.
            let uu = Units.evalUnit reg rel.Line u
            emit (IConst(point (v * uu.Factor)))
        | Name n ->
            match Map.tryFind n varIndex with
            | Some i ->
                touched.Add i |> ignore
                emit (IVar i)
            | None -> fail rel.Line (sprintf "internal: unresolved variable '%s'" n)
        | Neg x -> emit (INeg(compile x))
        | Bin(Add, a, b) -> emit (IAdd(compile a, compile b))
        | Bin(Sub, a, b) -> emit (ISub(compile a, compile b))
        | Bin(Mul, a, b) -> emit (IMul(compile a, compile b))
        | Bin(Div, a, b) -> emit (IDiv(compile a, compile b))
        | Pow(x, r) -> emit (IPow(compile x, r))
        | Call(f, args) ->
            match f, args with
            | "sqrt", [ x ] -> emit (ISqrt(compile x))
            | "exp", [ x ] -> emit (IExp(compile x))
            | "log", [ x ] -> emit (ILog(compile x))
            | "abs", [ x ] -> emit (IAbs(compile x))
            | "min", [ x; y ] -> emit (IMin(compile x, compile y))
            | "max", [ x; y ] -> emit (IMax(compile x, compile y))
            | _ -> fail rel.Line (sprintf "internal: cannot compile '%s'" f)

    // Every relation is normalised to a membership test on `lhs - rhs`.
    let root = emit (ISub(compile rel.Lhs, compile rel.Rhs))

    let target =
        match rel.Op with
        | Eq -> point 0.0
        | Le -> make System.Double.NegativeInfinity 0.0
        | Ge -> nonNegative

    { Tape = instrs.ToArray()
      Root = root
      Target = target
      Vars = touched |> Seq.toArray |> Array.sort
      Rel = rel }

// -------------------------------------------------------------- projections --

/// Narrow `cur` to those x with x·y ∋ z, i.e. x ∈ z/y.
let private projectQuotient (z: I) (y: I) (cur: I) =
    divList z y |> List.map (intersect cur) |> hullOf

/// Narrow `cur` to those x with x^n ∋ z, for integer n.
let rec private projectPowInt (z: I) (n: int) (cur: I) =
    if n = 0 then cur
    elif n = 1 then intersect cur z
    elif n < 0 then
        // x^n = z  <=>  x^|n| = 1/z
        divList (point 1.0) z
        |> List.map (fun w -> projectPowInt w (-n) cur)
        |> hullOf
    elif n % 2 <> 0 then
        // Odd powers are invertible over the whole line.
        intersect cur (widen (make (signedNthRoot z.Lo n) (signedNthRoot z.Hi n)))
    else
        // Even powers lose the sign, so both roots are candidates.
        let zp = intersect z nonNegative

        if isEmpty zp then
            empty
        else
            let r = widen (make (signedNthRoot zp.Lo n) (signedNthRoot zp.Hi n))
            hull (intersect cur r) (intersect cur (neg r))

let private projectPow (z: I) (r: Rational.Rat) (cur: I) =
    if Rational.isInt r then
        projectPowInt z (Rational.toInt r) cur
    else
        // A non-integer exponent is never zero, and fractional powers are
        // defined on [0, inf) only, so inverting stays inside that branch.
        let e = Rational.toFloat r
        let zp = intersect z nonNegative

        if isEmpty zp then
            empty
        else
            let a = System.Math.Pow(zp.Lo, 1.0 / e)
            let b = System.Math.Pow(zp.Hi, 1.0 / e)
            intersect cur (widen (make (min a b) (max a b)))

/// Narrow `cur` to those x with |x| ∋ z.
let private projectAbs (z: I) (cur: I) =
    let zp = intersect z nonNegative
    hull (intersect cur zp) (intersect cur (neg zp))

// -------------------------------------------------------------- contraction --

/// Forward evaluation only: the interval the relation's residual can take on
/// the given box. Used to certify feasibility, not to narrow.
let evaluate (scratch: I[]) (c: Constraint) (dom: I[]) : I =
    let tape = c.Tape

    for k in 0 .. tape.Length - 1 do
        scratch.[k] <-
            match tape.[k] with
            | IConst v -> v
            | IVar i -> dom.[i]
            | INeg a -> neg scratch.[a]
            | IAdd(a, b) -> add scratch.[a] scratch.[b]
            | ISub(a, b) -> sub scratch.[a] scratch.[b]
            | IMul(a, b) -> mul scratch.[a] scratch.[b]
            | IDiv(a, b) -> div scratch.[a] scratch.[b]
            | IPow(a, r) -> powRat scratch.[a] r
            | ISqrt a -> sqrt scratch.[a]
            | IExp a -> exp scratch.[a]
            | ILog a -> log scratch.[a]
            | IAbs a -> abs scratch.[a]
            | IMin(a, b) -> minI scratch.[a] scratch.[b]
            | IMax(a, b) -> maxI scratch.[a] scratch.[b]

    scratch.[c.Root]

/// Contract `dom` with respect to a single constraint.
///
/// Returns false when the constraint is unsatisfiable on this box, in which
/// case `dom` must be treated as discarded (it may have been partly narrowed).
let contract (scratch: I[]) (c: Constraint) (dom: I[]) : bool =
    let tape = c.Tape
    let root = evaluate scratch c dom

    // The relation itself: residual must land in the target set.
    scratch.[c.Root] <- intersect root c.Target

    if isEmpty scratch.[c.Root] then
        false
    else
        let mutable feasible = true
        let mutable k = tape.Length - 1

        while feasible && k >= 0 do
            let z = scratch.[k]

            if isEmpty z then
                feasible <- false
            else
                match tape.[k] with
                | IConst v ->
                    // A constant cannot be narrowed; if the requirement placed
                    // on it excludes its value, this box is infeasible.
                    if isEmpty (intersect v z) then feasible <- false
                | IVar i ->
                    let narrowed = intersect dom.[i] z
                    dom.[i] <- narrowed
                    if isEmpty narrowed then feasible <- false
                | INeg a -> scratch.[a] <- intersect scratch.[a] (neg z)
                | IAdd(a, b) ->
                    scratch.[a] <- intersect scratch.[a] (sub z scratch.[b])
                    scratch.[b] <- intersect scratch.[b] (sub z scratch.[a])
                | ISub(a, b) ->
                    scratch.[a] <- intersect scratch.[a] (add z scratch.[b])
                    scratch.[b] <- intersect scratch.[b] (sub scratch.[a] z)
                | IMul(a, b) ->
                    scratch.[a] <- projectQuotient z scratch.[b] scratch.[a]
                    scratch.[b] <- projectQuotient z scratch.[a] scratch.[b]
                | IDiv(a, b) ->
                    // z = a/b  =>  a ∈ z·b  and  b ∈ a/z
                    scratch.[a] <- intersect scratch.[a] (mul z scratch.[b])
                    scratch.[b] <- projectQuotient scratch.[a] z scratch.[b]
                | IPow(a, r) -> scratch.[a] <- projectPow z r scratch.[a]
                | ISqrt a -> scratch.[a] <- intersect scratch.[a] (square (intersect z nonNegative))
                | IExp a -> scratch.[a] <- intersect scratch.[a] (log z)
                | ILog a -> scratch.[a] <- intersect scratch.[a] (exp z)
                | IAbs a -> scratch.[a] <- projectAbs z scratch.[a]
                | IMin(a, b) ->
                    // min(a,b) ∈ z forces both operands above z's floor, and if
                    // one is wholly above z's ceiling the other realises the min.
                    let floorI = make z.Lo System.Double.PositiveInfinity
                    scratch.[a] <- intersect scratch.[a] floorI
                    scratch.[b] <- intersect scratch.[b] floorI

                    if not (isEmpty scratch.[a]) && scratch.[a].Lo > z.Hi then
                        scratch.[b] <- intersect scratch.[b] z

                    if not (isEmpty scratch.[b]) && scratch.[b].Lo > z.Hi then
                        scratch.[a] <- intersect scratch.[a] z
                | IMax(a, b) ->
                    let ceilI = make System.Double.NegativeInfinity z.Hi
                    scratch.[a] <- intersect scratch.[a] ceilI
                    scratch.[b] <- intersect scratch.[b] ceilI

                    if not (isEmpty scratch.[a]) && scratch.[a].Hi < z.Lo then
                        scratch.[b] <- intersect scratch.[b] z

                    if not (isEmpty scratch.[b]) && scratch.[b].Hi < z.Lo then
                        scratch.[a] <- intersect scratch.[a] z

                k <- k - 1

        feasible

/// True when the relation holds for *every* point of the box. Such a box is a
/// certificate of feasibility rather than merely "not yet refuted".
let isCertain (scratch: I[]) (c: Constraint) (dom: I[]) : bool =
    let r = evaluate scratch c dom
    not (isEmpty r) && isSubsetOf c.Target r
