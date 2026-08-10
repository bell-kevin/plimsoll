// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Dimensional inference.
///
/// This is not merely dimension *checking*. Undeclared quantities get their
/// dimensions solved for, so a model can say
///
///     given mass    = 1200 .. 1400 [kg]
///     given accel   = 2 .. 4 [m/s^2]
///     force = mass * accel
///
/// and Plimsoll works out on its own that `force` is measured in kg·m·s⁻².
///
/// How it works. The dimensions form a free abelian group: a dimension is a
/// vector of rational exponents over base names, multiplication adds those
/// vectors, and `^r` scales them. Every expression's dimension is therefore a
/// product of known dimensions times unknown variable dimensions raised to
/// rational powers. Sums force their addends to be equal, and relations force
/// their two sides to be equal, so each such site contributes one equation
///
///     Σ_v c_v · dim(v)  +  known  =  0
///
/// in exponent space. Crucially the coefficient matrix (c_v) is the *same* for
/// every base dimension -- only the `known` residual differs. So a single exact
/// rational Gaussian elimination solves for length, mass, time, money and
/// everything else at once. Inconsistent systems are dimension errors;
/// underdetermined ones leave free variables, which default to dimensionless
/// with a warning rather than a silent guess.
module Plimsoll.Core.Dimensional

open Plimsoll.Core.Rational
open Plimsoll.Core.Dimension
open Plimsoll.Core.Diagnostics
open Plimsoll.Core.Ast
open Plimsoll.Core.Types

/// The dimension of an expression: a known part times unknown variable
/// dimensions raised to rational exponents.
type private DimTerm =
    { Known: Dim
      Un: Map<int, Rat> }

let private tOne = { Known = one; Un = Map.empty }
let private tOfDim d = { Known = d; Un = Map.empty }

let private tOfVar i =
    { Known = one
      Un = Map.ofList [ i, Rational.one ] }

let private tMul a b =
    { Known = mul a.Known b.Known
      Un =
        b.Un
        |> Map.fold
            (fun acc k e ->
                let cur = Map.tryFind k acc |> Option.defaultValue Rational.zero
                let s = add cur e
                if isZero s then Map.remove k acc else Map.add k s acc)
            a.Un }

let private tPow a (r: Rat) =
    if isZero r then
        tOne
    else
        { Known = pow a.Known r
          Un = a.Un |> Map.map (fun _ e -> Rational.mul e r) }

let private tDiv a b = tMul a (tPow b (ofInt -1))

/// One equation of the system: Σ Coef[j]·dim(j) + Residual = 0.
type private Row =
    { Coef: Rat[]
      Residual: Dim
      /// Which source lines produced this equation, carried through row
      /// operations so an inconsistency can name the lines that caused it.
      Lines: Set<int> }

let private scaleRow (k: Rat) (r: Row) =
    { Coef = r.Coef |> Array.map (fun c -> Rational.mul c k)
      Residual = pow r.Residual k
      Lines = r.Lines }

let private subRow (a: Row) (b: Row) =
    { Coef = Array.map2 sub a.Coef b.Coef
      Residual = div a.Residual b.Residual
      Lines = Set.union a.Lines b.Lines }

/// Infer a dimension for every variable.
///
/// `declared` carries the dimension where the author gave a unit, `None` where
/// it must be inferred.
let infer
    (reg: Units.Registry)
    (names: string[])
    (declared: Dim option[])
    (rels: RelInfo list)
    : Result<Dim[] * Diag list, Diag> =
    try
        let nVars = names.Length
        let warnings = ResizeArray<Diag>()

        // Columns of the linear system: one per variable of unknown dimension.
        let unknownIdx =
            [| for i in 0 .. nVars - 1 do
                   if declared.[i].IsNone then yield i |]

        let colOf = unknownIdx |> Array.mapi (fun c v -> v, c) |> Map.ofArray
        let nCols = unknownIdx.Length
        let rows = ResizeArray<Row>()

        let addEquation (line: int) (a: DimTerm) (b: DimTerm) =
            let t = tDiv a b
            let coef = Array.create nCols Rational.zero

            t.Un
            |> Map.iter (fun v e ->
                match Map.tryFind v colOf with
                | Some c -> coef.[c] <- e
                | None -> failwithf "internal: variable %d has no column" v)

            rows.Add
                { Coef = coef
                  Residual = t.Known
                  Lines = Set.singleton line }

        let nameOf i = if i >= 0 && i < nVars then names.[i] else "?"

        // Walk an expression, returning its dimension term and emitting the
        // equations its structure implies.
        let rec termOf (line: int) (e: Expr) : DimTerm =
            match e with
            | Num _ -> tOne
            | Quantity(_, u) -> tOfDim (Units.evalUnit reg line u).Dim
            | Name n ->
                match names |> Array.tryFindIndex ((=) n) with
                | Some i ->
                    match declared.[i] with
                    | Some d -> tOfDim d
                    | None -> tOfVar i
                | None -> failwithf "internal: undeclared variable %s reached inference" n
            | Neg x -> termOf line x
            | Bin(Mul, a, b) -> tMul (termOf line a) (termOf line b)
            | Bin(Div, a, b) -> tDiv (termOf line a) (termOf line b)
            | Bin(Add, a, b)
            | Bin(Sub, a, b) ->
                // Addends must agree. This is where most real dimension bugs
                // are caught: adding a rate to a total, or USD to USD/month.
                let ta = termOf line a
                let tb = termOf line b
                addEquation line ta tb
                ta
            | Pow(x, r) -> tPow (termOf line x) r
            | Call(f, args) ->
                match Map.tryFind f functionArity with
                | None ->
                    failWith
                        line
                        (sprintf "unknown function '%s'" f)
                        ("available: " + (functionArity |> Map.toList |> List.map fst |> String.concat ", "))
                | Some arity when arity <> args.Length ->
                    fail line (sprintf "'%s' takes %d argument(s) but was given %d" f arity args.Length)
                | Some _ ->
                    match f, args with
                    | "sqrt", [ x ] -> tPow (termOf line x) (create 1L 2L)
                    | "abs", [ x ] -> termOf line x
                    | ("exp" | "log"), [ x ] ->
                        // Transcendentals only make sense on pure numbers.
                        addEquation line (termOf line x) tOne
                        tOne
                    | ("min" | "max"), [ x; y ] ->
                        let tx = termOf line x
                        addEquation line tx (termOf line y)
                        tx
                    | _ -> fail line (sprintf "cannot type '%s'" f)

        for r in rels do
            let lhs = termOf r.Line r.Lhs
            let rhs = termOf r.Line r.Rhs
            addEquation r.Line lhs rhs

        // ---- exact rational Gaussian elimination ----
        let m = rows.Count
        let arr = rows.ToArray()
        let pivots = ResizeArray<int * int>()
        let mutable pivotRow = 0

        for col in 0 .. nCols - 1 do
            let candidate =
                [ pivotRow .. m - 1 ] |> List.tryFind (fun i -> not (isZero arr.[i].Coef.[col]))

            match candidate with
            | Some r ->
                let tmp = arr.[pivotRow]
                arr.[pivotRow] <- arr.[r]
                arr.[r] <- tmp
                let pr = arr.[pivotRow]

                for i in pivotRow + 1 .. m - 1 do
                    if not (isZero arr.[i].Coef.[col]) then
                        let f = Rational.div arr.[i].Coef.[col] pr.Coef.[col]
                        arr.[i] <- subRow arr.[i] (scaleRow f pr)

                pivots.Add(pivotRow, col)
                pivotRow <- pivotRow + 1
            | None -> ()

        // Rows past the last pivot have no coefficients left. If such a row
        // still carries a dimension, the model contradicts itself.
        for i in pivotRow .. m - 1 do
            let r = arr.[i]

            if not (isOne r.Residual) then
                let lines = r.Lines |> Set.toList |> List.sort
                let where = lines |> List.map string |> String.concat ", "

                let msg =
                    sprintf "dimensional conflict: the relations on line(s) %s cannot all hold" where

                raise (
                    PlimError(
                        errorWith
                            (List.head lines)
                            msg
                            (sprintf
                                "they imply the dimensionless quantity 1 must equal %s"
                                (Dimension.format r.Residual))
                    )
                )

        // ---- back-substitution ----
        // Free columns keep the identity dimension, i.e. dimensionless.
        let solved = Array.create nCols one
        let isPivotCol = Array.create nCols false

        for (r, c) in pivots do
            isPivotCol.[c] <- true
            ignore r

        for (r, c) in Seq.rev (List.ofSeq pivots) do
            let row = arr.[r]
            let mutable acc = row.Residual

            for j in 0 .. nCols - 1 do
                if j <> c && not (isZero row.Coef.[j]) then
                    acc <- mul acc (pow solved.[j] row.Coef.[j])

            // Coef[c]·dim(c) = -acc   =>   dim(c) = (1/acc)^(1/Coef[c])
            solved.[c] <- pow (div one acc) (Rational.div Rational.one row.Coef.[c])

        for c in 0 .. nCols - 1 do
            if not isPivotCol.[c] then
                let v = unknownIdx.[c]

                warnings.Add(
                    warningWith
                        0
                        (sprintf "the dimension of '%s' is not determined by the model" (nameOf v))
                        "treating it as dimensionless; add a unit like `unknown x [kg]` to pin it down"
                )

        let result =
            Array.init nVars (fun i ->
                match declared.[i] with
                | Some d -> d
                | None -> solved.[Map.find i colOf])

        Ok(result, List.ofSeq warnings)
    with PlimError d ->
        Result.Error d
