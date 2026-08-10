// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Compilation: source text to a solvable model, and the public entry points.
module Plimsoll.Core.Model

open Plimsoll.Core.Dimension
open Plimsoll.Core.Interval
open Plimsoll.Core.Diagnostics
open Plimsoll.Core.Ast
open Plimsoll.Core.Types

/// A declaration collected during the first pass, before inference has run.
type private PreVar =
    { Name: string
      Kind: VarKind
      Lo: float
      Hi: float
      Display: Units.U option
      DeclaredDim: Dim option
      Line: int }

/// Evaluate a constant expression, in SI base units.
///
/// Range bounds must be constants: `given x = 40 .. 60 [USD]` is a declaration,
/// not a relation, so it may not depend on other quantities. Anything that does
/// belongs on the right-hand side of a relation instead.
let rec private constEval (reg: Units.Registry) (line: int) (e: Expr) : float * Dim * Units.U option =
    let requireSame op (da: Dim) (db: Dim) =
        if not (equal da db) then
            failWith
                line
                (sprintf "cannot %s %s and %s" op (Dimension.format da) (Dimension.format db))
                "both sides of a sum must have the same dimension"

    match e with
    | Num v -> v, one, None
    | Quantity(v, u) ->
        let uu = Units.evalUnit reg line u
        v * uu.Factor, uu.Dim, Some uu
    | Neg x ->
        let v, d, u = constEval reg line x
        -v, d, u
    | Bin(Add, a, b) ->
        let va, da, ua = constEval reg line a
        let vb, db, ub = constEval reg line b
        requireSame "add" da db
        va + vb, da, (match ua with Some _ -> ua | None -> ub)
    | Bin(Sub, a, b) ->
        let va, da, ua = constEval reg line a
        let vb, db, ub = constEval reg line b
        requireSame "subtract" da db
        va - vb, da, (match ua with Some _ -> ua | None -> ub)
    | Bin(Mul, a, b) ->
        // `Dimension.mul`/`div` must be qualified: the interval operations of
        // the same name are opened later and shadow them.
        let va, da, _ = constEval reg line a
        let vb, db, _ = constEval reg line b
        va * vb, Dimension.mul da db, None
    | Bin(Div, a, b) ->
        let va, da, _ = constEval reg line a
        let vb, db, _ = constEval reg line b
        va / vb, Dimension.div da db, None
    | Pow(x, r) ->
        let v, d, _ = constEval reg line x
        v ** Rational.toFloat r, pow d r, None
    | Call(f, args) ->
        let evaluated = args |> List.map (constEval reg line)

        match f, evaluated with
        | "sqrt", [ (v, d, _) ] -> System.Math.Sqrt v, pow d (Rational.create 1L 2L), None
        | "abs", [ (v, d, u) ] -> System.Math.Abs v, d, u
        | "exp", [ (v, d, _) ] ->
            requireSame "exponentiate" d one
            System.Math.Exp v, one, None
        | "log", [ (v, d, _) ] ->
            requireSame "take the logarithm of" d one
            System.Math.Log v, one, None
        | "min", [ (va, da, ua); (vb, db, _) ] ->
            requireSame "compare" da db
            System.Math.Min(va, vb), da, ua
        | "max", [ (va, da, ua); (vb, db, _) ] ->
            requireSame "compare" da db
            System.Math.Max(va, vb), da, ua
        | _, _ ->
            match Map.tryFind f functionArity with
            | Some n -> fail line (sprintf "'%s' takes %d argument(s) but was given %d" f n args.Length)
            | None -> fail line (sprintf "unknown function '%s'" f)
    | Name n ->
        failWith
            line
            (sprintf "'%s' cannot appear in a declared range" n)
            "ranges must be constant; to relate quantities, write a relation such as `budget = price * count`"

/// Every unit expression written anywhere in an expression.
let rec private unitExprsIn (e: Expr) : Expr list =
    match e with
    | Quantity(_, u) -> [ u ]
    | Neg x -> unitExprsIn x
    | Bin(_, a, b) -> unitExprsIn a @ unitExprsIn b
    | Pow(x, _) -> unitExprsIn x
    | Call(_, args) -> args |> List.collect unitExprsIn
    | Num _
    | Name _ -> []

let private unitExprsInStmt (st: Stmt) =
    let tag line = List.map (fun u -> line, u)

    match st with
    | Given(_, lo, hi, line) -> unitExprsIn lo @ unitExprsIn hi |> tag line
    | Unknown(_, Some ann, line) -> [ line, ann ]
    | Relate(_, l, r, line, _) -> unitExprsIn l @ unitExprsIn r |> tag line
    | UnitDecl(_, Some e, line) -> unitExprsIn e |> tag line
    | _ -> []

/// Turn source text into a solvable model.
let compile (src: string) : Result<CompiledModel, Diag> =
    match Parser.parse src with
    | Result.Error d -> Result.Error d
    | Ok(ast, parseWarnings) ->
        try
            let warnings = ResizeArray<Diag>(parseWarnings)
            let mutable reg = Units.baseRegistry
            let pre = ResizeArray<PreVar>()
            let rels = ResizeArray<RelInfo>()
            let plotNames = ResizeArray<string * string * int>()

            let indexOf (name: string) =
                pre |> Seq.tryFindIndex (fun v -> v.Name = name)

            // ---- pass 1: declarations, in source order ----
            for st in ast.Statements do
                match st with
                | DimensionDecl(name, line) ->
                    if reg.Dims.Contains name then
                        warnings.Add(warning line (sprintf "dimension '%s' is already declared" name))

                    reg <- Units.addDimension reg name

                | UnitDecl(name, defn, line) ->
                    match defn with
                    | Some e ->
                        let u = Units.evalUnit reg line e
                        reg <- Units.addUnit reg name u
                    | None ->
                        // `unit request` introduces both a fresh base dimension
                        // and its base unit, so counts of unlike things cannot be
                        // added together.
                        reg <- Units.addDimension reg name

                | Given(name, loE, hiE, line) ->
                    match indexOf name with
                    | Some _ -> fail line (sprintf "'%s' is declared more than once" name)
                    | None -> ()

                    let lo, dLo, uLo = constEval reg line loE
                    let hi, dHi, uHi = constEval reg line hiE

                    if not (equal dLo dHi) then
                        failWith
                            line
                            (sprintf
                                "the bounds of '%s' have different dimensions: %s and %s"
                                name
                                (Dimension.format dLo)
                                (Dimension.format dHi))
                            "write the unit once after the range, as in `given x = 40 .. 60 [USD]`"

                    if lo > hi then
                        failWith
                            line
                            (sprintf "the range of '%s' is empty: its lower bound exceeds its upper bound" name)
                            "write the smaller bound first"

                    pre.Add
                        { Name = name
                          Kind = GivenVar
                          Lo = lo
                          Hi = hi
                          Display = (match uLo with Some _ -> uLo | None -> uHi)
                          DeclaredDim = Some dLo
                          Line = line }

                | Unknown(name, ann, line) ->
                    match indexOf name with
                    | Some _ -> fail line (sprintf "'%s' is declared more than once" name)
                    | None -> ()

                    let u = ann |> Option.map (Units.evalUnit reg line)

                    pre.Add
                        { Name = name
                          Kind = UnknownVar
                          Lo = System.Double.NegativeInfinity
                          Hi = System.Double.PositiveInfinity
                          Display = u
                          DeclaredDim = u |> Option.map (fun x -> x.Dim)
                          Line = line }

                | Relate(op, lhs, rhs, line, text) ->
                    rels.Add
                        { Id = rels.Count
                          Op = op
                          Lhs = lhs
                          Rhs = rhs
                          Line = line
                          Text = text }

                | Plot(x, y, line) -> plotNames.Add(x, y, line)

            // ---- pass 2: quantities that appear only inside relations ----
            // Undeclared names become unknowns automatically, so intermediate
            // results need no ceremony: writing `revenue = price * volume`
            // brings `revenue` into being.
            for r in rels do
                for n in namesIn r.Lhs @ namesIn r.Rhs do
                    if (indexOf n).IsNone then
                        if (Units.tryLookup reg n).IsSome then
                            warnings.Add(
                                warningWith
                                    r.Line
                                    (sprintf "'%s' is being used as a quantity, but it is also a unit name" n)
                                    "units belong in brackets, as in `2 [kg]`; rename the quantity to avoid confusion"
                            )

                        pre.Add
                            { Name = n
                              Kind = DerivedVar
                              Lo = System.Double.NegativeInfinity
                              Hi = System.Double.PositiveInfinity
                              Display = None
                              DeclaredDim = None
                              Line = r.Line }

            let names = pre |> Seq.map (fun v -> v.Name) |> Array.ofSeq
            let declaredDims = pre |> Seq.map (fun v -> v.DeclaredDim) |> Array.ofSeq

            // ---- pass 3: dimensional inference ----
            match Dimensional.infer reg names declaredDims (List.ofSeq rels) with
            | Result.Error d -> Result.Error d
            | Ok(dims, inferWarnings) ->
                warnings.AddRange inferWarnings

                // Units the model itself speaks in. An inferred quantity is
                // reported in one of these when the dimensions match, so a model
                // written in USD/month does not get its answers back in money
                // per second. Every `unit` declaration has been processed by
                // now, so evaluating against the final registry is safe.
                let mentionedUnits =
                    ast.Statements
                    |> List.collect unitExprsInStmt
                    |> List.map (fun (line, ue) -> Units.evalUnit reg line ue)
                    |> List.distinctBy (fun u -> u.Label)
                    |> List.sortBy (fun u -> u.Label.Length, u.Label)

                // Dimensionless quantities are excluded: a model that mentions
                // `%` somewhere should not have every bare ratio rescaled by it.
                let displayForDim (d: Dimension.Dim) =
                    if isOne d then
                        Units.displayFor reg d
                    else
                        match mentionedUnits |> List.tryFind (fun u -> equal u.Dim d) with
                        | Some u -> u
                        | None -> Units.displayFor reg d

                let vars =
                    pre
                    |> Seq.mapi (fun i v ->
                        { Name = v.Name
                          Index = i
                          Kind = v.Kind
                          Declared = make v.Lo v.Hi
                          // An unannotated quantity still gets a readable unit.
                          Display =
                            match v.Display with
                            | Some u -> Some u
                            | None -> Some(displayForDim dims.[i])
                          Dim = dims.[i]
                          Line = v.Line })
                    |> Array.ofSeq

                let varIndex = names |> Array.mapi (fun i n -> n, i) |> Map.ofArray

                let constraints =
                    rels |> Seq.map (Contractor.buildConstraint reg varIndex) |> Array.ofSeq

                let plots =
                    [ for (x, y, line) in plotNames do
                          match Map.tryFind x varIndex, Map.tryFind y varIndex with
                          | Some xi, Some yi -> yield xi, yi
                          | None, _ -> fail line (sprintf "cannot plot unknown quantity '%s'" x)
                          | _, None -> fail line (sprintf "cannot plot unknown quantity '%s'" y) ]

                Ok
                    { Vars = vars
                      Constraints = constraints
                      Registry = reg
                      Plots = plots
                      Warnings = List.ofSeq warnings }
        with PlimError d ->
            Result.Error d

/// Compile and solve in one step.
let run (src: string) (opts: Solver.Options) : Result<CompiledModel * Solver.Solution, Diag> =
    match compile src with
    | Result.Error d -> Result.Error d
    | Ok m -> Ok(m, Solver.solve m opts)
