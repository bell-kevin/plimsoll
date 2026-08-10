// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// The test suite. No test framework: a handful of assertion helpers and a
/// non-zero exit code is all a CI job needs, and it keeps the dependency set
/// of the whole repository empty.
module Plimsoll.Tests.Program

open System
open Plimsoll.Core
open Plimsoll.Core.Interval
open Plimsoll.Core.Types

// ------------------------------------------------------------- the harness --

let mutable private passed = 0
let private failures = ResizeArray<string>()
let mutable private currentGroup = ""

let group name =
    currentGroup <- name
    printfn ""
    printfn "  %s" name

let private fail (name: string) (detail: string) =
    failures.Add(sprintf "%s / %s: %s" currentGroup name detail)
    printfn "    FAIL  %s" name
    printfn "          %s" detail

let private pass (name: string) =
    passed <- passed + 1
    printfn "    ok    %s" name

let check name cond =
    if cond then pass name else fail name "expected true"

let checkFalse name cond =
    if not cond then pass name else fail name "expected false"

/// Compare with a relative tolerance; exact equality is the wrong bar for
/// results that legitimately widen by an ULP.
let close name (expected: float) (actual: float) =
    let tol = 1e-9 * Math.Max(1.0, Math.Abs expected)

    if Math.Abs(expected - actual) <= tol then
        pass name
    else
        fail name (sprintf "expected %.17g, got %.17g" expected actual)

let exact name (expected: float) (actual: float) =
    if expected = actual then
        pass name
    else
        fail name (sprintf "expected exactly %.17g, got %.17g" expected actual)

let intervalClose name (elo: float) (ehi: float) (a: I) =
    let tolOf (x: float) = 1e-7 * Math.Max(1.0, Math.Abs x)

    if Math.Abs(a.Lo - elo) <= tolOf elo && Math.Abs(a.Hi - ehi) <= tolOf ehi then
        pass name
    else
        fail name (sprintf "expected [%g, %g], got %s" elo ehi (toString a))

let equalStr name (expected: string) (actual: string) =
    if expected = actual then
        pass name
    else
        fail name (sprintf "expected '%s', got '%s'" expected actual)

// ----------------------------------------------------------------- helpers --

let solveWith opts src =
    match Model.run src opts with
    | Ok(m, s) -> m, s
    | Result.Error d -> failwithf "unexpected compile error: %s" (Diagnostics.format d)

let solve src = solveWith Solver.defaults src

let envOf (m: CompiledModel) (s: Solver.Solution) name =
    match m.VarByName name with
    | Some v -> s.Vars.[v.Index].Envelope
    | None -> failwithf "no such quantity: %s" name

let dispOf (m: CompiledModel) (s: Solver.Solution) name =
    match m.VarByName name with
    | Some v -> s.Vars.[v.Index].Display
    | None -> failwithf "no such quantity: %s" name

let dimOf (m: CompiledModel) name =
    match m.VarByName name with
    | Some v -> Dimension.format v.Dim
    | None -> failwithf "no such quantity: %s" name

let compileError src =
    match Model.compile src with
    | Result.Error d -> d
    | Ok _ -> failwith "expected a compile error, but compilation succeeded"

// ------------------------------------------------------------------- tests --

let testRational () =
    group "Rational"
    let r = Rational.create 6L 4L
    check "normalises to lowest terms" (r.Num = 3L && r.Den = 2L)
    let neg = Rational.create 1L -2L
    check "keeps the denominator positive" (neg.Num = -1L && neg.Den = 2L)

    check
        "one half plus one half is exactly one"
        (Rational.add (Rational.create 1L 2L) (Rational.create 1L 2L) = Rational.one)

    check "thirds do not drift" (
        let third = Rational.create 1L 3L
        Rational.add (Rational.add third third) third = Rational.one
    )

    check "parses a fraction" (Rational.tryParse "1/2" = Some(Rational.create 1L 2L))
    check "rejects nonsense" (Rational.tryParse "cheese" = None)
    check "recovers a half from a float" (Rational.tryOfFloat 0.5 = Some(Rational.create 1L 2L))

let testDimension () =
    group "Dimension"
    let len = Dimension.ofBase Dimension.Base.length
    let time = Dimension.ofBase Dimension.Base.time
    let vel = Dimension.div len time
    equalStr "formats velocity" "m·s⁻¹" (Dimension.format vel)
    check "dimensionless is the identity" (Dimension.isOne (Dimension.div len len))
    equalStr "formats dimensionless as 1" "1" (Dimension.format Dimension.one)

    check
        "sqrt of an area is a length"
        (Dimension.equal (Dimension.pow (Dimension.pow len (Rational.ofInt 2)) (Rational.create 1L 2L)) len)

    check "zero exponents are pruned" (Dimension.equal (Dimension.mul vel time) len)

let testIntervalSoundness () =
    group "Interval arithmetic"
    exact "2 + 2 stays exactly 4" 4.0 (add (point 2.0) (point 2.0)).Lo
    exact "2 + 2 has no upper slack" 4.0 (add (point 2.0) (point 2.0)).Hi
    exact "0.5 * 0.25 is exact" 0.125 (mul (point 0.5) (point 0.25)).Lo

    // A third is not representable, so the enclosure must straddle it.
    let third = div (point 1.0) (point 3.0)
    check "1/3 is enclosed, not asserted" (third.Lo < 1.0 / 3.0 || third.Hi > 1.0 / 3.0)
    check "1/3 encloses the true value" (third.Lo <= 1.0 / 3.0 && 1.0 / 3.0 <= third.Hi)
    check "1/3 is tight" (relativeWidth third < 1e-15)

    // 0.1 + 0.2 <> 0.3 in binary; the interval must still contain the real sum.
    let s = add (point 0.1) (point 0.2)
    check "0.1 + 0.2 encloses 0.3" (s.Lo <= 0.3 && 0.3 <= s.Hi)

    intervalClose "products take endpoint extrema" -6.0 8.0 (mul (make -2.0 4.0) (make -1.5 2.0))
    intervalClose "even powers are non-negative" 0.0 16.0 (powInt (make -4.0 2.0) 2)
    intervalClose "odd powers keep sign" -8.0 27.0 (powInt (make -2.0 3.0) 3)
    intervalClose "sqrt clamps to the real branch" 0.0 2.0 (Interval.sqrt (make -4.0 4.0))
    check "sqrt of a negative interval is empty" (isEmpty (Interval.sqrt (make -9.0 -4.0)))
    intervalClose "abs folds the negative side" 0.0 5.0 (Interval.abs (make -5.0 3.0))
    check "log of zero is unbounded below" (Double.IsNegativeInfinity (Interval.log (make 0.0 1.0)).Lo)
    check "exp never goes negative" ((Interval.exp entire).Lo >= 0.0)

    group "Extended division"
    let pieces = divList (point 1.0) (make -1.0 1.0)
    check "dividing by a zero-straddling interval splits in two" (List.length pieces = 2)

    check
        "the split pieces are the two unbounded rays"
        (pieces
         |> List.exists (fun p -> Double.IsNegativeInfinity p.Lo && p.Hi <= -1.0)
         && pieces |> List.exists (fun p -> p.Lo >= 1.0 && Double.IsPositiveInfinity p.Hi))

    check "0/0 carries no information" (isEntire (div (point 0.0) (point 0.0)))
    check "1/0 is empty" (isEmpty (div (point 1.0) (point 0.0)))
    intervalClose "ordinary division is unaffected" 2.0 5.0 (div (make 4.0 10.0) (point 2.0))

    group "Interval set algebra"
    check "empty is detected" (isEmpty (make 1.0 -1.0))
    check "intersection of disjoint is empty" (isEmpty (intersect (make 0.0 1.0) (make 2.0 3.0)))
    intervalClose "hull spans the gap" 0.0 3.0 (hull (make 0.0 1.0) (make 2.0 3.0))
    check "hull with empty is identity" (hull (make 1.0 2.0) empty = make 1.0 2.0)
    check "subset holds" (isSubsetOf (make 0.0 10.0) (make 1.0 2.0))
    checkFalse "subset fails when it should" (isSubsetOf (make 1.0 2.0) (make 0.0 10.0))

let testUnits () =
    group "Units"
    let reg = Units.baseRegistry

    let factorOf name =
        match Units.tryLookup reg name with
        | Some u -> u.Factor
        | None -> failwithf "unit not found: %s" name

    close "a minute is sixty seconds" 60.0 (factorOf "min")
    close "exact names beat prefix decomposition" 60.0 (factorOf "min")
    close "a kilogram is the base unit of mass" 1.0 (factorOf "kg")
    close "a gram is a thousandth" 1e-3 (factorOf "g")
    close "a millimetre resolves via prefix" 1e-3 (factorOf "mm")
    close "a kilowatt resolves via prefix" 1e3 (factorOf "kW")
    close "a kibibyte is binary" (1024.0 * 8.0) (factorOf "KiB")
    close "percent is a hundredth" 0.01 (factorOf "%")
    check "an unknown unit is not invented" ((Units.tryLookup reg "flurbs").IsNone)

    check
        "a named unit is preferred for output"
        (match Units.bestNamedUnit reg (Units.tryLookup reg "W").Value.Dim with
         | Some u -> u.Label = "W"
         | None -> false)

    check
        "dimensionless does not borrow radians"
        ((Units.bestNamedUnit reg Dimension.one).IsNone)

let testParser () =
    group "Parser"

    let m, s =
        solve
            """
            # a comment
            given wide = 1_000 .. 2e3        // digit grouping and exponents
            given tall = 30 [%]
            area = wide * tall
            """

    intervalClose "underscores and exponents parse alike" 1000.0 2000.0 (envOf m s "wide")
    intervalClose "percent is stored as a ratio" 0.3 0.3 (envOf m s "tall")
    intervalClose "percent displays as written" 30.0 30.0 (dispOf m s "tall")
    intervalClose "the product follows" 300.0 600.0 (envOf m s "area")

    // A line starting with an operator begins a new statement. Without the
    // rule, the declaration below would read as `1 .. 2 - 1` and `a` would come
    // out as [1, 1] instead of [1, 2].
    let mNl, sNl =
        solve
            """
            given a = 1 .. 2
            -1 = 0 - 1
            """

    intervalClose "a leading operator starts a new statement" 1.0 2.0 (envOf mNl sNl "a")
    check "and the new statement is parsed on its own terms" (sNl.Status <> Solver.Infeasible)

    let m2, s2 =
        solve
            """
            given a = 1 .. 2
            b = a +
                10
            """

    intervalClose "a trailing operator does continue the line" 11.0 12.0 (envOf m2 s2 "b")

    let dRange = compileError "given a = 5 .. 1"
    check "an inverted range is rejected" (dRange.Message.Contains "empty")

    let dConst = compileError "given a = 1 .. 2\ngiven b = a .. 3"
    check "ranges must be constant" (dConst.Message.Contains "cannot appear in a declared range")

    let dDup = compileError "given a = 1 .. 2\ngiven a = 3 .. 4"
    check "duplicate declarations are rejected" (dDup.Message.Contains "more than once")

let testDimensionalInference () =
    group "Dimensional inference"

    let m, s =
        solve
            """
            given mass  = 1200 .. 1400 [kg]
            given accel = 2 .. 4 [m/s^2]
            force = mass * accel
            """

    equalStr "an undeclared product gets its dimension" "kg·m·s⁻²" (dimOf m "force")
    intervalClose "and its value" 2400.0 5600.0 (envOf m s "force")
    equalStr "and a readable unit" "N" (m.VarByName "force").Value.Display.Value.Label

    // Solving a dimension backwards: `speed` is only pinned by its use.
    let m2, _ =
        solve
            """
            given dist = 100 [m]
            given time = 10 [s]
            dist = speed * time
            """

    equalStr "a dimension can be inferred backwards" "m·s⁻¹" (dimOf m2 "speed")

    // Fractional exponents.
    let m3, _ =
        solve
            """
            given area = 16 [m^2]
            side = sqrt(area)
            """

    equalStr "sqrt halves the exponents" "m" (dimOf m3 "side")

    let d1 = compileError "given a = 1 [kg]\ngiven b = 1 [s]\nc = a + b"
    check "adding unlike dimensions is an error" (d1.Message.Contains "dimensional conflict")

    let d2 =
        compileError
            """
            unit USD
            given rev  = 100 [USD]
            given rate = 5 [USD/month]
            total = rev + rate
            """

    check "a total cannot absorb a rate" (d2.Message.Contains "dimensional conflict")

    let d3 = compileError "given x = 2 [kg]\ny = exp(x)"
    check "exp requires a pure number" (d3.Message.Contains "dimensional conflict")

    let d4 = compileError "given x = 2 [kg]\ngiven y = 3 [m]\nx = y"
    check "a relation must be dimensionally sound" (d4.Message.Contains "dimensional conflict")

let testForwardAndBackward () =
    group "Solving forwards"

    let m, s =
        solve
            """
            given a = 2 .. 4
            b = a * 3
            """

    intervalClose "products propagate forwards" 6.0 12.0 (envOf m s "b")

    group "Solving backwards"

    // The headline capability: constrain the output, read the input.
    let m2, s2 =
        solve
            """
            unknown a
            b = a * 3
            b = 12
            """

    intervalClose "an input is recovered from an output" 4.0 4.0 (envOf m2 s2 "a")

    // Backwards through a division, with the divisor itself a range.
    let m3, s3 =
        solve
            """
            given revenue = 100 .. 200
            margin = profit / revenue
            margin >= 0.5
            """

    check "a lower bound flows back through division" ((envOf m3 s3 "profit").Lo >= 50.0 - 1e-9)

    // Both directions at once: the target narrows the inputs *and* the inputs
    // narrow the target.
    let m4, s4 =
        solve
            """
            given x = 0 .. 100
            given y = 0 .. 100
            x + y = 50
            x >= 20
            """

    check "a sum constraint bounds each addend" ((envOf m4 s4 "y").Hi <= 30.0 + 1e-9)
    check "and the other one too" ((envOf m4 s4 "x").Lo >= 20.0 - 1e-9)

    group "Sharpening by interval disjunction"

    // Propagation alone cannot see that x and y trade off, so the hull of
    // t = x*y over the *feasible* set is narrower than interval arithmetic on
    // the declared boxes. Slicing recovers most of that.
    let m5, s5 =
        solve
            """
            given x = 1 .. 3
            given y = 1 .. 3
            x + y = 4
            t = x * y
            """

    // Interval arithmetic on the declared boxes alone would give [1, 9]. The
    // true range under x + y = 4 is [3, 4]; paving must get close to it, with
    // slack on the order of the branch tolerance.
    let t = envOf m5 s5 "t"
    check "the product's upper bound respects the tradeoff" (t.Hi <= 4.05)
    check "the lower bound is not over-tightened" (t.Lo <= 3.0)
    check "the enclosure still covers the true maximum" (t.Hi >= 4.0 - 0.05)
    check "and it beats plain interval arithmetic" (t.Hi < 8.0)

let testInfeasibility () =
    group "Infeasibility"

    let _, s =
        solve
            """
            given x = 1 .. 2
            x >= 5
            """

    check "a contradiction is detected" (s.Status = Solver.Infeasible)
    check "and explained" s.Conflict.IsSome

    match s.Conflict with
    | Some c ->
        check "the conflicting relation is named" (c.Relations |> List.exists (fun r -> r.Text.Contains "x >= 5"))
        check "the conflicting assumption is named" (c.Givens |> List.exists (fun v -> v.Name = "x"))
    | None -> ()

    // Minimality: the irrelevant assumption must be filtered out of the report.
    let _, s2 =
        solve
            """
            given a = 1 .. 2
            given irrelevant = 100 .. 200
            a >= 10
            """

    check "an infeasible model with a spectator is still infeasible" (s2.Status = Solver.Infeasible)

    match s2.Conflict with
    | Some c ->
        check "the conflict set is minimal" (c.Givens |> List.forall (fun v -> v.Name <> "irrelevant"))
        check "the conflict set keeps what matters" (c.Givens |> List.exists (fun v -> v.Name = "a"))
        check "and only the relations that matter" (List.length c.Relations = 1)
    | None -> ()

    // The best case is price 20 against cost 12, so profit cannot reach 9.
    let _, s3 =
        solve
            """
            given price = 10 .. 20
            given cost  = 12 .. 15
            profit = price - cost
            profit >= 9
            """

    check "a conflict across a chain of relations is found" (s3.Status = Solver.Infeasible)

    match s3.Conflict with
    | Some c ->
        check "the chain's defining relation is retained" (List.length c.Relations = 2)
        check "and both assumptions are implicated" (List.length c.Givens = 2)
    | None -> check "a conflict is reported for the chain" false

    // A profit target of 8 is exactly attainable, so the same model must not be
    // reported as impossible.
    let _, s4 =
        solve
            """
            given price = 10 .. 20
            given cost  = 12 .. 15
            profit = price - cost
            profit >= 8
            """

    check "an attainable target is not called impossible" (s4.Status <> Solver.Infeasible)

let testCertification () =
    group "Certification"

    // Inequalities alone can be certified: a whole box satisfies them.
    let _, s =
        solve
            """
            given x = 1 .. 2
            x >= 0
            """

    check "an inequality-only model is certified feasible" (s.Status = Solver.Certified)

    // A relation linking two ranges cannot be certified: the box contains
    // points that violate it, even though a solution exists inside.
    let _, s2 =
        solve
            """
            given x = 1 .. 2
            y = x * 2
            """

    check "a relation across a range is consistent, not certified" (s2.Status = Solver.Consistent)

    // But when propagation pins every quantity exactly, the resulting degenerate
    // box really is a certificate, and saying so is not a lie.
    let _, s3 =
        solve
            """
            given x = 1 .. 2
            y = x * 2
            y = 3
            """

    check "an exactly pinned solution is certified" (s3.Status = Solver.Certified)

let testSensitivity () =
    group "Sensitivity"

    // `wide` carries almost all the uncertainty in the product, so pinning it
    // must rank above pinning `narrow`.
    let _, s =
        solve
            """
            given wide   = 1 .. 100
            given narrow = 10 .. 10.01
            product = wide * narrow
            """

    check "something is ranked" (not s.Sensitivities.IsEmpty)

    match s.Sensitivities with
    | top :: _ -> equalStr "the dominant assumption is identified" "wide" top.Source.Name
    | [] -> ()

    check
        "the dominant assumption explains most of the width"
        (match s.Sensitivities with
         | top :: _ -> top.Reduction > 0.9
         | [] -> false)

let testRegion () =
    group "Feasible region"

    let _, s =
        solve
            """
            given x = 0 .. 10
            given y = 0 .. 10
            x + y <= 10
            plot x, y
            """

    check "a region is produced" s.Region.IsSome

    match s.Region with
    | Some r ->
        let excluded = r.Grid |> Array.filter ((=) Solver.Excluded) |> Array.length
        let allowed = r.Grid |> Array.filter ((<>) Solver.Excluded) |> Array.length
        check "part of the box is ruled out" (excluded > 0)
        check "and part of it survives" (allowed > 0)
        // The constraint is a half-plane through the box's diagonal, so roughly
        // half the cells should survive.
        let frac = float allowed / float r.Grid.Length
        check "the surviving fraction is about half" (frac > 0.4 && frac < 0.7)
    | None -> ()

/// Randomised soundness check: the reported envelope must contain every value
/// the model can actually take. This is the guarantee that distinguishes a
/// sound solver from a sampling one, so it is worth testing by sampling.
let testSoundnessProperty () =
    group "Soundness (randomised)"
    let rng = Random(20260810)
    let mutable violations = 0
    let mutable trials = 0

    for _ in 1 .. 60 do
        let aLo = rng.NextDouble() * 10.0 + 0.5
        let aHi = aLo + rng.NextDouble() * 10.0
        let bLo = rng.NextDouble() * 10.0 + 0.5
        let bHi = bLo + rng.NextDouble() * 10.0

        let src =
            sprintf
                """
                given a = %.10f .. %.10f
                given b = %.10f .. %.10f
                y = (a * b + a) / (b + 3)
                z = sqrt(a) * b^2 - a / b
                """
                aLo
                aHi
                bLo
                bHi

        let m, s = solve src
        let envY = envOf m s "y"
        let envZ = envOf m s "z"

        for _ in 1 .. 40 do
            let a = aLo + rng.NextDouble() * (aHi - aLo)
            let b = bLo + rng.NextDouble() * (bHi - bLo)
            let y = (a * b + a) / (b + 3.0)
            let z = Math.Sqrt a * (b ** 2.0) - a / b
            trials <- trials + 2

            if not (contains y envY) then violations <- violations + 1
            if not (contains z envZ) then violations <- violations + 1

    check (sprintf "every one of %d sampled values lies inside its envelope" trials) (violations = 0)

    if violations > 0 then
        printfn "          %d violations" violations

/// The browser build is a shell around these two functions, so they are what
/// needs covering; there is no separate web logic to test.
let testPresentation () =
    group "Presentation"

    // The ceiling binds: revenue can reach 12000, so price cannot reach 60 and
    // volume cannot reach 200. Both assumptions must come back narrowed.
    let html =
        Plimsoll.Present.Html.solveToHtml
            """
            dimension money
            unit USD = money
            given price  = 40 .. 60 [USD]
            given volume = 100 .. 200
            revenue = price * volume
            revenue <= 5000 [USD]
            """

    check "renders a status" (html.Contains "status")
    check "renders the quantities table" (html.Contains "quantities")
    check "names the quantities" (html.Contains "revenue" && html.Contains "price")
    check "shows a unit" (html.Contains "USD")

    // The headline visual: an assumption the model itself cut down.
    check "flags an assumption that was narrowed" (html.Contains "narrowed from")

    let bad = Plimsoll.Present.Html.solveToHtml "given a = 1 .. 2\nb = a +"
    check "a broken model renders its diagnostic" (bad.Contains "diag error")
    checkFalse "and does not pretend to have solved anything" (bad.Contains "quantities")

    let blank = Plimsoll.Present.Html.solveToHtml "   "
    checkFalse "an empty model does not error" (blank.Contains "diag error")

    equalStr
        "markup in a model is escaped, not injected"
        "&lt;script&gt;alert(1)&lt;/script&gt;"
        (Plimsoll.Present.Html.escape "<script>alert(1)</script>")

    // Diagnostics quote characters from the source, so the escaping path is
    // what stops a model from injecting markup into the page that renders it.
    let hostile = Plimsoll.Present.Html.solveToHtml "given a = 1 .. 2\na = <"
    check "a diagnostic escapes the character it quotes" (hostile.Contains "&lt;")

    let jsonModel, jsonSol =
        solve
            """
            given x = 1 .. 2
            y = x * 2
            """

    let json = Plimsoll.Present.Report.renderJson jsonModel jsonSol
    check "JSON names the status" (json.Contains "\"status\"")
    check "JSON lists quantities" (json.Contains "\"quantities\"")
    check "JSON reports dimensions" (json.Contains "\"dimension\"")

    // Infinity is not a JSON number; unbounded must serialise as null.
    let unboundedModel, unboundedSol = solve "unknown q [kg]\nq >= 0 [kg]"
    let json2 = Plimsoll.Present.Report.renderJson unboundedModel unboundedSol
    check "unbounded serialises as null, not a magic number" (json2.Contains "\"hi\": null")
    checkFalse "and never emits a bare Infinity" (json2.Contains "Infinity")

/// The engine must not wedge on a model that cannot be narrowed.
let testTermination () =
    group "Termination"

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let _, s =
        solve
            """
            given a = 1 .. 2
            given b = 1 .. 2
            given c = 1 .. 2
            given d = 1 .. 2
            w = a * b + c * d
            v = w / (a + b)
            u = sqrt(v * v + w)
            """

    sw.Stop()
    check "an unconstrained model still terminates" (s.Status <> Solver.Infeasible)
    check (sprintf "and does so promptly (%d ms)" sw.ElapsedMilliseconds) (sw.ElapsedMilliseconds < 20000L)

    // Cyclic relations must reach a fixpoint rather than oscillate.
    let sw2 = System.Diagnostics.Stopwatch.StartNew()

    let _, s2 =
        solve
            """
            given k = 0.5 .. 0.6
            x = k * y + 1
            y = k * x + 1
            """

    sw2.Stop()
    check "a cyclic model reaches a fixpoint" (s2.Status <> Solver.Infeasible)
    check (sprintf "without spinning (%d ms)" sw2.ElapsedMilliseconds) (sw2.ElapsedMilliseconds < 20000L)

[<EntryPoint>]
let main _ =
    Console.OutputEncoding <- Text.Encoding.UTF8
    printfn "Plimsoll test suite"

    testRational ()
    testDimension ()
    testIntervalSoundness ()
    testUnits ()
    testParser ()
    testDimensionalInference ()
    testForwardAndBackward ()
    testInfeasibility ()
    testCertification ()
    testSensitivity ()
    testRegion ()
    testSoundnessProperty ()
    testPresentation ()
    testTermination ()

    printfn ""
    printfn "%s" (String('-', 60))

    if failures.Count = 0 then
        printfn "  %d passed, 0 failed" passed
        0
    else
        printfn "  %d passed, %d FAILED" passed failures.Count
        printfn ""

        for f in failures do
            printfn "  - %s" f

        1
