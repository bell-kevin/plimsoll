// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// The solver: propagation to a fixpoint, branch-and-prune sharpening,
/// conflict explanation, and sensitivity ranking.
///
/// Four questions get answered here, and only the first is what a spreadsheet
/// can do:
///
///   1. What is each quantity's feasible envelope?  -- propagate, then pave.
///   2. Is the model even satisfiable?              -- an emptied domain proves not.
///   3. If not, which assumptions collide?          -- minimal conflict set.
///   4. Which assumption costs the most precision?  -- pin it, re-solve, compare.
module Plimsoll.Core.Solver

open System
open System.Collections.Generic
open Plimsoll.Core.Interval
open Plimsoll.Core.Types
open Plimsoll.Core.Contractor

type Options =
    { /// Cap on constraint revisions per propagation run.
      MaxPropagationSteps: int
      /// Hard cap on sub-problems solved while sharpening envelopes.
      SharpenBudget: int
      /// Slices per variable per sharpening round. Higher is tighter, and costs
      /// linearly rather than exponentially.
      Slices: int
      /// Sharpening rounds. Later rounds re-slice domains the earlier ones shrank.
      Rounds: int
      /// Relative width below which a variable is not worth slicing.
      Tolerance: float
      /// Resolution of the 2-D feasible-region grid, per axis.
      RegionCells: int
      /// Most variables to slice. Cost grows linearly with this.
      MaxBranchVars: int
      ComputeSensitivity: bool }

let defaults =
    { MaxPropagationSteps = 20000
      SharpenBudget = 20000
      Slices = 128
      Rounds = 3
      Tolerance = 1e-3
      RegionCells = 44
      MaxBranchVars = 8
      ComputeSensitivity = true }

type Status =
    /// Proven unsatisfiable: some quantity's domain was emptied.
    | Infeasible
    /// A box was found in which every relation holds throughout: a positive
    /// certificate, not just an absence of contradiction.
    | Certified
    /// No contradiction found at the search tolerance. Models containing
    /// equalities land here, since an equality's solution set has no volume for
    /// a box to sit inside.
    | Consistent

type VarResult =
    { Var: VarInfo
      /// Feasible envelope, SI base units.
      Envelope: I
      /// The same envelope in the unit the author wrote.
      Display: I }

type CellStatus =
    | Excluded
    | Possible
    | Guaranteed

type Region =
    { XVar: VarInfo
      YVar: VarInfo
      XRange: I
      YRange: I
      Cells: int
      /// Row-major, `Cells` * `Cells` entries.
      Grid: CellStatus[] }

type Conflict =
    { Relations: RelInfo list
      Givens: VarInfo list }

type Sensitivity =
    { Source: VarInfo
      BestTarget: VarInfo
      /// Fraction of `BestTarget`'s width removed by pinning `Source` to its
      /// midpoint. 1.0 means the target becomes exact.
      Reduction: float
      PerTarget: (int * float) list }

type Solution =
    { Status: Status
      Vars: VarResult[]
      Conflict: Conflict option
      Sensitivities: Sensitivity list
      Region: Region option
      PropagationSteps: int
      BoxesExamined: int }

// ------------------------------------------------------------- propagation --

/// Has `after` narrowed `before` enough to be worth re-examining neighbours?
/// Without a threshold, asymptotic convergence can revise constraints forever.
let private improved (before: I) (after: I) =
    if isEmpty after then true
    elif not (isBounded before) && isBounded after then true
    else
        let wb = width before
        let wa = width after
        wb - wa > 1e-9 * Math.Max(1.0, Math.Abs wb)

type private Workspace =
    { Scratch: I[]
      VarToCons: int[][]
      InQueue: bool[]
      Queue: Queue<int> }

let private makeWorkspace (m: CompiledModel) =
    let scratchSize =
        if m.Constraints.Length = 0 then
            1
        else
            m.Constraints |> Array.map (fun c -> c.Tape.Length) |> Array.max

    let buckets = Array.init m.Vars.Length (fun _ -> ResizeArray<int>())

    m.Constraints
    |> Array.iteri (fun ci c -> c.Vars |> Array.iter (fun v -> buckets.[v].Add ci))

    { Scratch = Array.zeroCreate scratchSize
      VarToCons = buckets |> Array.map (fun b -> b.ToArray())
      InQueue = Array.create m.Constraints.Length false
      Queue = Queue<int>() }

/// Contract `dom` against `active` constraints until nothing changes.
/// Returns false as soon as any domain is emptied.
let private propagate (m: CompiledModel) (ws: Workspace) (active: bool[]) (dom: I[]) (maxSteps: int) =
    let q = ws.Queue
    q.Clear()
    Array.fill ws.InQueue 0 ws.InQueue.Length false

    for ci in 0 .. m.Constraints.Length - 1 do
        if active.[ci] then
            ws.InQueue.[ci] <- true
            q.Enqueue ci

    let mutable steps = 0
    let mutable feasible = true

    while feasible && q.Count > 0 && steps < maxSteps do
        let ci = q.Dequeue()
        ws.InQueue.[ci] <- false
        steps <- steps + 1
        let c = m.Constraints.[ci]
        let before = c.Vars |> Array.map (fun v -> dom.[v])

        if not (contract ws.Scratch c dom) then
            feasible <- false
        else
            for k in 0 .. c.Vars.Length - 1 do
                let v = c.Vars.[k]

                if improved before.[k] dom.[v] then
                    for cj in ws.VarToCons.[v] do
                        if active.[cj] && not ws.InQueue.[cj] then
                            ws.InQueue.[cj] <- true
                            q.Enqueue cj

    feasible, steps

let private initialDomains (m: CompiledModel) =
    m.Vars
    |> Array.map (fun v ->
        match v.Kind with
        | GivenVar -> v.Declared
        | _ -> v.Declared) // `entire` unless the author declared bounds

// -------------------------------------------------------------- sharpening --

/// Constructive interval disjunction.
///
/// Propagation treats each relation separately, so it cannot see that two
/// quantities trade off against each other. With `x + y = 4` and both in
/// [1, 3], propagation reports `x*y ∈ [1, 9]`, while the truth is [3, 4] --
/// the extremes of x and y are not simultaneously reachable.
///
/// The fix is to slice one variable's domain into k pieces, solve each piece
/// separately, discard the refuted ones, and hull what survives. A slice pins
/// x tightly enough that propagation can then pin y, so the correlation becomes
/// visible. Repeating over each variable in turn, for a few rounds, tightens
/// everything at a cost of k · vars · rounds sub-problems.
///
/// This replaces branch-and-prune bisection deliberately: bisection is
/// exponential in the number of independent variables, and for a model whose
/// inputs are independent it spends that budget learning nothing. Slicing is
/// linear and, per unit of work, tighter.
///
/// Only refuted slices are ever dropped, so the result remains a guaranteed
/// enclosure.
///
/// Reference: Trombettoni & Chabert, "Constructive interval disjunction"
/// (CP 2007).
let private sharpen
    (m: CompiledModel)
    (ws: Workspace)
    (active: bool[])
    (dom: I[])
    (branchVars: int[])
    (opts: Options)
    =
    let cur = Array.copy dom
    let mutable examined = 0
    let mutable certified = false

    /// A box in which every relation holds throughout is a positive certificate
    /// of feasibility, not merely an absence of contradiction.
    let checkCertain (b: I[]) =
        if not certified then
            let all =
                m.Constraints
                |> Array.mapi (fun i c -> i, c)
                |> Array.forall (fun (i, c) -> not active.[i] || isCertain ws.Scratch c b)

            if all then certified <- true

    checkCertain cur

    let mutable round = 0
    let mutable progressed = true

    while round < opts.Rounds && progressed && examined < opts.SharpenBudget do
        progressed <- false

        for v in branchVars do
            if
                isBounded cur.[v]
                && relativeWidth cur.[v] > opts.Tolerance
                && examined < opts.SharpenBudget
            then
                let k = opts.Slices
                let lo = cur.[v].Lo
                let sliceWidth = (cur.[v].Hi - lo) / float k
                let acc = Array.create m.Vars.Length empty
                let mutable anyFeasible = false

                for j in 0 .. k - 1 do
                    if examined < opts.SharpenBudget then
                        let b = Array.copy cur
                        b.[v] <- intersect cur.[v] (make (lo + sliceWidth * float j) (lo + sliceWidth * float (j + 1)))

                        if not (isEmpty b.[v]) then
                            examined <- examined + 1
                            let feasible, _ = propagate m ws active b opts.MaxPropagationSteps

                            if feasible then
                                anyFeasible <- true
                                checkCertain b

                                for i in 0 .. acc.Length - 1 do
                                    acc.[i] <- hull acc.[i] b.[i]

                // If every slice was refuted the model is infeasible, which the
                // initial propagation would already have caught; leave `cur`
                // alone rather than replacing it with the empty hull.
                if anyFeasible then
                    for i in 0 .. cur.Length - 1 do
                        let narrowed = intersect cur.[i] acc.[i]
                        if improved cur.[i] narrowed then progressed <- true
                        cur.[i] <- narrowed

        round <- round + 1

    cur, examined, certified

// ------------------------------------------------------- conflict explanation --

/// Find an irreducible subset of assumptions that is still contradictory.
///
/// Deletion filtering: try removing each assumption; if the remainder is still
/// infeasible the assumption was not needed, so drop it for good. What survives
/// is minimal -- every member is load-bearing -- which turns "your model is
/// impossible" into "these three lines are impossible *together*".
///
/// Dropping a `given`'s range only widens a domain, and dropping a relation only
/// removes a restriction, so infeasibility is monotone and the filter is sound.
let private explainConflict (m: CompiledModel) (ws: Workspace) (opts: Options) =
    let nc = m.Constraints.Length

    let givenIdx =
        m.Vars
        |> Array.filter (fun v -> v.Kind = GivenVar && isBounded v.Declared)
        |> Array.map (fun v -> v.Index)

    let activeCons = Array.create nc true
    let activeGiven = HashSet<int>(givenIdx)

    let stillInfeasible () =
        let dom =
            m.Vars
            |> Array.map (fun v -> if activeGiven.Contains v.Index then v.Declared else entire)

        let feasible, _ = propagate m ws activeCons dom opts.MaxPropagationSteps
        not feasible

    if not (stillInfeasible ()) then
        None
    else
        for ci in 0 .. nc - 1 do
            activeCons.[ci] <- false
            if not (stillInfeasible ()) then activeCons.[ci] <- true

        for gi in List.ofSeq activeGiven do
            activeGiven.Remove gi |> ignore
            if not (stillInfeasible ()) then activeGiven.Add gi |> ignore

        Some
            { Relations =
                [ for ci in 0 .. nc - 1 do
                      if activeCons.[ci] then yield m.Constraints.[ci].Rel ]
              Givens =
                m.Vars
                |> Array.filter (fun v -> activeGiven.Contains v.Index)
                |> Array.toList }

// ------------------------------------------------------------- sensitivity --

/// Which assumption is costing the most precision?
///
/// For each `given`, collapse it to its midpoint, re-solve, and see how much
/// narrower everything else becomes. This answers the question people actually
/// have about an estimate -- "what should I go and measure?" -- and it is only
/// answerable because the whole model can be re-run backwards cheaply.
let private sensitivities
    (m: CompiledModel)
    (ws: Workspace)
    (active: bool[])
    (dom0: I[])
    (baseline: I[])
    (opts: Options)
    =
    let targets =
        m.Vars
        |> Array.filter (fun v ->
            v.Kind <> GivenVar
            && isBounded baseline.[v.Index]
            && width baseline.[v.Index] > 0.0)
        |> Array.map (fun v -> v.Index)

    if targets.Length = 0 then
        []
    else
        m.Vars
        |> Array.filter (fun v -> v.Kind = GivenVar && isBounded v.Declared && width v.Declared > 0.0)
        |> Array.choose (fun g ->
            let dom = Array.copy dom0
            dom.[g.Index] <- point (mid dom0.[g.Index])
            let feasible, _ = propagate m ws active dom opts.MaxPropagationSteps

            if not feasible then
                None
            else
                let perTarget =
                    targets
                    |> Array.choose (fun t ->
                        let w0 = width baseline.[t]
                        let w1 = width dom.[t]

                        if w0 > 0.0 && not (Double.IsInfinity w0) && not (Double.IsNaN w1) then
                            Some(t, Math.Clamp(1.0 - w1 / w0, 0.0, 1.0))
                        else
                            None)
                    |> Array.toList

                match perTarget with
                | [] -> None
                | xs ->
                    let bestT, bestR = xs |> List.maxBy snd

                    if bestR <= 1e-6 then
                        None
                    else
                        Some
                            { Source = g
                              BestTarget = m.Vars.[bestT]
                              Reduction = bestR
                              PerTarget = xs })
        |> Array.sortByDescending (fun s -> s.Reduction)
        |> Array.toList

// ----------------------------------------------------------------- region --

/// Classify a uniform grid over two variables. Each cell is solved
/// independently, so a cell is `Excluded` only when the relations genuinely
/// cannot hold anywhere inside it.
let private regionGrid
    (m: CompiledModel)
    (ws: Workspace)
    (active: bool[])
    (dom0: I[])
    (xi: int)
    (yi: int)
    (xr: I)
    (yr: I)
    (opts: Options)
    =
    let n = opts.RegionCells
    let grid = Array.create (n * n) Excluded

    let slice (r: I) k =
        let w = (r.Hi - r.Lo) / float n
        make (r.Lo + w * float k) (r.Lo + w * float (k + 1))

    for iy in 0 .. n - 1 do
        for ix in 0 .. n - 1 do
            let dom = Array.copy dom0
            dom.[xi] <- intersect dom.[xi] (slice xr ix)
            dom.[yi] <- intersect dom.[yi] (slice yr iy)

            if not (isEmpty dom.[xi]) && not (isEmpty dom.[yi]) then
                let feasible, _ = propagate m ws active dom opts.MaxPropagationSteps

                if feasible then
                    let certain =
                        m.Constraints
                        |> Array.mapi (fun i c -> i, c)
                        |> Array.forall (fun (i, c) -> not active.[i] || isCertain ws.Scratch c dom)

                    grid.[iy * n + ix] <- if certain then Guaranteed else Possible

    { XVar = m.Vars.[xi]
      YVar = m.Vars.[yi]
      XRange = xr
      YRange = yr
      Cells = n
      Grid = grid }

// ------------------------------------------------------------------- solve --

let solve (m: CompiledModel) (opts: Options) : Solution =
    let ws = makeWorkspace m
    let active = Array.create m.Constraints.Length true
    let dom0 = initialDomains m
    let dom = Array.copy dom0
    let feasible, steps = propagate m ws active dom opts.MaxPropagationSteps

    let display (v: VarInfo) (i: I) =
        match v.Display with
        | Some u -> Units.toDisplay u i
        | None -> i

    if not feasible then
        { Status = Infeasible
          Vars =
            m.Vars
            |> Array.map (fun v ->
                { Var = v
                  Envelope = empty
                  Display = empty })
          Conflict = explainConflict m ws opts
          Sensitivities = []
          Region = None
          PropagationSteps = steps
          BoxesExamined = 0 }
    else
        // Slice the quantities the author actually chose, widest first. Derived
        // quantities are determined by the relations, so slicing them buys
        // nothing that slicing their inputs does not already buy.
        let branchVars =
            m.Vars
            |> Array.filter (fun v -> v.Kind <> DerivedVar && isBounded dom.[v.Index] && width dom.[v.Index] > 0.0)
            |> Array.sortByDescending (fun v -> relativeWidth dom.[v.Index])
            |> Array.truncate opts.MaxBranchVars
            |> Array.map (fun v -> v.Index)

        // Sharpening starts from the propagated box and only ever intersects,
        // so its result is an enclosure no matter how the budget runs out.
        let envelope, boxes, certified = sharpen m ws active dom branchVars opts

        // Sensitivity compares propagation against propagation. Measuring a
        // pinned propagation run against the *paved* baseline would conflate
        // "pinning this helped" with "paving helped", and could even report a
        // negative contribution.
        let sens =
            if opts.ComputeSensitivity then
                sensitivities m ws active dom0 dom opts
            else
                []

        let region =
            match m.Plots with
            | (xi, yi) :: _ when isBounded envelope.[xi] && isBounded envelope.[yi] ->
                Some(regionGrid m ws active dom0 xi yi envelope.[xi] envelope.[yi] opts)
            | _ -> None

        { Status = if certified then Certified else Consistent
          Vars =
            m.Vars
            |> Array.map (fun v ->
                { Var = v
                  Envelope = envelope.[v.Index]
                  Display = display v envelope.[v.Index] })
          Conflict = None
          Sensitivities = sens
          Region = region
          PropagationSteps = steps
          BoxesExamined = boxes }
