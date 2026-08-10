// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Rendering a solution as HTML.
///
/// This lives on the F# side on purpose. The browser build is a thin shell whose
/// only job is to hand a string of source in and put a string of markup on the
/// page, so there is exactly one implementation of "what a solution looks like"
/// and it is the same one the command line uses.
module Plimsoll.Present.Html

open System
open System.Text
open System.Globalization
open Plimsoll.Core
open Plimsoll.Core.Interval
open Plimsoll.Core.Types

let private inv = CultureInfo.InvariantCulture

let escape (s: string) =
    s
        .Replace("&", "&amp;")
        .Replace("<", "&lt;")
        .Replace(">", "&gt;")
        .Replace("\"", "&quot;")

let private unitLabel (v: VarInfo) =
    match v.Display with
    | Some u when u.Label <> "1" -> u.Label
    | _ -> ""

let private displayOf (v: VarInfo) (i: I) =
    match v.Display with
    | Some u -> Units.toDisplay u i
    | None -> i

/// Half-width over midpoint, as a percentage.
let private tolerancePct (i: I) =
    if isEmpty i || not (isBounded i) then None
    elif isPoint i then Some 0.0
    else
        let m = Math.Abs(mid i)
        if m = 0.0 then None else Some(100.0 * (width i / 2.0) / m)

let private toleranceText (i: I) =
    match tolerancePct i with
    | None -> if isBounded i then "" else "unbounded"
    | Some 0.0 -> "exact"
    | Some p when p >= 1000.0 -> "&gt;1000%"
    | Some p -> "±" + p.ToString((if p < 10.0 then "0.0" else "0"), inv) + "%"

/// Fraction of the bar to fill: full means pinned, empty means wide open.
let private precisionFraction (i: I) =
    match tolerancePct i with
    | None -> 0.0
    | Some p -> Math.Clamp(1.0 - p / 100.0, 0.0, 1.0)

// ------------------------------------------------------------- diagnostics --

let renderDiag (d: Diagnostics.Diag) =
    let sb = StringBuilder()

    let cls =
        match d.Severity with
        | Diagnostics.Severity.Error -> "diag error"
        | Diagnostics.Severity.Warning -> "diag warning"

    sb.Append("<div class=\"").Append(cls).Append("\">") |> ignore

    if d.Line > 0 then
        sb.Append("<span class=\"line\">line ").Append(d.Line).Append("</span> ") |> ignore

    sb.Append("<span class=\"msg\">").Append(escape d.Message).Append("</span>") |> ignore

    match d.Hint with
    | Some h -> sb.Append("<div class=\"hint\">").Append(escape h).Append("</div>") |> ignore
    | None -> ()

    sb.Append("</div>") |> ignore
    sb.ToString()

// -------------------------------------------------------------- the tables --

let private renderRow (v: VarInfo) (value: I) (declared: I option) (showBar: bool) =
    let sb = StringBuilder()
    sb.Append("<tr>") |> ignore
    sb.Append("<td class=\"name\">").Append(escape v.Name).Append("</td>") |> ignore

    sb
        .Append("<td class=\"value\">")
        .Append(escape (Report.formatInterval value))
        .Append("</td>")
    |> ignore

    sb.Append("<td class=\"unit\">").Append(escape (unitLabel v)).Append("</td>") |> ignore

    // The most informative cell in the whole report: an assumption whose range
    // the model itself has cut down.
    sb.Append("<td class=\"note\">") |> ignore

    match declared with
    | Some d when not (isEmpty value) && isBounded d && (d.Lo < value.Lo || value.Hi < d.Hi) ->
        sb
            .Append("<span class=\"narrowed\">narrowed from ")
            .Append(escape (Report.formatInterval d))
            .Append("</span>")
        |> ignore
    | _ -> ()

    sb.Append("</td>") |> ignore

    if showBar then
        let frac = precisionFraction value

        sb
            .Append("<td class=\"bar\"><div class=\"track\"><div class=\"fill\" style=\"width:")
            .Append((frac * 100.0).ToString("0.#", inv))
            .Append("%\"></div></div></td>")
        |> ignore

        sb.Append("<td class=\"tol\">").Append(toleranceText value).Append("</td>") |> ignore
    else
        sb.Append("<td class=\"bar\"></td><td class=\"tol\"></td>") |> ignore

    sb.Append("</tr>") |> ignore
    sb.ToString()

// ----------------------------------------------------------------- summary --

let renderSolution (m: CompiledModel) (s: Solver.Solution) =
    let sb = StringBuilder()

    // ---- status ----
    let statusClass, statusWord, statusBlurb =
        match s.Status with
        | Solver.Infeasible -> "status impossible", "Impossible", "no assignment satisfies this model"
        | Solver.Certified -> "status feasible", "Feasible", "a region was found in which every relation provably holds"
        | Solver.Consistent -> "status consistent", "Consistent", "no contradiction exists within these bounds"

    sb
        .Append("<div class=\"")
        .Append(statusClass)
        .Append("\"><strong>")
        .Append(statusWord)
        .Append("</strong><span>")
        .Append(statusBlurb)
        .Append("</span></div>")
    |> ignore

    // ---- warnings ----
    if not m.Warnings.IsEmpty then
        sb.Append("<div class=\"diags\">") |> ignore

        for w in m.Warnings do
            sb.Append(renderDiag w) |> ignore

        sb.Append("</div>") |> ignore

    let given = s.Vars |> Array.filter (fun r -> r.Var.Kind = GivenVar)
    let solved = s.Vars |> Array.filter (fun r -> r.Var.Kind <> GivenVar)

    let table (title: string) (rows: unit -> unit) =
        sb.Append("<h2>").Append(title).Append("</h2>") |> ignore
        sb.Append("<table class=\"quantities\">") |> ignore
        rows ()
        sb.Append("</table>") |> ignore

    if given.Length > 0 then
        table "Assumed" (fun () ->
            for r in given do
                sb.Append(renderRow r.Var r.Display (Some(displayOf r.Var r.Var.Declared)) false)
                |> ignore)

    if solved.Length > 0 then
        table "Implied" (fun () ->
            for r in solved do
                sb.Append(renderRow r.Var r.Display None (s.Status <> Solver.Infeasible))
                |> ignore)

    // ---- conflict ----
    match s.Conflict with
    | Some conflict ->
        sb.Append("<h2>The smallest set of assumptions that still conflicts</h2>") |> ignore
        sb.Append("<ul class=\"conflict\">") |> ignore

        for v in conflict.Givens do
            sb
                .Append("<li><span class=\"line\">line ")
                .Append(v.Line)
                .Append("</span> <code>given ")
                .Append(escape v.Name)
                .Append(" = ")
                .Append(escape (Report.formatInterval (displayOf v v.Declared)))
                .Append(" ")
                .Append(escape (unitLabel v))
                .Append("</code></li>")
            |> ignore

        for r in conflict.Relations do
            let text = if String.IsNullOrWhiteSpace r.Text then Ast.toString r.Lhs else r.Text

            sb
                .Append("<li><span class=\"line\">line ")
                .Append(r.Line)
                .Append("</span> <code>")
                .Append(escape text)
                .Append("</code></li>")
            |> ignore

        sb.Append("</ul>") |> ignore

        sb.Append(
            "<p class=\"aside\">Every one of these is load-bearing: drop any single one and the model becomes satisfiable again.</p>"
        )
        |> ignore
    | None -> ()

    // ---- sensitivity ----
    if not s.Sensitivities.IsEmpty then
        sb.Append("<h2>What to measure next</h2>") |> ignore
        sb.Append("<table class=\"sensitivity\">") |> ignore

        for x in s.Sensitivities |> List.truncate 6 do
            sb
                .Append("<tr><td class=\"name\">")
                .Append(escape x.Source.Name)
                .Append("</td><td class=\"bar\"><div class=\"track\"><div class=\"fill\" style=\"width:")
                .Append((x.Reduction * 100.0).ToString("0.#", inv))
                .Append("%\"></div></div></td><td class=\"tol\">")
                .Append((x.Reduction * 100.0).ToString("0", inv))
                .Append("%</td><td class=\"note\">of the uncertainty in <em>")
                .Append(escape x.BestTarget.Name)
                .Append("</em></td></tr>")
            |> ignore

        sb.Append("</table>") |> ignore

        sb.Append(
            "<p class=\"aside\">Each figure is the width removed from that quantity by collapsing the assumption to a single value.</p>"
        )
        |> ignore

    // ---- region ----
    match s.Region with
    | Some r ->
        sb.Append("<h2>Feasible region</h2>") |> ignore
        sb.Append("<div class=\"region\">").Append(Report.renderSvg r).Append("</div>") |> ignore
    | None -> ()

    sb
        .Append("<p class=\"stats\">")
        .Append(m.Vars.Length)
        .Append(" quantities · ")
        .Append(m.Constraints.Length)
        .Append(" relations · ")
        .Append(s.PropagationSteps)
        .Append(" constraint revisions · ")
        .Append(s.BoxesExamined)
        .Append(" sub-problems solved</p>")
    |> ignore

    sb.ToString()

/// Tuned for a browser, where this runs on every keystroke. The region grid is
/// by far the most expensive part -- it solves a sub-problem per cell -- so it
/// gets a coarser grid than the command line uses.
let browserDefaults =
    { Solver.defaults with
        RegionCells = 26
        Slices = 64 }

/// Compile, solve and render. The single entry point the browser shell calls.
let solveToHtml (src: string) : string =
    if String.IsNullOrWhiteSpace src then
        "<p class=\"aside\">Write a model, or load one of the examples above.</p>"
    else
        try
            match Model.compile src with
            | Result.Error d ->
                "<div class=\"diags\">" + renderDiag d + "</div>"
                + "<p class=\"aside\">Nothing is solved until the model reads cleanly.</p>"
            | Ok m ->
                let solution = Solver.solve m browserDefaults
                renderSolution m solution
        with e ->
            // The engine should never throw, but a browser page that goes blank
            // is worse than one that admits what happened.
            "<div class=\"diags\"><div class=\"diag error\"><span class=\"msg\">"
            + escape ("internal error: " + e.Message)
            + "</span></div></div>"
