// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Rendering solutions as a terminal report, as JSON, and as SVG.
module Plimsoll.Present.Report

open System
open System.Text
open System.Globalization
open Plimsoll.Core
open Plimsoll.Core.Interval
open Plimsoll.Core.Types

let private inv = CultureInfo.InvariantCulture

// ------------------------------------------------------------------ colour --

/// Honours the NO_COLOR convention, and stays quiet when piped to a file.
let mutable useColour =
    isNull (Environment.GetEnvironmentVariable "NO_COLOR")
    && not Console.IsOutputRedirected

let private c (code: string) (s: string) =
    if useColour then "\u001b[" + code + "m" + s + "\u001b[0m" else s

let private dim s = c "2" s
let private bold s = c "1" s
let private red s = c "31" s
let private green s = c "32" s
let private yellow s = c "33" s
let private cyan s = c "36" s
let private magenta s = c "35" s

// ------------------------------------------------------------- number text --

/// Four-ish significant digits, thousands separators, no trailing noise.
let formatNumber (x: float) =
    if Double.IsNaN x then "?"
    elif Double.IsPositiveInfinity x then "+∞"
    elif Double.IsNegativeInfinity x then "-∞"
    elif x = 0.0 then "0"
    else
        let a = Math.Abs x

        if a >= 1e7 || a < 1e-4 then
            x.ToString("0.###e+0", inv)
        else
            let decimals =
                if a >= 1000.0 then 0
                elif a >= 100.0 then 1
                elif a >= 10.0 then 2
                elif a >= 1.0 then 3
                else 4

            x.ToString("#,##0." + String('#', decimals), inv)

let formatInterval (i: I) =
    if isEmpty i then "impossible"
    elif isPoint i then formatNumber i.Lo
    elif isEntire i then "anything"
    else formatNumber i.Lo + " .. " + formatNumber i.Hi

/// Half-width as a percentage of the midpoint: the "± x%" people expect.
let private tolerance (i: I) =
    if isEmpty i then None
    elif isPoint i then Some 0.0
    elif not (isBounded i) then None
    else
        let m = Math.Abs(mid i)
        if m = 0.0 then None else Some(100.0 * (width i / 2.0) / m)

let private toleranceText (i: I) =
    match tolerance i with
    | None -> if isBounded i then "" else "unbounded"
    | Some 0.0 -> "exact"
    | Some p when p >= 1000.0 -> ">1000%"
    | Some p -> "±" + p.ToString((if p < 10.0 then "0.0" else "0"), inv) + "%"

/// A small bar showing how much room is left in a quantity, so a table of
/// numbers reads as a shape at a glance.
let private bar (i: I) (cells: int) =
    if not (isBounded i) then String('░', cells)
    else
        match tolerance i with
        | None
        | Some 0.0 -> String('█', cells)
        | Some p ->
            // 0% -> full, 100%+ -> empty. Precision, not uncertainty.
            let frac = Math.Clamp(1.0 - p / 100.0, 0.0, 1.0)
            let filled = int (Math.Round(frac * float cells))
            String('█', filled) + String('░', cells - filled)

let private unitLabel (v: VarInfo) =
    match v.Display with
    | Some u when u.Label <> "1" -> u.Label
    | _ -> ""

// ------------------------------------------------------------ text report --

let renderText (path: string) (m: CompiledModel) (s: Solver.Solution) =
    let sb = StringBuilder()
    let line (t: string) = sb.AppendLine t |> ignore
    let blank () = sb.AppendLine() |> ignore

    blank ()
    line (bold "  PLIMSOLL" + dim ("  ·  " + path))

    // ---- diagnostics ----
    if not m.Warnings.IsEmpty then
        blank ()
        line (yellow "  NOTES")

        for w in m.Warnings do
            let where = if w.Line > 0 then sprintf "line %d: " w.Line else ""
            line ("    " + yellow "!" + " " + where + w.Message)

            match w.Hint with
            | Some h -> line (dim ("        " + h))
            | None -> ()

    let given = s.Vars |> Array.filter (fun r -> r.Var.Kind = GivenVar)
    let solved = s.Vars |> Array.filter (fun r -> r.Var.Kind <> GivenVar)

    let nameWidth =
        if s.Vars.Length = 0 then
            8
        else
            s.Vars |> Array.map (fun r -> r.Var.Name.Length) |> Array.max |> max 8

    let valueOf (r: Solver.VarResult) = formatInterval r.Display

    let valueWidth =
        if s.Vars.Length = 0 then
            10
        else
            s.Vars |> Array.map (fun r -> (valueOf r).Length) |> Array.max |> max 10

    let unitWidth =
        if s.Vars.Length = 0 then
            0
        else
            s.Vars |> Array.map (fun r -> (unitLabel r.Var).Length) |> Array.max

    let row (colour: string -> string) (r: Solver.VarResult) (withBar: bool) =
        let name = r.Var.Name.PadRight nameWidth
        let value = (valueOf r).PadLeft valueWidth
        let u = (unitLabel r.Var).PadRight unitWidth

        let tail =
            if withBar && s.Status <> Solver.Infeasible then
                "  " + dim (bar r.Display 10) + " " + dim (toleranceText r.Display)
            else
                ""

        line ("    " + name + "  " + colour value + "  " + dim u + tail)

    if given.Length > 0 then
        blank ()
        line (bold "  ASSUMED")

        for r in given do
            row cyan r false

    if solved.Length > 0 then
        blank ()
        line (bold "  IMPLIED")

        for r in solved do
            row (if s.Status = Solver.Infeasible then red else green) r true

    // ---- status ----
    blank ()

    match s.Status with
    | Solver.Infeasible ->
        line ("  " + red (bold "IMPOSSIBLE") + dim "  ·  no assignment satisfies this model")

        match s.Conflict with
        | Some conflict ->
            blank ()
            line (bold "  THE SMALLEST SET OF ASSUMPTIONS THAT STILL CONFLICTS")
            blank ()

            for v in conflict.Givens do
                let d =
                    match v.Display with
                    | Some u -> Units.toDisplay u v.Declared
                    | None -> v.Declared

                line (
                    "    "
                    + dim (sprintf "line %-4d" v.Line)
                    + " given "
                    + bold v.Name
                    + " = "
                    + formatInterval d
                    + " "
                    + dim (unitLabel v)
                )

            for r in conflict.Relations do
                let text = if String.IsNullOrWhiteSpace r.Text then Ast.toString r.Lhs else r.Text
                line ("    " + dim (sprintf "line %-4d" r.Line) + " " + text)

            blank ()
            line (dim "    Every one of these is load-bearing: drop any single one and the")
            line (dim "    model becomes satisfiable again.")
        | None -> ()

    | Solver.Certified ->
        line (
            "  "
            + green (bold "FEASIBLE")
            + dim "  ·  a region was found in which every relation provably holds"
        )

    | Solver.Consistent ->
        line ("  " + green (bold "CONSISTENT") + dim "  ·  no contradiction exists within these bounds")

        if m.Constraints |> Array.exists (fun k -> k.Rel.Op = Ast.Eq) then
            line (dim "              equalities have no volume, so no interior certificate is possible")

    // ---- sensitivity ----
    if not s.Sensitivities.IsEmpty then
        blank ()
        line (bold "  WHAT TO MEASURE NEXT")
        blank ()

        let top = s.Sensitivities |> List.truncate 5

        let w =
            top |> List.map (fun x -> x.Source.Name.Length) |> List.max |> max 8

        for x in top do
            let pct = (x.Reduction * 100.0).ToString("0", inv)

            line (
                "    pinning "
                + magenta (x.Source.Name.PadRight w)
                + "  removes "
                + bold (pct.PadLeft 3 + "%")
                + " of the uncertainty in "
                + cyan x.BestTarget.Name
            )

        blank ()
        line (dim "    Each figure is the width removed from that quantity by collapsing")
        line (dim "    the assumption to a single value. It is the answer to \"what should")
        line (dim "    I go and find out?\".")

    // ---- region ----
    match s.Region with
    | Some r ->
        blank ()
        line (bold "  FEASIBLE REGION" + dim (sprintf "  ·  %s (x) against %s (y)" r.XVar.Name r.YVar.Name))
        blank ()

        // Downsample the grid to something a terminal can show.
        let rows = 18
        let cols = 54

        for iy in rows - 1 .. -1 .. 0 do
            let sbRow = StringBuilder()

            for ix in 0 .. cols - 1 do
                let gx = ix * r.Cells / cols
                let gy = iy * r.Cells / rows

                let ch =
                    match r.Grid.[gy * r.Cells + gx] with
                    | Solver.Guaranteed -> '█'
                    | Solver.Possible -> '▒'
                    | Solver.Excluded -> '·'

                sbRow.Append ch |> ignore

            line ("    " + dim (sbRow.ToString()))

        let xu = unitLabel r.XVar
        let yu = unitLabel r.YVar

        line (
            "    "
            + dim (
                sprintf
                    "x: %s %s     y: %s %s"
                    (formatInterval (match r.XVar.Display with
                                     | Some u -> Units.toDisplay u r.XRange
                                     | None -> r.XRange))
                    xu
                    (formatInterval (match r.YVar.Display with
                                     | Some u -> Units.toDisplay u r.YRange
                                     | None -> r.YRange))
                    yu
            )
        )

        line (dim "    █ provably feasible   ▒ possible   · ruled out")
    | None -> ()

    blank ()

    line (
        dim (
            sprintf
                "  %d quantities, %d relations, %d constraint revisions, %d sub-problems solved"
                m.Vars.Length
                m.Constraints.Length
                s.PropagationSteps
                s.BoxesExamined
        )
    )

    blank ()
    sb.ToString()

// ------------------------------------------------------------------- JSON --

let private jstr (s: string) =
    let sb = StringBuilder()
    sb.Append '"' |> ignore

    for ch in s do
        match ch with
        | '"' -> sb.Append "\\\"" |> ignore
        | '\\' -> sb.Append "\\\\" |> ignore
        | '\n' -> sb.Append "\\n" |> ignore
        | '\r' -> sb.Append "\\r" |> ignore
        | '\t' -> sb.Append "\\t" |> ignore
        | c when c < ' ' -> sb.Append(sprintf "\\u%04x" (int c)) |> ignore
        | c -> sb.Append c |> ignore

    sb.Append '"' |> ignore
    sb.ToString()

/// JSON uses null for infinities: they are not valid JSON numbers, and a
/// consumer needs to distinguish "unbounded" from a huge magic value.
let private jnum (x: float) =
    if Double.IsNaN x || Double.IsInfinity x then
        "null"
    else
        x.ToString("R", inv)

let renderJson (m: CompiledModel) (s: Solver.Solution) =
    let sb = StringBuilder()

    let status =
        match s.Status with
        | Solver.Infeasible -> "infeasible"
        | Solver.Certified -> "certified"
        | Solver.Consistent -> "consistent"

    sb.Append("{\n") |> ignore
    sb.Append("  \"status\": ").Append(jstr status).Append(",\n") |> ignore

    sb.Append("  \"quantities\": [\n") |> ignore

    s.Vars
    |> Array.iteri (fun i r ->
        let kind =
            match r.Var.Kind with
            | GivenVar -> "given"
            | UnknownVar -> "unknown"
            | DerivedVar -> "derived"

        sb
            .Append("    {\"name\": ")
            .Append(jstr r.Var.Name)
            .Append(", \"kind\": ")
            .Append(jstr kind)
            .Append(", \"dimension\": ")
            .Append(jstr (Dimension.format r.Var.Dim))
            .Append(", \"unit\": ")
            .Append(jstr (unitLabel r.Var))
            .Append(", \"lo\": ")
            .Append(jnum r.Display.Lo)
            .Append(", \"hi\": ")
            .Append(jnum r.Display.Hi)
            .Append(", \"si_lo\": ")
            .Append(jnum r.Envelope.Lo)
            .Append(", \"si_hi\": ")
            .Append(jnum r.Envelope.Hi)
            .Append("}")
        |> ignore

        if i < s.Vars.Length - 1 then sb.Append ',' |> ignore
        sb.Append '\n' |> ignore)

    sb.Append("  ],\n") |> ignore

    sb.Append("  \"sensitivity\": [\n") |> ignore

    s.Sensitivities
    |> List.iteri (fun i x ->
        sb
            .Append("    {\"source\": ")
            .Append(jstr x.Source.Name)
            .Append(", \"target\": ")
            .Append(jstr x.BestTarget.Name)
            .Append(", \"reduction\": ")
            .Append(jnum x.Reduction)
            .Append("}")
        |> ignore

        if i < s.Sensitivities.Length - 1 then sb.Append ',' |> ignore
        sb.Append '\n' |> ignore)

    sb.Append("  ],\n") |> ignore

    match s.Conflict with
    | Some conflict ->
        sb.Append("  \"conflict\": {\n") |> ignore

        sb
            .Append("    \"givens\": [")
            .Append(conflict.Givens |> List.map (fun v -> jstr v.Name) |> String.concat ", ")
            .Append("],\n")
        |> ignore

        sb
            .Append("    \"relations\": [")
            .Append(
                conflict.Relations
                |> List.map (fun r -> sprintf "{\"line\": %d, \"text\": %s}" r.Line (jstr r.Text))
                |> String.concat ", "
            )
            .Append("]\n")
        |> ignore

        sb.Append("  },\n") |> ignore
    | None -> sb.Append("  \"conflict\": null,\n") |> ignore

    sb
        .Append("  \"stats\": {\"propagation_steps\": ")
        .Append(string s.PropagationSteps)
        .Append(", \"boxes_examined\": ")
        .Append(string s.BoxesExamined)
        .Append("},\n")
    |> ignore

    sb.Append("  \"warnings\": [") |> ignore

    sb.Append(
        m.Warnings
        |> List.map (fun w -> sprintf "{\"line\": %d, \"message\": %s}" w.Line (jstr w.Message))
        |> String.concat ", "
    )
    |> ignore

    sb.Append("]\n}\n") |> ignore
    sb.ToString()

// -------------------------------------------------------------------- SVG --

/// The feasible region as a standalone SVG. Colours are chosen to survive both
/// light and dark backgrounds.
let renderSvg (r: Solver.Region) =
    let cells = r.Cells
    let plot = 520.0
    let padL, padB, padT, padR = 78.0, 62.0, 46.0, 24.0
    let w = plot + padL + padR
    let h = plot + padT + padB
    let cw = plot / float cells
    let sb = StringBuilder()

    let f (x: float) = x.ToString("0.###", inv)

    sb
        .Append(sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %s %s\" width=\"%s\" height=\"%s\" font-family=\"ui-sans-serif, system-ui, sans-serif\">\n" (f w) (f h) (f w) (f h))
    |> ignore

    sb.Append("<rect width=\"100%\" height=\"100%\" fill=\"none\"/>\n") |> ignore

    sb
        .Append(sprintf "<text x=\"%s\" y=\"26\" font-size=\"15\" font-weight=\"600\" fill=\"#94a3b8\">Feasible region</text>\n" (f padL))
    |> ignore

    // Cells. Row 0 of the grid is the bottom of the picture.
    for iy in 0 .. cells - 1 do
        for ix in 0 .. cells - 1 do
            let st = r.Grid.[iy * cells + ix]

            if st <> Solver.Excluded then
                let x = padL + float ix * cw
                let y = padT + plot - float (iy + 1) * cw

                let fill =
                    match st with
                    | Solver.Guaranteed -> "#22c55e"
                    | _ -> "#3b82f6"

                let op =
                    match st with
                    | Solver.Guaranteed -> "0.85"
                    | _ -> "0.42"

                sb
                    .Append(sprintf "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\" fill-opacity=\"%s\"/>\n" (f x) (f y) (f (cw + 0.5)) (f (cw + 0.5)) fill op)
                |> ignore

    // Frame and axes.
    sb
        .Append(sprintf "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"none\" stroke=\"#64748b\" stroke-width=\"1\"/>\n" (f padL) (f padT) (f plot) (f plot))
    |> ignore

    let disp (v: VarInfo) (i: I) =
        match v.Display with
        | Some u -> Units.toDisplay u i
        | None -> i

    let xr = disp r.XVar r.XRange
    let yr = disp r.YVar r.YRange

    let label (v: VarInfo) =
        match v.Display with
        | Some u when u.Label <> "1" -> v.Name + " [" + u.Label + "]"
        | _ -> v.Name

    let tick (t: float) = formatNumber t

    // x ticks
    for k in 0 .. 4 do
        let frac = float k / 4.0
        let x = padL + frac * plot
        let value = xr.Lo + frac * (xr.Hi - xr.Lo)

        sb
            .Append(sprintf "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"#64748b\"/>\n" (f x) (f (padT + plot)) (f x) (f (padT + plot + 5.0)))
        |> ignore

        sb
            .Append(sprintf "<text x=\"%s\" y=\"%s\" font-size=\"12\" fill=\"#94a3b8\" text-anchor=\"middle\">%s</text>\n" (f x) (f (padT + plot + 20.0)) (tick value))
        |> ignore

    // y ticks
    for k in 0 .. 4 do
        let frac = float k / 4.0
        let y = padT + plot - frac * plot
        let value = yr.Lo + frac * (yr.Hi - yr.Lo)

        sb
            .Append(sprintf "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"#64748b\"/>\n" (f (padL - 5.0)) (f y) (f padL) (f y))
        |> ignore

        sb
            .Append(sprintf "<text x=\"%s\" y=\"%s\" font-size=\"12\" fill=\"#94a3b8\" text-anchor=\"end\">%s</text>\n" (f (padL - 9.0)) (f (y + 4.0)) (tick value))
        |> ignore

    sb
        .Append(sprintf "<text x=\"%s\" y=\"%s\" font-size=\"13\" fill=\"#cbd5e1\" text-anchor=\"middle\">%s</text>\n" (f (padL + plot / 2.0)) (f (h - 16.0)) (label r.XVar))
    |> ignore

    sb
        .Append(sprintf "<text x=\"16\" y=\"%s\" font-size=\"13\" fill=\"#cbd5e1\" text-anchor=\"middle\" transform=\"rotate(-90 16 %s)\">%s</text>\n" (f (padT + plot / 2.0)) (f (padT + plot / 2.0)) (label r.YVar))
    |> ignore

    // Legend
    sb
        .Append(sprintf "<rect x=\"%s\" y=\"12\" width=\"12\" height=\"12\" fill=\"#22c55e\" fill-opacity=\"0.85\"/><text x=\"%s\" y=\"22\" font-size=\"12\" fill=\"#94a3b8\">provable</text>\n" (f (padL + plot - 190.0)) (f (padL + plot - 172.0)))
    |> ignore

    sb
        .Append(sprintf "<rect x=\"%s\" y=\"12\" width=\"12\" height=\"12\" fill=\"#3b82f6\" fill-opacity=\"0.42\"/><text x=\"%s\" y=\"22\" font-size=\"12\" fill=\"#94a3b8\">possible</text>\n" (f (padL + plot - 100.0)) (f (padL + plot - 82.0)))
    |> ignore

    sb.Append("</svg>\n") |> ignore
    sb.ToString()
