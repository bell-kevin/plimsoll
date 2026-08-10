// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

module Plimsoll.Cli.Program

open System
open System.IO
open Plimsoll.Core
open Plimsoll.Present

let version = "0.1.0"

let private usage =
    """
plimsoll - solve relations between quantities that are only known to a range

USAGE
    plimsoll <model.plim> [options]
    plimsoll -                        read the model from standard input

OPTIONS
    --json               emit machine-readable JSON instead of a report
    --svg <path>         write the feasible region to an SVG file
    --tolerance <x>      relative width to refine to (default 0.001)
    --cells <n>          resolution of the feasible-region grid (default 44)
    --no-sensitivity     skip the "what to measure next" analysis
    --no-colour          disable ANSI colour (or set NO_COLOR)
    --version            print the version and exit
    --help               print this message

EXIT STATUS
    0   the model is satisfiable
    2   the model is impossible, and the conflicting assumptions are reported
    1   the model could not be read

    A model file therefore works as an assertion: put one in CI and a design
    that drifts out of its own limits will fail the build.

LICENCE
    AGPL-3.0-or-later. This is free software: you may redistribute and modify
    it, and if you run a modified version over a network you must offer its
    source to your users.
"""

type private Args =
    { Path: string option
      Json: bool
      Svg: string option
      Opts: Solver.Options
      Help: bool
      Version: bool }

let private parseArgs (argv: string[]) =
    let mutable a =
        { Path = None
          Json = false
          Svg = None
          Opts = Solver.defaults
          Help = false
          Version = false }

    let mutable i = 0
    let mutable error = None

    let needValue (flag: string) =
        if i + 1 < argv.Length then
            i <- i + 1
            Some argv.[i]
        else
            error <- Some(sprintf "%s needs a value" flag)
            None

    while i < argv.Length && error.IsNone do
        match argv.[i] with
        | "--help"
        | "-h" -> a <- { a with Help = true }
        | "--version" -> a <- { a with Version = true }
        | "--json" -> a <- { a with Json = true }
        | "--no-colour"
        | "--no-color" -> Report.useColour <- false
        | "--no-sensitivity" ->
            a <-
                { a with
                    Opts = { a.Opts with ComputeSensitivity = false } }
        | "--svg" ->
            match needValue "--svg" with
            | Some v -> a <- { a with Svg = Some v }
            | None -> ()
        | "--tolerance" ->
            match needValue "--tolerance" with
            | Some v ->
                match Double.TryParse(v, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                | true, x when x > 0.0 -> a <- { a with Opts = { a.Opts with Tolerance = x } }
                | _ -> error <- Some "--tolerance needs a positive number"
            | None -> ()
        | "--cells" ->
            match needValue "--cells" with
            | Some v ->
                match Int32.TryParse v with
                | true, n when n >= 4 && n <= 400 -> a <- { a with Opts = { a.Opts with RegionCells = n } }
                | _ -> error <- Some "--cells needs a number between 4 and 400"
            | None -> ()
        | flag when flag.StartsWith "-" && flag <> "-" -> error <- Some(sprintf "unknown option %s" flag)
        | path ->
            match a.Path with
            | None -> a <- { a with Path = Some path }
            | Some _ -> error <- Some "give only one model file"

        i <- i + 1

    match error with
    | Some e -> Result.Error e
    | None -> Ok a

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8

    match parseArgs argv with
    | Result.Error e ->
        eprintfn "plimsoll: %s" e
        eprintfn "try 'plimsoll --help'"
        1
    | Ok args ->

    if args.Help || (argv.Length = 0) then
        printfn "%s" usage
        0
    elif args.Version then
        printfn "plimsoll %s" version
        0
    else
        match args.Path with
        | None ->
            eprintfn "plimsoll: no model file given"
            1
        | Some path ->
            let source =
                try
                    if path = "-" then Ok(Console.In.ReadToEnd()) else Ok(File.ReadAllText path)
                with e ->
                    Result.Error e.Message

            match source with
            | Result.Error e ->
                eprintfn "plimsoll: cannot read %s: %s" path e
                1
            | Ok src ->
                match Model.run src args.Opts with
                | Result.Error d ->
                    eprintfn ""
                    eprintfn "  %s: %s" path (Diagnostics.format d)
                    eprintfn ""
                    1
                | Ok(m, solution) ->
                    if args.Json then
                        printf "%s" (Report.renderJson m solution)
                    else
                        printf "%s" (Report.renderText path m solution)

                    match args.Svg, solution.Region with
                    | Some out, Some region ->
                        try
                            File.WriteAllText(out, Report.renderSvg region)
                            if not args.Json then printfn "  wrote %s" out
                        with e ->
                            eprintfn "plimsoll: cannot write %s: %s" out e.Message
                    | Some _, None ->
                        eprintfn "plimsoll: no region to draw; add a `plot x, y` statement with two bounded quantities"
                    | None, _ -> ()

                    // An impossible model is a failure, so this can gate CI.
                    if solution.Status = Solver.Infeasible then 2 else 0
