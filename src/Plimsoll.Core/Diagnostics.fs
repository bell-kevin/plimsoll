// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Diagnostics carried out of parsing, dimensional checking and solving.
module Plimsoll.Core.Diagnostics

/// Qualified access is required: an unqualified `Error` case would shadow
/// `Result.Error` in every module that opens this one.
[<RequireQualifiedAccess>]
type Severity =
    | Error
    | Warning

type Diag =
    { Severity: Severity
      Line: int
      Message: string
      /// Optional second line offering a concrete fix.
      Hint: string option }

let error line msg =
    { Severity = Severity.Error
      Line = line
      Message = msg
      Hint = None }

let errorWith line msg hint =
    { Severity = Severity.Error
      Line = line
      Message = msg
      Hint = Some hint }

let warning line msg =
    { Severity = Severity.Warning
      Line = line
      Message = msg
      Hint = None }

let warningWith line msg hint =
    { Severity = Severity.Warning
      Line = line
      Message = msg
      Hint = Some hint }

/// Raised internally by the parser and checker; converted to `Result` at the
/// module boundary so callers never see exceptions.
exception PlimError of Diag

let fail line msg = raise (PlimError(error line msg))
let failWith line msg hint = raise (PlimError(errorWith line msg hint))

let format (d: Diag) =
    let sev =
        match d.Severity with
        | Severity.Error -> "error"
        | Severity.Warning -> "warning"

    let head =
        if d.Line > 0 then
            sprintf "line %d: %s: %s" d.Line sev d.Message
        else
            sprintf "%s: %s" sev d.Message

    match d.Hint with
    | Some h -> head + "\n  hint: " + h
    | None -> head
