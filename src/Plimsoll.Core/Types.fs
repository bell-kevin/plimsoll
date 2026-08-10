// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// The compiled form of a model: a variable table plus constraints reduced to
/// straight-line tapes over SI base units.
module Plimsoll.Core.Types

open Plimsoll.Core.Dimension
open Plimsoll.Core.Interval
open Plimsoll.Core.Units
open Plimsoll.Core.Ast

type VarKind =
    /// The author supplied a range or a value.
    | GivenVar
    /// Declared with `unknown`, no range.
    | UnknownVar
    /// Never declared; appeared in a relation and was created on demand.
    | DerivedVar

type VarInfo =
    { Name: string
      Index: int
      Kind: VarKind
      /// The declared domain, in SI base units. `entire` when undeclared.
      Declared: I
      /// The unit the author wrote, used to render results back in their terms.
      Display: U option
      Dim: Dim
      Line: int }

/// A relation as written, kept alongside its compiled tape so that reports can
/// quote the author's own line.
type RelInfo =
    { Id: int
      Op: RelOp
      Lhs: Expr
      Rhs: Expr
      Line: int
      Text: string }

/// A single instruction of a constraint tape. Operands are indices of earlier
/// instructions, so a tape is a topologically sorted expression DAG. Flattening
/// to an array is what makes the two-pass HC4 contraction (forward evaluate,
/// then walk backwards projecting) a pair of simple loops.
type Instr =
    | IConst of I
    | IVar of int
    | INeg of int
    | IAdd of int * int
    | ISub of int * int
    | IMul of int * int
    | IDiv of int * int
    | IPow of int * Rational.Rat
    | ISqrt of int
    | IExp of int
    | ILog of int
    | IAbs of int
    | IMin of int * int
    | IMax of int * int

/// A relation compiled to `tape[Root] ∈ Target`.
///
/// Every relation becomes a membership test on a single expression:
/// `l = r` becomes `l - r ∈ [0,0]`, `l <= r` becomes `l - r ∈ [-inf,0]`, and
/// `l >= r` becomes `l - r ∈ [0,+inf]`. One shape for the contractor to handle.
type Constraint =
    { Tape: Instr[]
      Root: int
      Target: I
      /// Variables this constraint touches, for the propagation queue.
      Vars: int[]
      Rel: RelInfo }

type CompiledModel =
    { Vars: VarInfo[]
      Constraints: Constraint[]
      Registry: Registry
      /// Variable index pairs named by `plot` statements.
      Plots: (int * int) list
      Warnings: Diagnostics.Diag list }

    member this.VarByName(name: string) =
        this.Vars |> Array.tryFind (fun v -> v.Name = name)

/// The built-in functions, with their arities.
let functionArity =
    Map.ofList [ "sqrt", 1; "exp", 1; "log", 1; "abs", 1; "min", 2; "max", 2 ]
