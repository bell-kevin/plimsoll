// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// The dimension algebra: the free abelian group over base dimension names,
/// with rational exponents.
///
/// A dimension is a map from base name to exponent, e.g. force is
/// `{mass:1, length:1, time:-2}`. Multiplication adds exponents, division
/// subtracts, raising to a power scales. Dimensionless is the empty map --
/// which is why `Map.empty` is the group identity and zero exponents are
/// always pruned: two spellings of dimensionless must compare equal.
module Plimsoll.Core.Dimension

open Plimsoll.Core.Rational

type Dim = Map<string, Rat>

/// The seven SI base dimensions. Models may declare more (`dimension money`).
module Base =
    let length = "length"
    let mass = "mass"
    let time = "time"
    let current = "current"
    let temperature = "temperature"
    let amount = "amount"
    let luminosity = "luminosity"

    let si =
        [ length; mass; time; current; temperature; amount; luminosity ]

/// Dimensionless.
let one: Dim = Map.empty

let ofBase (name: string) : Dim = Map.ofList [ name, Rational.one ]

let isOne (d: Dim) = Map.isEmpty d

/// Drop zero exponents so that structural equality means dimensional equality.
let private prune (d: Dim) : Dim =
    d |> Map.filter (fun _ e -> not (isZero e))

let mul (a: Dim) (b: Dim) : Dim =
    b
    |> Map.fold
        (fun acc k e ->
            let cur = Map.tryFind k acc |> Option.defaultValue Rational.zero
            Map.add k (Rational.add cur e) acc)
        a
    |> prune

let pow (a: Dim) (e: Rat) : Dim =
    // Note: `Rational.mul` must be qualified -- `mul` above shadows it here.
    if isZero e then
        one
    else
        a |> Map.map (fun _ x -> Rational.mul x e) |> prune

let inv (a: Dim) : Dim = pow a (ofInt -1)

let div (a: Dim) (b: Dim) : Dim = mul a (inv b)

let equal (a: Dim) (b: Dim) = prune a = prune b

/// A canonical, human-readable rendering: positive exponents first, then
/// negative, e.g. `kg·m·s⁻²`. Used in reports and error messages.
let format (d: Dim) =
    let d = prune d

    if Map.isEmpty d then
        "1"
    else
        // Short SI letters make the output compact; declared dimensions keep
        // their full name so a model's own vocabulary stays recognisable.
        let symbol name =
            match name with
            | "length" -> "m"
            | "mass" -> "kg"
            | "time" -> "s"
            | "current" -> "A"
            | "temperature" -> "K"
            | "amount" -> "mol"
            | "luminosity" -> "cd"
            | other -> other

        let render (name, e: Rat) =
            if e = Rational.one then
                symbol name
            else
                symbol name + toExponentString e

        // SI writes mass before length before time -- kg·m·s⁻², not m·kg·s⁻².
        // Base dimensions follow that convention; dimensions the model declared
        // for itself come afterwards, alphabetically.
        let conventionalOrder =
            [ Base.mass
              Base.length
              Base.time
              Base.current
              Base.temperature
              Base.amount
              Base.luminosity
              "information" ]

        let rank (name: string) =
            match List.tryFindIndex ((=) name) conventionalOrder with
            | Some i -> i
            | None -> List.length conventionalOrder

        let sortKey (name: string, _) = rank name, name

        let pos =
            d |> Map.toList |> List.filter (fun (_, e) -> e > Rational.zero) |> List.sortBy sortKey

        let neg =
            d |> Map.toList |> List.filter (fun (_, e) -> e < Rational.zero) |> List.sortBy sortKey

        (pos @ neg) |> List.map render |> String.concat "·"
