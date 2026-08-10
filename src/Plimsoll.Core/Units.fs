// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// The unit registry, and evaluation of unit expressions.
///
/// A unit is a dimension plus a scale factor into SI base units. The solver
/// works entirely in SI base units; units exist at the edges, to read models in
/// and to write answers back out in whatever the author wrote.
///
/// Offset scales (degrees Celsius, Fahrenheit) are deliberately absent. A unit
/// here is a pure scaling, which is what makes `x^(1/2)` and `a*b` meaningful;
/// offsets break that algebra. Use K for absolute temperature.
module Plimsoll.Core.Units

open Plimsoll.Core.Dimension
open Plimsoll.Core.Diagnostics
open Plimsoll.Core.Ast

type U =
    { Dim: Dim
      /// value_in_SI = value_as_written * Factor
      Factor: float
      /// How the author spelled it, preserved for output.
      Label: string }

type Registry =
    { Units: Map<string, U>
      Dims: Set<string> }

let private mk dim factor label =
    { Dim = dim; Factor = factor; Label = label }

let dimensionless label = mk one 1.0 label

/// Decimal SI prefixes plus the IEC binary ones. Longest first, so that "da"
/// and "Ki" are matched before "d" and "K".
let prefixes: (string * float) list =
    [ "Ki", 1024.0
      "Mi", 1024.0 ** 2.0
      "Gi", 1024.0 ** 3.0
      "Ti", 1024.0 ** 4.0
      "Pi", 1024.0 ** 5.0
      "da", 1e1
      "Y", 1e24
      "Z", 1e21
      "E", 1e18
      "P", 1e15
      "T", 1e12
      "G", 1e9
      "M", 1e6
      "k", 1e3
      "h", 1e2
      "d", 1e-1
      "c", 1e-2
      "m", 1e-3
      "u", 1e-6
      "µ", 1e-6
      "μ", 1e-6
      "n", 1e-9
      "p", 1e-12
      "f", 1e-15
      "a", 1e-18
      "z", 1e-21
      "y", 1e-24 ]

/// The built-in registry: seven SI base dimensions, an `information` dimension
/// because byte-counting models are common, and the usual derived units.
let baseRegistry: Registry =
    let dLen = ofBase Base.length
    let dMass = ofBase Base.mass
    let dTime = ofBase Base.time
    let dCurr = ofBase Base.current
    let dTemp = ofBase Base.temperature
    let dAmt = ofBase Base.amount
    let dLum = ofBase Base.luminosity
    let dInfo = ofBase "information"

    let p2 d = pow d (Rational.ofInt 2)
    let p3 d = pow d (Rational.ofInt 3)

    let force = div (mul dMass dLen) (p2 dTime) // kg·m·s⁻²
    let energy = mul force dLen
    let power = div energy dTime
    let pressure = div force (p2 dLen)
    let charge = mul dCurr dTime
    let voltage = div power dCurr

    let units =
        [ // SI base
          "m", mk dLen 1.0 "m"
          "kg", mk dMass 1.0 "kg"
          "g", mk dMass 1e-3 "g"
          "s", mk dTime 1.0 "s"
          "A", mk dCurr 1.0 "A"
          "K", mk dTemp 1.0 "K"
          "mol", mk dAmt 1.0 "mol"
          "cd", mk dLum 1.0 "cd"
          // Named derived
          "Hz", mk (pow dTime (Rational.ofInt -1)) 1.0 "Hz"
          "N", mk force 1.0 "N"
          "Pa", mk pressure 1.0 "Pa"
          "J", mk energy 1.0 "J"
          "W", mk power 1.0 "W"
          "C", mk charge 1.0 "C"
          "V", mk voltage 1.0 "V"
          "F", mk (div charge voltage) 1.0 "F"
          "ohm", mk (div voltage dCurr) 1.0 "ohm"
          "Wh", mk energy 3600.0 "Wh"
          // Length / volume / mass conventions
          "L", mk (p3 dLen) 1e-3 "L"
          "t", mk dMass 1e3 "t"
          "km", mk dLen 1e3 "km"
          "mi", mk dLen 1609.344 "mi"
          "ft", mk dLen 0.3048 "ft"
          "inch", mk dLen 0.0254 "inch"
          "ha", mk (p2 dLen) 1e4 "ha"
          // Time, including calendar units. A month is a twelfth of a Julian
          // year, which is the only self-consistent choice.
          "min", mk dTime 60.0 "min"
          "h", mk dTime 3600.0 "h"
          "hr", mk dTime 3600.0 "hr"
          "day", mk dTime 86400.0 "day"
          "week", mk dTime 604800.0 "week"
          "month", mk dTime (86400.0 * 365.25 / 12.0) "month"
          "year", mk dTime (86400.0 * 365.25) "year"
          // Pressure
          "bar", mk pressure 1e5 "bar"
          // Information
          "bit", mk dInfo 1.0 "bit"
          "B", mk dInfo 8.0 "B"
          // Dimensionless conveniences
          "percent", mk one 0.01 "percent"
          "%", mk one 0.01 "%"
          "rad", mk one 1.0 "rad"
          "sr", mk one 1.0 "sr"
          "deg", mk one (System.Math.PI / 180.0) "deg" ]
        |> Map.ofList

    { Units = units
      Dims = Set.ofList (("information" :: Base.si)) }

/// Resolve a symbol, trying an exact match before any prefix decomposition.
/// Exact-first is what keeps `min` a minute rather than a milli-inch, and `kg`
/// a kilogram rather than a kilo-gram-with-the-wrong-factor.
let tryLookup (reg: Registry) (sym: string) : U option =
    match Map.tryFind sym reg.Units with
    | Some u -> Some u
    | None ->
        prefixes
        |> List.tryPick (fun (p, mult) ->
            if sym.Length > p.Length && sym.StartsWith(p, System.StringComparison.Ordinal) then
                let rest = sym.Substring(p.Length)

                match Map.tryFind rest reg.Units with
                | Some u ->
                    Some
                        { Dim = u.Dim
                          Factor = u.Factor * mult
                          Label = sym }
                | None -> None
            else
                None)

/// Suggest near-miss unit names, so an unknown symbol produces a useful hint.
let private suggest (reg: Registry) (sym: string) =
    let lower = sym.ToLowerInvariant()

    reg.Units
    |> Map.toList
    |> List.map fst
    |> List.filter (fun k ->
        k.ToLowerInvariant() = lower
        || (k.Length > 1 && lower.StartsWith(k.ToLowerInvariant(), System.StringComparison.Ordinal)))
    |> List.truncate 3

/// Evaluate a unit expression to a dimension and scale factor.
///
/// `Quantity` is accepted so that a scaled unit definition reads naturally:
/// `unit kUSD = 1000 [USD]`.
let rec evalUnit (reg: Registry) (line: int) (e: Expr) : U =
    match e with
    | Num v -> mk one v (Interval.toString (Interval.point v))
    | Quantity(v, u) ->
        let inner = evalUnit reg line u

        { Dim = inner.Dim
          Factor = v * inner.Factor
          Label = inner.Label }
    | Name n ->
        match tryLookup reg n with
        | Some u -> { u with Label = n }
        | None ->
            match suggest reg n with
            | [] ->
                failWith
                    line
                    (sprintf "unknown unit '%s'" n)
                    "declare it first, e.g. `unit request` for a fresh dimension, or `unit kUSD = 1000 [USD]`"
            | ss -> failWith line (sprintf "unknown unit '%s'" n) (sprintf "did you mean %s?" (String.concat " or " ss))
    | Bin(Mul, a, b) ->
        let x, y = evalUnit reg line a, evalUnit reg line b

        { Dim = mul x.Dim y.Dim
          Factor = x.Factor * y.Factor
          Label = x.Label + "·" + y.Label }
    | Bin(Div, a, b) ->
        let x, y = evalUnit reg line a, evalUnit reg line b

        { Dim = div x.Dim y.Dim
          Factor = x.Factor / y.Factor
          Label = x.Label + "/" + y.Label }
    | Pow(a, r) ->
        let x = evalUnit reg line a

        { Dim = pow x.Dim r
          Factor = x.Factor ** Rational.toFloat r
          Label = x.Label + "^" + Rational.toString r }
    | Bin(Add, _, _)
    | Bin(Sub, _, _) ->
        failWith line "units cannot be added or subtracted" "a unit is a product of powers, e.g. [kg*m/s^2]"
    | Neg _ -> fail line "a unit cannot be negated"
    | Call(f, _) -> fail line (sprintf "'%s(...)' is not valid inside a unit" f)

/// Add a fresh base dimension together with a base unit of the same name.
let addDimension (reg: Registry) (name: string) =
    { Units = Map.add name (mk (ofBase name) 1.0 name) reg.Units
      Dims = Set.add name reg.Dims }

let addUnit (reg: Registry) (name: string) (u: U) =
    { reg with Units = Map.add name { u with Label = name } reg.Units }

/// The unit Plimsoll falls back to when the author never named one: SI base
/// units composed from the dimension itself.
/// (`Dimension.format` must be qualified: `Diagnostics.format` shadows it here.)
let siFor (d: Dim) = mk d 1.0 (Dimension.format d)

/// The nicest named unit for a dimension, if the registry has one: `W` reads
/// better than `kg·m²·s⁻³` when reporting an inferred quantity.
///
/// Only unscaled units qualify, so hours never stand in for seconds. The
/// dimensionless case is excluded deliberately -- `rad` is technically a match
/// and would be a bizarre label for a ratio.
let bestNamedUnit (reg: Registry) (d: Dim) : U option =
    if isOne d then
        None
    else
        reg.Units
        |> Map.toList
        |> List.filter (fun (_, u) -> equal u.Dim d && u.Factor = 1.0)
        |> List.sortBy (fun (k, _) -> k.Length, k)
        |> List.tryHead
        |> Option.map snd

/// The unit used to present a quantity the author never annotated.
let displayFor (reg: Registry) (d: Dim) =
    match bestNamedUnit reg d with
    | Some u -> u
    | None -> siFor d

/// Convert an SI-normalised interval into the author's display unit.
let toDisplay (u: U) (i: Interval.I) =
    if Interval.isEmpty i then i
    else Interval.div i (Interval.point u.Factor)

/// Convert an as-written interval into SI base units.
let toSi (u: U) (i: Interval.I) = Interval.mul i (Interval.point u.Factor)
