// Plimsoll - a dimensionally-typed interval constraint solver.
// Copyright (C) 2026  Plimsoll contributors
// SPDX-License-Identifier: AGPL-3.0-or-later

/// Sound interval arithmetic over the extended reals.
///
/// "Sound" means the computed interval is *guaranteed* to contain the true
/// mathematical result. Every result is rounded outward, so no real value that
/// satisfies the model can ever fall outside a Plimsoll bound. This is the
/// property that separates Plimsoll from sampling tools: a Monte Carlo run of
/// 10^6 draws tells you where samples landed, not where the answer must lie.
///
/// .NET gives no access to the hardware rounding mode, so instead of rounding
/// down/up in hardware we compute in round-to-nearest and then widen by one
/// ULP -- but only when the operation was actually inexact. Inexactness is
/// detected exactly, via Knuth's TwoSum for +/- and fused-multiply-add
/// residuals for *, / and sqrt. So `[2,2] + [2,2]` stays exactly `[4,4]`
/// while `[1,1] / [3,3]` correctly widens to straddle 1/3.
module Plimsoll.Core.Interval

open System

[<Struct>]
type I = { Lo: float; Hi: float }

// ---------------------------------------------------------------- rounding --

/// Next representable value below x (saturating at -infinity).
let inline private down (x: float) =
    if Double.IsNaN x then x
    elif Double.IsNegativeInfinity x then x
    else Math.BitDecrement x

/// Next representable value above x (saturating at +infinity).
let inline private up (x: float) =
    if Double.IsNaN x then x
    elif Double.IsPositiveInfinity x then x
    else Math.BitIncrement x

let inline private finite (x: float) = not (Double.IsNaN x || Double.IsInfinity x)

/// Knuth's TwoSum: a + b is exact in floating point iff the residual is zero.
/// Valid for any magnitudes, unlike the cheaper Fast2Sum.
let inline private addIsExact (a: float) (b: float) (s: float) =
    if not (finite a && finite b && finite s) then
        false
    else
        let bv = s - a
        let av = s - bv
        (a - av) + (b - bv) = 0.0

let inline private mulIsExact (a: float) (b: float) (p: float) =
    if not (finite a && finite b && finite p) then
        false
    else
        Math.FusedMultiplyAdd(a, b, -p) = 0.0

let inline private divIsExact (a: float) (b: float) (q: float) =
    if not (finite a && finite b && finite q) then
        false
    else
        Math.FusedMultiplyAdd(q, b, -a) = 0.0

/// Round a sum outward on the low side only when it was inexact.
let inline private addD a b =
    let s = a + b
    if addIsExact a b s then s else down s

let inline private addU a b =
    let s = a + b
    if addIsExact a b s then s else up s

/// 0 * infinity is taken as 0 here. This is the standard convention for
/// extended interval multiplication: a factor that is exactly zero annihilates
/// an unbounded factor, because the zero endpoint contributes a zero product.
let inline private mulRaw (a: float) (b: float) =
    if (a = 0.0 && Double.IsInfinity b) || (b = 0.0 && Double.IsInfinity a) then
        0.0
    else
        a * b

let inline private mulD a b =
    let p = mulRaw a b
    if mulIsExact a b p then p else down p

let inline private mulU a b =
    let p = mulRaw a b
    if mulIsExact a b p then p else up p

let inline private divD a b =
    let q = a / b
    if divIsExact a b q then q else down q

let inline private divU a b =
    let q = a / b
    if divIsExact a b q then q else up q

/// Transcendentals are not guaranteed correctly rounded by the platform libm,
/// so we widen by two ULPs rather than one. Slightly loose, always sound.
let inline private looseD (x: float) = down (down x)
let inline private looseU (x: float) = up (up x)

// ------------------------------------------------------------ construction --

/// The canonical empty interval. Chosen as (+inf, -inf) so that `intersect`
/// (max of los, min of his) and `hull` (min of los, max of his) both treat it
/// as the correct identity without special-casing.
let empty = { Lo = Double.PositiveInfinity; Hi = Double.NegativeInfinity }

let entire =
    { Lo = Double.NegativeInfinity
      Hi = Double.PositiveInfinity }

let nonNegative =
    { Lo = 0.0; Hi = Double.PositiveInfinity }

/// True when the interval holds no real number. NaN endpoints are empty too,
/// which keeps every downstream operation total.
let inline isEmpty (a: I) = not (a.Lo <= a.Hi)

let inline isEntire (a: I) =
    Double.IsNegativeInfinity a.Lo && Double.IsPositiveInfinity a.Hi

let inline isBounded (a: I) = finite a.Lo && finite a.Hi

/// Build an interval, normalising anything degenerate to `empty`.
let make (lo: float) (hi: float) =
    if Double.IsNaN lo || Double.IsNaN hi || lo > hi then
        empty
    else
        { Lo = lo; Hi = hi }

let point (x: float) = if Double.IsNaN x then empty else { Lo = x; Hi = x }

let inline isPoint (a: I) = not (isEmpty a) && a.Lo = a.Hi

// ------------------------------------------------------------- measurement --

let width (a: I) =
    if isEmpty a then
        0.0
    elif isBounded a then
        // Only widen an inexact difference. Widening unconditionally would give
        // a pinned value a width of one denormal instead of zero, and every
        // "is this exact yet?" test downstream would answer no forever.
        let d = a.Hi - a.Lo
        if addIsExact a.Hi (-a.Lo) d then d else up d
    else
        Double.PositiveInfinity

/// A finite, well-behaved interior point. Unbounded sides are stepped in from
/// the bound so that bisection and midpoint-pinning always make progress.
let mid (a: I) =
    if isEmpty a then
        Double.NaN
    elif isBounded a then
        if a.Lo = a.Hi then a.Lo
        else
            let m = a.Lo + (a.Hi - a.Lo) * 0.5
            if finite m then m else a.Lo * 0.5 + a.Hi * 0.5
    elif finite a.Lo then a.Lo + 1.0
    elif finite a.Hi then a.Hi - 1.0
    else 0.0

/// Width relative to magnitude -- the honest way to report "how pinned down is
/// this?" for quantities whose scale we do not know in advance.
let relativeWidth (a: I) =
    if isEmpty a then 0.0
    elif not (isBounded a) then Double.PositiveInfinity
    else
        let scale = max (abs a.Lo) (abs a.Hi)
        if scale = 0.0 then 0.0 else width a / scale

let inline contains (x: float) (a: I) = a.Lo <= x && x <= a.Hi
let inline containsZero (a: I) = a.Lo <= 0.0 && 0.0 <= a.Hi

/// True when every point of `a` lies inside `b`.
let isSubsetOf (b: I) (a: I) =
    isEmpty a || (not (isEmpty b) && b.Lo <= a.Lo && a.Hi <= b.Hi)

// ------------------------------------------------------------- set algebra --

let intersect (a: I) (b: I) =
    if isEmpty a || isEmpty b then empty else make (max a.Lo b.Lo) (min a.Hi b.Hi)

let hull (a: I) (b: I) =
    if isEmpty a then b
    elif isEmpty b then a
    else { Lo = min a.Lo b.Lo; Hi = max a.Hi b.Hi }

let hullOf (xs: I seq) = xs |> Seq.fold hull empty

// -------------------------------------------------------------- arithmetic --

let neg (a: I) = if isEmpty a then empty else { Lo = -a.Hi; Hi = -a.Lo }

let add (a: I) (b: I) =
    if isEmpty a || isEmpty b then empty else make (addD a.Lo b.Lo) (addU a.Hi b.Hi)

let sub (a: I) (b: I) =
    if isEmpty a || isEmpty b then empty else make (addD a.Lo (-b.Hi)) (addU a.Hi (-b.Lo))

let mul (a: I) (b: I) =
    if isEmpty a || isEmpty b then
        empty
    else
        // The extrema of a product of intervals always occur at endpoint pairs.
        let lo =
            min (min (mulD a.Lo b.Lo) (mulD a.Lo b.Hi)) (min (mulD a.Hi b.Lo) (mulD a.Hi b.Hi))

        let hi =
            max (max (mulU a.Lo b.Lo) (mulU a.Lo b.Hi)) (max (mulU a.Hi b.Lo) (mulU a.Hi b.Hi))

        make lo hi

/// Reciprocal of an interval that does not straddle zero in its interior.
let private recipBranch (lo: float) (hi: float) =
    // Caller guarantees 0 <= lo or hi <= 0.
    let r1 = if hi = 0.0 then Double.NegativeInfinity else divD 1.0 hi
    let r2 = if lo = 0.0 then Double.PositiveInfinity else divU 1.0 lo
    make (min r1 r2) (max r1 r2)

/// Extended interval division, returning up to two disjoint pieces.
///
/// Dividing by an interval that straddles zero genuinely produces a gap:
/// [1,1] / [-1,1] is (-inf,-1] union [1,+inf). Collapsing that to its hull
/// would throw away the whole constraint, so the pieces are kept separate and
/// intersected individually by the contractor. This is what lets Plimsoll
/// narrow a variable that sits under a division.
let divList (a: I) (b: I) : I list =
    if isEmpty a || isEmpty b then
        []
    elif not (containsZero b) then
        [ mul a (recipBranch b.Lo b.Hi) ]
    elif b.Lo = 0.0 && b.Hi = 0.0 then
        // Division by exactly zero: only 0/0 is unconstrained, anything else empty.
        if containsZero a then [ entire ] else []
    elif containsZero a then
        // Numerator and denominator both straddle zero: no information.
        [ entire ]
    else
        // Two half-branches, one for each sign of the denominator.
        let negBranch =
            if b.Lo < 0.0 then [ mul a (recipBranch b.Lo 0.0) ] else []

        let posBranch =
            if b.Hi > 0.0 then [ mul a (recipBranch 0.0 b.Hi) ] else []

        negBranch @ posBranch

/// Division collapsed to a single enclosing interval (sound, possibly wide).
let div (a: I) (b: I) = divList a b |> hullOf

let square (a: I) =
    if isEmpty a then
        empty
    elif a.Lo >= 0.0 then make (mulD a.Lo a.Lo) (mulU a.Hi a.Hi)
    elif a.Hi <= 0.0 then make (mulD a.Hi a.Hi) (mulU a.Lo a.Lo)
    else make 0.0 (max (mulU a.Lo a.Lo) (mulU a.Hi a.Hi))

let sqrt (a: I) =
    let a = intersect a nonNegative // sqrt is real only on [0, inf)

    if isEmpty a then
        empty
    else
        let sd (x: float) =
            let r = Math.Sqrt x
            if finite x && finite r && Math.FusedMultiplyAdd(r, r, -x) = 0.0 then r else down r

        let su (x: float) =
            let r = Math.Sqrt x
            if finite x && finite r && Math.FusedMultiplyAdd(r, r, -x) = 0.0 then r else up r

        make (sd a.Lo) (su a.Hi)

let exp (a: I) =
    // Clamped to [0, inf): widening outward can otherwise push the lower bound
    // of exp(-inf) to a negative denormal, which is sound but nonsensical.
    if isEmpty a then
        empty
    else
        intersect (make (looseD (Math.Exp a.Lo)) (looseU (Math.Exp a.Hi))) nonNegative

let log (a: I) =
    let a = intersect a nonNegative // log is real only on (0, inf)

    if isEmpty a then
        empty
    else
        let lo = if a.Lo = 0.0 then Double.NegativeInfinity else looseD (Math.Log a.Lo)
        make lo (looseU (Math.Log a.Hi))

let abs (a: I) =
    if isEmpty a then
        empty
    elif a.Lo >= 0.0 then a
    elif a.Hi <= 0.0 then neg a
    else make 0.0 (max (-a.Lo) a.Hi)

let minI (a: I) (b: I) =
    if isEmpty a || isEmpty b then empty else make (min a.Lo b.Lo) (min a.Hi b.Hi)

let maxI (a: I) (b: I) =
    if isEmpty a || isEmpty b then empty else make (max a.Lo b.Lo) (max a.Hi b.Hi)

/// Integer power, exact about the sign rules that make even powers non-negative.
let rec powInt (a: I) (n: int) : I =
    if isEmpty a then empty
    elif n = 0 then point 1.0
    elif n = 1 then a
    elif n < 0 then div (point 1.0) (powInt a (-n))
    elif n % 2 = 0 then
        let pd (x: float) = down (Math.Pow(x, float n))
        let pu (x: float) = up (Math.Pow(x, float n))
        if a.Lo >= 0.0 then make (pd a.Lo) (pu a.Hi)
        elif a.Hi <= 0.0 then make (pd a.Hi) (pu a.Lo)
        else make 0.0 (max (pu a.Lo) (pu a.Hi))
    else
        // Odd powers are monotonically increasing.
        make (down (Math.Pow(a.Lo, float n))) (up (Math.Pow(a.Hi, float n)))

/// Rational power. Non-integer exponents are only real on [0, inf), which is
/// enforced here rather than silently producing NaN.
let powRat (a: I) (r: Rational.Rat) : I =
    if Rational.isInt r then
        powInt a (Rational.toInt r)
    else
        let a = intersect a nonNegative

        if isEmpty a then
            empty
        else
            let e = Rational.toFloat r
            let lo = looseD (Math.Pow(a.Lo, e))
            let hi = looseU (Math.Pow(a.Hi, e))
            if e >= 0.0 then make lo hi else make hi lo

/// Widen by two ULPs on each side. Used at the few places where a result comes
/// out of a library routine whose rounding we cannot certify, so that the
/// enclosure guarantee still holds.
let widen (a: I) =
    if isEmpty a then a else make (down (down a.Lo)) (up (up a.Hi))

/// The real n-th root, keeping the sign for odd n. Rounded outward by the
/// caller via `widen`.
let signedNthRoot (x: float) (n: int) =
    if n = 0 then Double.NaN
    elif x >= 0.0 then Math.Pow(x, 1.0 / float n)
    else -(Math.Pow(-x, 1.0 / float n))

// -------------------------------------------------------------- bisection --

/// Split an interval for branch-and-prune search. Unbounded sides are split at
/// a finite point so that search always makes progress.
let bisect (a: I) =
    if isEmpty a then
        empty, empty
    else
        let m =
            if isBounded a then mid a
            elif finite a.Lo then a.Lo + Math.Max(1.0, Math.Abs a.Lo)
            elif finite a.Hi then a.Hi - Math.Max(1.0, Math.Abs a.Hi)
            else 0.0

        make a.Lo m, make m a.Hi

// ----------------------------------------------------------------- output --

let private fmtNum (x: float) =
    if Double.IsPositiveInfinity x then "+inf"
    elif Double.IsNegativeInfinity x then "-inf"
    elif x = 0.0 then "0"
    else
        let a = Math.Abs x
        if a >= 1e-4 && a < 1e9 then
            let s = x.ToString("0.#####", Globalization.CultureInfo.InvariantCulture)
            if s = "-0" then "0" else s
        else
            x.ToString("0.#####e+0", Globalization.CultureInfo.InvariantCulture)

let toString (a: I) =
    if isEmpty a then "(empty)"
    elif isPoint a then fmtNum a.Lo
    else sprintf "%s .. %s" (fmtNum a.Lo) (fmtNum a.Hi)
