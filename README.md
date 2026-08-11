<a name="readme-top"></a>

# Plimsoll

https://bell-kevin.github.io/plimsoll/

**A spreadsheet makes you invent a number for everything you only know a range for, then computes one confident answer from your inventions. Plimsoll computes what is still possible from the ranges you actually know.**

You write *relations* between quantities, with units, and you write **ranges instead of numbers** — because a cache hit rate is "82 to 94 percent", not 88. Plimsoll then answers four questions, and only the first is one a spreadsheet can answer at all:

1. **What is every quantity's feasible envelope?** Guaranteed bounds, not samples.
2. **Is this model even possible?**
3. **If not, which assumptions collide?** The smallest set that still contradicts.
4. **Which assumption is costing me the most precision?** — i.e. what should I go and measure.

Relations run in **every direction**. Constrain an output and the inputs narrow. Nothing is an "input" or an "output"; there are only quantities and the relations between them.

> A Plimsoll line is the mark on a ship's hull showing how heavily it may safely be loaded. This tool draws that line around a calculation.

*Licence: AGPL-3.0-or-later. Every dependency is free software; in fact the engine has no package dependencies at all.*

![The browser build: a model on the left, its solution on the right, with the assumptions the model itself narrowed marked in orange](docs/screenshot.png)

---

## The thing it does that other tools don't

Here is a real capacity model. Note the last relation: it is a **budget**, not a result.

```plim
given peak_users        = 400_000 .. 600_000 [user]
given req_per_user      = 0.3 .. 1.2 [request/s/user]
given cache_hit_rate    = 82 .. 94 [%]
given req_per_core      = 180 .. 240 [request/s/core]
given cores_per_server  = 32 [core/server]
given failover_headroom = 1.35 .. 1.6
given cost_per_server   = 220 .. 260 [USD/month/server]

offered_load = peak_users * req_per_user
origin_load  = offered_load * (1 - cache_hit_rate)
cores_needed = origin_load * failover_headroom / req_per_core
servers      = cores_needed / cores_per_server
monthly_cost = servers * cost_per_server

monthly_cost <= 800 [USD/month]
```

```
  ASSUMED
    peak_users         400,000 .. 600,000  user
    req_per_user             0.3 .. 0.862  request/s/user
    cache_hit_rate            82.76 .. 94  %
    ...

  IMPLIED
    origin_load           7,200 .. 20,687  request·s⁻¹   █████░░░░░ ±48%
    servers                1.266 .. 3.636  server        █████░░░░░ ±48%
    monthly_cost             278.4 .. 800  USD/month     █████░░░░░ ±48%

  WHAT TO MEASURE NEXT
    pinning req_per_user       removes  80% of the uncertainty in monthly_cost
    pinning cache_hit_rate     removes  77% of the uncertainty in offered_load
```

Look at the **ASSUMED** table. You wrote `cache_hit_rate = 82 .. 94`, and Plimsoll reports `82.76 .. 94`. It pushed the budget backwards through five relations and worked out that **a hit rate below 82.76% cannot be afforded**. You never asked it to solve for the hit rate. There is no "goal seek" here, and no solver to configure — it is simply what the relations mean.

`monthly_cost` was never given a unit either. Plimsoll inferred that it is money per time, and chose to report it in USD/month because that is the unit your model speaks in.

### When a model is impossible, it says which lines are to blame

```
  IMPOSSIBLE  ·  no assignment satisfies this model

  THE SMALLEST SET OF ASSUMPTIONS THAT STILL CONFLICTS

    line 16   given cac = 900 .. 1,400 USD/customer
    line 17   given monthly_price = 40 .. 60 USD/month/customer
    line 18   given gross_margin = 72 .. 82 %
    line 19   given monthly_churn = 2.5 .. 4 %/month
    line 23   lifetime = 1 / monthly_churn
    line 25   ltv           = monthly_price * gross_margin * lifetime
    line 26   ltv_cac_ratio = ltv / cac
    line 34   ltv_cac_ratio >= 3

    Every one of these is load-bearing: drop any single one and the
    model becomes satisfiable again.
```

That model also contains two `payback` relations. They are **absent** from the report, because they are satisfiable and play no part in the contradiction. The set is minimised by deletion filtering, so nothing irrelevant survives to distract you.

---

## Why the bounds are trustworthy

Plimsoll's arithmetic is **sound**: the interval it reports is guaranteed to contain the true result. A Monte Carlo tool tells you where a million samples happened to land; it cannot tell you that nothing lies outside.

Getting this right on IEEE-754 requires care, since .NET exposes no hardware rounding mode. Every result is therefore rounded outward by one ULP — **but only when the operation was genuinely inexact**, which is detected exactly:

- `+` and `-` use Knuth's TwoSum: the residual is zero exactly when the sum is representable.
- `*`, `/` and `sqrt` use fused-multiply-add residuals.
- Transcendentals widen by two ULPs, since the platform libm is not certified correctly-rounded.

So `[2,2] + [2,2]` stays exactly `[4,4]`, while `[1,1] / [3,3]` correctly straddles ⅓. The test suite verifies enclosure by sampling 4,800 random points through non-trivial expressions and asserting every one lands inside the reported envelope.

---

## How it works

Four pieces, each doing something a spreadsheet has no equivalent for.

**1. Dimensional inference — not just checking.** Dimensions form a free abelian group: a vector of rational exponents over base names, where multiplication adds vectors and `^r` scales them. Every sum and every relation forces two dimension vectors to be equal, giving one linear equation per site:

```
Σ_v c_v · dim(v) + known = 0
```

The coefficient matrix is *the same for every base dimension* — only the residual differs. So a single exact rational Gaussian elimination solves for length, mass, time, money and everything else simultaneously. Inconsistent systems are dimension errors naming the lines that collide; underdetermined ones leave free variables, which default to dimensionless **with a warning** rather than a silent guess. This is why `force = mass * accel` needs no annotation: the engine works out kg·m·s⁻² and then reports it as newtons.

**2. HC4-revise makes relations bidirectional.** Each relation is normalised to `f(x) ∈ target` and compiled to a flat tape. Contraction is two walks: evaluate bottom-up, intersect the root with the target, then walk backwards inverting each operation — `z = x+y` gives `x ∈ z−y`, `z = x·y` gives `x ∈ z/y`. Dividing by an interval straddling zero yields *two* disjoint pieces, and intersecting each separately with the operand's current domain preserves information that hulling them away would destroy. Constraints are revised in an AC-3 style queue until nothing changes.

**3. Constructive interval disjunction sharpens the result.** Propagation handles each relation in isolation, so it cannot see that quantities trade off. With `x + y = 4` and both in `[1,3]`, propagation reports `x·y ∈ [1,9]`; the truth is `[3,4]`, because the extremes are not simultaneously reachable. Slicing one variable into *k* pieces, solving each, and hulling the survivors makes the correlation visible — and costs `k · vars · rounds`, linear rather than the exponential cost of branch-and-prune bisection. Only refuted slices are ever discarded, so the result stays a guaranteed enclosure. (Replacing bisection with this cut a benchmark from 264 ms to 13 ms.)

**4. Minimal conflict sets explain impossibility.** Remove one assumption; if the model is *still* impossible, that assumption was never needed, so drop it permanently. Because widening a range and deleting a relation both only ever make a model easier to satisfy, infeasibility is monotone and the filter is sound. What survives is irreducible.

References: Benhamou, Goualard, Granvilliers & Puget, *Revising hull and box consistency* (ICLP 1999); Trombettoni & Chabert, *Constructive interval disjunction* (CP 2007).

---

## Why F#

The engine is ~2,600 lines of F# with **zero package dependencies**.

Algebraic data types and exhaustive pattern matching are load-bearing rather than decorative here: an expression tape, a dimension term, an extended-division result that may be one interval or two — each is a closed set of cases the compiler forces you to handle. Exact rational arithmetic for dimension exponents means `sqrt(area)` gives `m` and not `m^0.9999999`. And F#'s own heritage of first-class units of measure is what suggested taking dimensions seriously in the first place.

The browser build runs the *same compiled assembly* on the .NET WebAssembly runtime, so `Math.FusedMultiplyAdd` behaves identically and the soundness guarantee survives the trip to the client. A transpile-to-JavaScript approach would have quietly lost it.

---

## Prior art, and what is actually new

I checked these before building, and confirmed each site was live.

| | forward | backward | units | sound bounds | explains impossibility |
|---|---|---|---|---|---|
| Spreadsheets | ✓ | ✗ | ✗ | ✗ | ✗ |
| [Guesstimate](https://www.getguesstimate.com/), [Squiggle](https://www.squiggle-language.com/) | ✓ | ✗ | ✗ | sampled | ✗ |
| [Numbat](https://numbat.dev/), Qalculate | ✓ | ✗ | ✓ | ✗ | ✗ |
| [Warth's constraint spreadsheet](https://alexwarth.github.io/projects/constraint-based-spreadsheet/) | ✓ | ✓ | ✗ | ✗ | detects only |
| [IBEX](https://ibex-team.github.io/ibex-lib/), RealPaver | ✓ | ✓ | ✗ | ✓ | ✗ |
| **Plimsoll** | ✓ | ✓ | ✓ | ✓ | ✓ |

None of the individual ingredients is new, and I make no claim otherwise. Interval constraint propagation is decades of solid research, and IBEX and RealPaver are its excellent C++ embodiments — but they are *libraries for solver authors*, with no units, no authoring language, and no answer to "why is this impossible?". Guesstimate and Squiggle brought uncertainty to ordinary estimation, but only forwards, and by sampling. Warth's prototype made a spreadsheet multi-directional, over point values.

The closest thing to an *interactive* interval constraint solver I could find is Brandeis' IAsolver. Its page still returns HTTP 200, but it is a Java `<applet>` — a plugin technology no browser has been able to run for roughly a decade. Reachable is not the same as usable.

What is new is the combination: **rigorous multi-directional interval solving, with dimensional inference, that explains its own contradictions, in a language a non-specialist can write.**

---

## Install and run

Requires the [.NET SDK 10](https://dotnet.microsoft.com/download) or newer (MIT licensed). Nothing else.

```bash
git clone https://github.com/YOUR-USERNAME/plimsoll
cd plimsoll

dotnet run --project tests/Plimsoll.Tests          # 119 tests, no framework needed
dotnet build src/Plimsoll.Cli -c Release

./src/Plimsoll.Cli/bin/Release/net10.0/plimsoll examples/datacenter.plim
```

Or install it as a tool:

```bash
dotnet pack src/Plimsoll.Cli -c Release
dotnet tool install --global --add-source src/Plimsoll.Cli/bin/Release Plimsoll.Cli
plimsoll examples/rocket.plim
```

```
plimsoll <model.plim> [options]
plimsoll -                    read the model from standard input

--json               machine-readable output
--svg <path>         write the feasible region as SVG
--tolerance <x>      relative width to refine to (default 0.001)
--cells <n>          feasible-region grid resolution (default 44)
--no-sensitivity     skip the "what to measure next" pass
--no-colour          disable ANSI colour (or set NO_COLOR)
```

**Exit status is meaningful: `0` satisfiable, `2` impossible, `1` unreadable.** So a `.plim` file works as an executable assertion — put one in CI and a design that drifts outside its own limits fails the build.

### The examples

| File | What it shows |
|---|---|
| `examples/datacenter.plim` | A budget pushed backwards into a required cache hit rate |
| `examples/rocket.plim` | Tsiolkovsky's equation read in reverse, solving for mass ratio through a `log` |
| `examples/heat-pump.plim` | A circuit limit narrowing what the building fabric is allowed to be |
| `examples/timber-joist.plim` | A serviceability check where *both sides* of the inequality are ranges |
| `examples/saas-unit-economics.plim` | A deliberately impossible plan, and the minimal explanation |

### The browser build

```bash
dotnet workload install wasm-tools        # once
dotnet run --project src/Plimsoll.Web     # then open the printed URL
```

The solver runs **client-side**, on the .NET WebAssembly runtime. There is no
backend, no API and no telemetry; your model never leaves the machine, and
`dotnet publish src/Plimsoll.Web -c Release` produces a directory of static files
you can host anywhere. A workflow in `.github/workflows/pages.yml` deploys it to
GitHub Pages — enable *Settings → Pages → Source: GitHub Actions*.

This is also why the browser build is a Blazor shell around the F# engine rather
than a transpile to JavaScript: it executes the *same compiled assembly*, so
`Math.FusedMultiplyAdd` and `Math.BitIncrement` behave exactly as they do on the
command line and the soundness guarantee survives the trip to the client. There
is no JavaScript equivalent of a fused multiply-add, so a transpiled build would
have quietly lost the exactness detection that keeps bounds tight.

---

## The language

Full reference in [docs/language.md](docs/language.md). It is small:

```plim
dimension money             # a new base dimension
unit USD = money            # a unit of it
unit request                # shorthand: fresh dimension *and* its base unit
unit kUSD = 1000 [USD]      # a scaled unit

given price   = 40 .. 60 [USD]     # a range
given g       = 9.80665 [m/s^2]    # an exact value
unknown thrust [N]                 # declared, unbounded, dimension fixed
                                   # (undeclared names in relations are created
                                   #  automatically, dimension inferred)

revenue = price * volume           # a relation, not an assignment
margin >= 30 [%]                   # =, <=, >= all allowed
plot price, volume                 # name two axes for the feasible region
```

Units go in `[brackets]` wherever a number needs one. `sqrt exp log abs min max` are available, and exponents must be exact rationals (`x^2`, `x^-1`, `x^(1/2)`) because that is what dimensional soundness requires.

---

## Honest limitations

- **Sound, not always tight.** HC4 is weakened when a variable occurs several times in one relation (the classic dependency problem: it cannot know that `x - x` is zero). Bounds are always valid enclosures; they are not always the tightest possible.
- **Equalities usually cannot be *certified*.** An equality's solution set has no volume, so no box fits inside it. Plimsoll distinguishes `FEASIBLE` (a region was proven to satisfy every relation) from `CONSISTENT` (no contradiction found), and never conflates them.
- **Strict inequalities are relaxed.** A closed interval cannot exclude its own endpoint, so `<` becomes `<=`, with a warning.
- **No offset units.** Celsius and Fahrenheit are absent by design: a unit here is a pure scaling, which is what makes `a*b` and `x^(1/2)` meaningful. Use kelvin.
- **Sensitivity is a width-reduction measure**, not a variance decomposition. It answers "what would pinning this buy me?", which is usually the question — but it is not Sobol indices.
- **Underdetermined dimensions default to dimensionless**, with a warning. Annotate to be sure.

---

## Layout

```
src/Plimsoll.Core/     the engine, no dependencies
  Rational.fs          exact rationals for dimension exponents
  Dimension.fs         the dimension algebra
  Interval.fs          sound interval arithmetic
  Lexer.fs Parser.fs   the .plim language
  Units.fs             SI, prefixes, user-declared dimensions
  Dimensional.fs       inference by rational Gaussian elimination
  Contractor.fs        HC4-revise
  Solver.fs            propagation, sharpening, conflicts, sensitivity
src/Plimsoll.Present/  rendering: terminal, JSON, SVG, HTML (shared)
src/Plimsoll.Cli/      the command line
src/Plimsoll.Web/      browser shell (runs the same assembly on WebAssembly)
tests/Plimsoll.Tests/  119 tests, hand-rolled harness
examples/              worked models, also served by the web build
```

The CLI and the browser share one renderer, so there is a single definition of
what a solution looks like rather than two that drift apart.

## Contributing

Issues and patches welcome. `dotnet run --project tests/Plimsoll.Tests` must stay green, and new behaviour needs a test. Please keep `Plimsoll.Core` dependency-free.

## Licence

Copyright (C) 2026 Plimsoll contributors.

Licensed under the **GNU Affero General Public License, version 3 or later**. This is free software: you may use, study, share and modify it. If you run a modified version as a network service, the AGPL requires you to offer its source to your users. See [LICENSE](LICENSE).

https://bell-kevin.github.io/plimsoll/

<p align="left"><a href="#readme-top">back to top</a></p>
