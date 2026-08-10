# The Plimsoll language

A `.plim` file is a list of statements, one per line, in any order that declares
things before it uses them. Comments run from `#` or `//` to end of line.

There are only six kinds of statement.

---

## `dimension` — introduce a base dimension

```plim
dimension money
```

Creates a new base dimension, independent of the seven SI ones, together with a
base unit of the same name. Use it when your model measures something physics
has no opinion about.

```plim
dimension money
unit USD = money
unit EUR = money          # same dimension, so these are interconvertible
```

## `unit` — introduce a unit

```plim
unit request              # fresh base dimension AND its base unit
unit USD = money          # a unit of an existing dimension
unit kUSD = 1000 [USD]    # a scaled unit
unit L = 0.001 [m^3]      # anything expressible as a scaled product of powers
```

The bare form `unit request` is the one you want for counting things. It creates
a *new dimension*, which means a request can never be silently added to a user
or a byte. This is the cheapest possible way to make counting mistakes into
compile errors:

```plim
unit user
unit request
given traffic = 5 [request/s/user]
```

Units are pure scalings. There is deliberately no way to define an offset scale
such as degrees Celsius, because a unit that carries an offset breaks the
algebra that makes `a*b` and `x^(1/2)` meaningful. Use kelvin.

## `given` — a quantity you know, to within a range

```plim
given price     = 40 .. 60 [USD]      # a range
given g         = 9.80665 [m/s^2]     # an exact value
given ratio     = 0.9 .. 1.1          # dimensionless
given cost      = 1 .. 2 [kUSD]       # any unit of the right dimension
```

Write the unit once, after the range: a bare lower bound inherits the unit of
the upper bound. Both bounds may carry units explicitly if you prefer, but they
must agree dimensionally.

Range bounds must be **constants**. They may contain arithmetic
(`0.5 * 88 [m/s]`) but may not refer to other quantities — a bound that depends
on something else is a relation, not a declaration.

## `unknown` — a quantity you do not know at all

```plim
unknown thrust [N]        # unbounded, but dimensionally fixed
unknown ratio             # unbounded, dimension inferred
```

You rarely need this, because **any name that appears in a relation without
having been declared is created automatically**, with its dimension inferred:

```plim
given mass  = 1200 .. 1400 [kg]
given accel = 2 .. 4 [m/s^2]
force = mass * accel      # `force` springs into being as kg·m·s⁻², shown as N
```

Use `unknown` when you want to state the unit for documentation, or to pin a
dimension the model does not determine on its own.

## Relations — the actual content

```plim
revenue = price * volume
margin >= 30 [%]
deflection <= span / 300
```

A relation is **not an assignment**. It states that two expressions stand in a
relationship, and it is used in whichever direction information is available.
All of these are the same relation, and any one of the three quantities can be
the one that gets narrowed:

```plim
revenue = price * volume
price = revenue / volume
volume * price = revenue
```

Operators are `=`, `<=` and `>=`. `<` and `>` are accepted but relaxed to their
non-strict forms with a warning, because a closed interval cannot exclude its
own endpoint.

## `plot` — name two axes

```plim
plot cache_hit_rate, servers
```

Asks for a feasible-region map over those two quantities. Each cell of a grid is
solved independently, so a cell is ruled out only when the relations genuinely
cannot hold anywhere inside it. Rendered as text by default, or to a file with
`--svg`.

---

## Expressions

```
+  -  *  /  ^        ( )        f(x)
```

Multiplication and division bind tighter than addition and subtraction;
`^` binds tightest. `·` and `×` are accepted for multiplication, so `kg·m` works.

Available functions: `sqrt`, `exp`, `log`, `abs`, `min`, `max`.
`exp` and `log` require a dimensionless argument, and will report a dimensional
conflict otherwise.

**Exponents must be exact rational constants** — `x^2`, `x^-1`, `x^(1/2)`. A
variable exponent has no fixed dimension, so it cannot be checked, and Plimsoll
rejects it rather than guessing.

### Numbers

```plim
1_000_000        # underscores group digits
2e3   1.5e-6     # scientific notation
0.25
```

### Units in expressions

A unit annotation attaches to the number it follows:

```plim
thrust = 1.2 [MN]
budget <= 800 [USD/month]
coeff = 0.9 [W/m^2/K]
```

Unit names are only recognised inside brackets. Outside them, every name is a
quantity — so a variable called `m` is your variable, not metres. If you do name
a quantity after a unit, Plimsoll warns you.

### Built-in units

SI base: `m kg g s A K mol cd`

Derived: `Hz N Pa J W C V F ohm Wh L t km mi ft inch ha min h hr day week month
year bar bit B percent % rad sr deg`

A month is a twelfth of a Julian year and a year is 365.25 days, those being the
only mutually consistent choices.

Decimal prefixes `Y Z E P T G M k h da d c m u µ n p f a z y` and binary
prefixes `Ki Mi Gi Ti Pi` apply to any unit. **An exact name always wins over a
prefix reading**, so `min` is a minute rather than a milli-inch, and `KiB` is
1024 bytes.

---

## Multi-line statements

A statement ends at the line break. To continue an expression, leave the
operator at the *end* of the line:

```plim
total = alpha +          # continues
        beta +
        gamma
```

A line that *begins* with an operator starts a new statement instead. This is
what stops a stray `-3` on the next line from silently changing the meaning of
the declaration above it.

---

## Reading the output

**`ASSUMED`** lists your `given` quantities — *after* solving. If a bound here
differs from what you wrote, the model's other relations forbade the rest of the
range. This is often the most informative part of the report.

**`IMPLIED`** lists everything else, with a precision bar and a `±%` figure.

**Status** is one of:

- `FEASIBLE` — a region was found in which every relation provably holds. A positive certificate.
- `CONSISTENT` — no contradiction was found. Models containing equalities normally land here, since an equality's solution set has no volume for a box to sit inside.
- `IMPOSSIBLE` — some quantity's domain was emptied, which is a proof.

**`WHAT TO MEASURE NEXT`** collapses each assumption to its midpoint, re-solves,
and reports how much uncertainty that removed. It is a width-reduction measure,
not a variance decomposition, and it answers the practical question: if you can
go and pin down exactly one thing, which one is worth the trip?

---

## Exit status

| code | meaning |
|---|---|
| 0 | satisfiable |
| 2 | impossible — the conflicting assumptions are printed |
| 1 | the model could not be read |

A `.plim` file is therefore an executable assertion. Committing one to CI turns
"the design still fits inside its limits" into a build step.
