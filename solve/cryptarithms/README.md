# Cryptarithms

We have chosen the Z3 solver together with Python in order to implement this task.

The notation accepted by the solver is Python-like:
- upper-case letters for variables (which limits it to only 26 letters)
- lower-case letters for the `or`, `and`, `not`, operators
- the `+`, `-` arithmetic operators
- `==` comparison operator
- parentheses

## Options

- `--distinct` allows distinct solutions
- `--smtlib` dumps the S-expr SMTLIB notation into a file
- the solver expects a file handle as an argument

## Examples

```sh
> ./solver.py input-long.txt # contains the long input from the problem statement

sat
[T = 9,
 A = 6,
 S = 1,
 O = 2,
 M = 1,
 Y = 0,
 E = 9,
 R = 2,
 N = 2,
 H = 8,
 TESTS = 99191,
 TEN = 992,
 OTHER = 29892,
 HAS = 861,
 HE = 89,
 MOON = 1222,
 ON = 22,
 TEAM = 9961,
 MEET = 1999,
 MAN = 162,
 ONE = 229,
 SAME = 1619,
 THE = 989,
 HEAR = 8962,
 OR = 22,
 SEE = 199,
 AS = 61,
 HOME = 8219,
 AT = 69,
 STAY = 1960,
 TRY = 920,
 SOON = 1222,
 MAY = 160,
 THEY = 9890,
 THAT = 9869,
 SAY = 160,
 TO = 92,
 SEEM = 1991,
 MEN = 192,
 MORE = 1229,
 MANY = 1620,
 SO = 12]
took 0.03931448700313922 seconds
```

```sh
> ./solver.py --distinct input-example.txt  # contains the short input from the problem statement
unsat
took 67.55617855899618 seconds
```

We believe that it's quite unfortunate that the unsatisfiable example takes so much
time to actually prove that it is indeed unsolvable.
There might be ways to make it faster -- for example by changing the settings of the solver,
but we were not successful in such endeavours as no settings changes had any real impact on the speed of this example.
