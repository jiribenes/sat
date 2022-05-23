# sat

## Installation / building

Tested on GHC 8.10.7 and Cabal >=3.0.
You can use Nix in order to prepare the environment,
see a more detailed tutorial [here](https://github.com/jiribenes/cocobolo/#building).
(Just subtitute `cocobolo` for `sat`.)

## Usage

```sh
# to show help
> cabal run sat -- --help

# in general
> cabal run sat -- $ARGS
```

## Reports

### Using a SAT solver

See the `solve/` folder for three examples on how to use a SAT/SMT solver
together with some reports.

### DPLL with adjacency lists

As a heuristic, we use pick literals in an increasing variable order.
Invoke with
```sh
> cabal run sat -- dpll $INPUT_FILE
```

```
class   time (seconds, avg.)
-----------------------------
uf50     0.0876223077699251
uuf50    0.10459998103964609

uf75     0.338379621450149
uuf75    0.6595959465000487

uf100    3.82715032542008
uuf100  12.215521234360349 
```

Measured using `./benchmark.py` located in the root -- 100 from each group; time is in seconds, averaged.
All measurements come from an optimized binary (compiled with `-O2`).
We can clearly see that the biggest problem are the unsatisfiable tests
as their time increases more rapidly than time of satisfiable tests.
We can also see the exponential time increase when increasing the number of variables (see the 10x jump between `uf75` and `uf100`).

### DPLL with watched literals

```sh
> cabal run sat -- watched $INPUT_FILE
```

```
class   time (seconds, avg.)
-----------------------------
uf50     0.0834373040003508
uuf50    0.0975302919165794

uf75     0.1775751472475046
uuf75    0.328277350999997

uf100    1.631737387831284
uuf100   3.4501382350827043 

uf125    5.63246564132957
uuf125  29.63107954734005
```

Compared to adjacency lists, the number of decisions is more-or-less the same
(when measured, the values are nearly indistinguishable).
However, the 2x time speedup is quite remarkable given how simple the modification is.
It even seems to scale way better into larger formulas -- `uuf100` is almost four times faster than previously
and we can even manage to benchmark formulas of 125 variables!

### CDCL (with watched literals)

```sh
> cabal run sat -- cdcl $INPUT_FILE
```

```
class   time (seconds, avg.)
-----------------------------
uf50     0.09610157199495006
uuf50    0.08600014999683481

uf75     0.07546412300143857
uuf75    0.3353654600068694

uf100    1.395625179997296
uuf100   4.7258756079972954

uf125    4.426152090993128
uuf125  67.02587713299727
```

Note that compared to DPLL, unsatisfied problemes take even longer on average than before.
Moreover, the speedup can be seen only on satisfiable problems
and is more visible with the increasing number of variables where we begin to gain some ground compared to DPLL.
We believe that the time increase for unsatisfied formulas can be attributed to the restarts
which essentially throwaway a lot of the work in hopes of finding a better path towards a sat solution.
However, the speedup is still worth it.
