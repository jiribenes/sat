# Equivalence checking

We use Z3 with its Python bindings because of its percieved convenience even for SAT solving.
Unfortunately, the Z3 library was not designed to be used the way we would like to,
which might worsen the performance. Nevertheless, we should still be able to see how
the function behaves with increasing number of variables.

## Data

We used the [Uniform Random-3-SAT](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html) in the forms of uf20, uf50, and uf75
(20, 50, and 75 variables, respectively).
Unfortunately, Z3 was not able to read the files because of the `%` sign which is located at the bottom of these files.
We believe this to be a bug of the Z3 dimacs parser which might be a nice way to contribute to the project.
Moreover, Z3 requires the files to have the `*.dimacs` extension instead of the commonly used `*.cnf` extension.

Both of these problems meant that we needed to preprocess the data a bit more.

## Results

We implemented the two methods -- called `first` and `second` in the `./solver.py` script.
We created a benchmark which tests the equivalence/implication for every pair of the testing data.
As already stated, we used uf20, uf50, and uf75 datasets and took 25 formulas from each.
Then we tested them for equivalence in all possible ways, including the cases where the tested files were the same.

```
phi  psi  method  time (seconds, avg.)
--------------------------------------
uf20 uf20 first    0.12143761414
uf20 uf20 second   0.23008781167
uf20 uf50 first    0.18892307837
uf20 uf50 second   0.12359934266
uf20 uf75 first    0.24601678661
uf20 uf75 second   0.12506822943

uf50 uf20 first    0.18723410442
uf50 uf20 second   0.12333632007
uf50 uf50 first    0.25365226236
uf50 uf50 second   0.48435866951
uf50 uf75 first    0.31133132151
uf50 uf75 second   0.1470794418

uf75 uf20 first    0.24559151732
uf75 uf20 second   0.12510537752
uf75 uf50 first    0.31024809385
uf75 uf50 second   0.14664560599
uf75 uf75 first    0.37017666571
uf75 uf75 second   0.77305198237
```

Whereas the "first" method is quite slower with larger formulas, the "second" is usually only negligibly slower.
However, there is an outlier in the `50, 50, second` and the `75, 75, second` case
-- the sudden increases are very large and we haven't been able to figure out why.
Because of a lack of any other evidence, we believe that it's a repeated measurement error.
