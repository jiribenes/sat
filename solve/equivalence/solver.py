#!/usr/bin/env python3

from timeit import default_timer as timer
from z3 import *
import argparse
from collections import defaultdict

# Unfortunately, Z3 does not allow to copy existing CNFs.
# So we just serialize and deserialize the object.
def copy_solver(s):
    assump = s.assertions()
    new_solver = Solver()
    for clause in assump:
        new_solver.add( clause )
    return new_solver

# First method: uses Tseitin encoding to negate phi and attaches it to the existing formula in solver 's'
def first(s, phi):
    newClauses = []
    clauseClause = []
    for i, clause in enumerate(phi):
        gate = Bool(f"c_{i}")          # c_i represents the clause C_i in a Tseitin-like manner
        clauseClause.append(Not(gate)) # we'll have one clause where all c_i are negated

        for lit in clause.children():
            newClauses.append(Or(Not(lit), gate)) # -lit \/ gate == lit -> gate
    
    newClauses.append(Or(*clauseClause))
    s.add( And(*newClauses) )

    result = s.check()
    return result == unsat

# Second method: asserts the negation of every clause and repeatedly calls a sat solver
def second(s, phi):
    for clause in phi:
        s1 = copy_solver(s)
        for lit in clause.children():
            s1.add( Not(lit) )      # assert negation
           
        result = s1.check()
        if result == sat:
            return False
    return True

# Checks an implication stored in the two files using the function 'f'.
# For 'f', use either 'first' or 'second'.
def implies(f, file1, file2):
    solver1 = Solver()
    solver1.from_file(file1)
    assump = solver1.assertions()
    
    solver2 = Solver()
    solver2.from_file(file2)

    return f(solver2, assump)

# Takes two files and decides if there are any implications between them.
# Calls implies twice with 'f' which determines if we use 'first' or 'second' method.
def decide(f, file1, file2):
    start = timer()
    ltr = implies(f, file1, file2)
    rtl = implies(f, file2, file1)
    end = timer()

    if ltr and rtl:
        print("equivalent")
    elif ltr:
        print("left to right implication")
    elif rtl:
        print("right to left implication")
    else:
        print("none")

    print(f"took {end - start} seconds")
    return end - start
    
def main():
    # argument parsing
    parser = argparse.ArgumentParser()

    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--first', action='store_true')
    group.add_argument('--second', action='store_true')

    parser.add_argument('left', metavar='LEFT', type=str, help='Left input file to be read (requires *.dimacs file ending!)')
    parser.add_argument('right', metavar='RIGHT', type=str, help='Right input file to be read (requires *.dimacs file ending!)')

    args = parser.parse_args()
    print(args)
    if args.first and not args.second:
        return decide(first, args.left, args.right)
    elif not args.first and args.second:
        return decide(second, args.left, args.right)

def benchmark_group(group1, group2):
    times = defaultdict(list)

    for i in range(1, 25 + 1):
        for j in range(1, 25 + 1):
            left  = f"data/{group1}/{group1}-0{i}-fixed.dimacs"
            right = f"data/{group2}/{group2}-0{j}-fixed.dimacs"

            times["first"].append(decide(first, left, right))
            times["second"].append(decide(second, left, right))

    return times

GROUPS = ["uf20", "uf50", "uf75"]

def benchmark():
    for group1 in GROUPS:
        for group2 in GROUPS:
            for (method, times) in benchmark_group(group1, group2).items():
                print(group1, group2, method, sum(times) / 625.0) # 25 * 25

def preprocess_files():
    for group in GROUPS:
        for i in range(1, 25 + 1):
            file = f"data/{group}/{group}-0{i}.dimacs"
            file_fixed = f"data/{group}/{group}-0{i}-fixed.dimacs"
            with open(file, "r") as inp:
                with open(file_fixed, "w") as outp:
                    while True:
                        line = inp.readline()
                        if not line:  # break on EOF
                            break

                        if line.startswith("%"): # break when we start reading the '%' garbage, there is nothing interesting afterwards
                            break

                        outp.write(line)


if __name__ == '__main__':
    # preprocess_files()
    # benchmark()
    main()
