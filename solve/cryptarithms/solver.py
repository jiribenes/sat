#!/usr/bin/env python3

from timeit import default_timer as timer
from z3 import *
import re
import argparse

def solve(s, distinct, dump):
    start = timer()
    solver = Solver()

    # parse the individual words
    allWords = re.findall(r'\b[A-Z]\w*\b', s)
    allLetters = "".join(allWords)
    
    # variables for words
    words   = { word   : Int(word)   for word   in allWords   }
    # variables for letters
    letters = { letter : Int(letter) for letter in allLetters }

    for _, var in letters.items(): # letters are between 0 and 9, inclusive
        solver.add(0 <= var)
        solver.add(var <= 9)

    if distinct: # if distinct, propagate to Z3
        solver.add(Distinct(*letters.values()))

    for word in words.keys(): # the first letter in a word is never 0
        first_letter = word[0]
        solver.add( letters[ first_letter ] != 0 )

    for word, var in words.items(): # convert a word to a decimal value
        indicesAndVars = enumerate(reversed([letters[l] for l in list(word)]))
        condition = Sum(*[ var * 10 ** index for index, var in indicesAndVars ])

        solver.add( var == condition )

    # Here is a lovely hack:
    # we use python's `eval` so that we don't have to parse :)
    solver.add(eval(s, None, words))

    if dump:
        with open("dump.smt", "w") as f:
            f.write(solver.sexpr())

    result = solver.check()
    end = timer()
    print(result)
    if result == sat:
        print(solver.model())
    print(f"took {end - start} seconds")

def main():
    # argument parsing
    parser = argparse.ArgumentParser()
    parser.add_argument('--distinct', default=False, action='store_true', help='Consider only distinct values?')
    parser.add_argument('--smtlib', default=False, action='store_true', help='Dump smtlib representation')
    parser.add_argument('input', metavar='INPUT', type=argparse.FileType('r'), default=sys.stdin, help='Input file to be read (stdin by default)')
    args = parser.parse_args()

    solve(args.input.read(), args.distinct, args.smtlib)

if __name__ == '__main__':
    main()
