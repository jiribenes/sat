#!/usr/bin/env python3

from sys import argv, stderr

# Used for indexing to keep track of variables
def index(i, j, n):
    return i * n + j + 1

def generate(n):
    cnf = []

    for i in range(n): # at least one queen
        at_least_one = [index(i, j, n) for j in range(n)]
        cnf.append(at_least_one)

    for i in range(n):
        for j in range(n):
            for k in range(j):
                cnf.append([-index(i, k, n), -index(i, j, n)]) # no queens in same row
                cnf.append([-index(k, i, n), -index(j, i, n)]) #           or same column

    for i in range(n):
        for j in range(n - i):
            for k in range(j):
                cnf.append([-index(i + k, k, n), -index(i + j, j, n)])                 # no queens in same diagonal
                cnf.append([-index(n - i - k - 1, k, n), -index(n - i - j - 1, j, n)]) #           or the other diagonal
                if i > 0:
                    cnf.append([-index(k, i + k, n), -index(j, i + j, n)])
                    cnf.append([-index(n - k - 1, i + k, n), -index(n - j - 1, i + j, n)])

    vars = n * n
    clauses = len(cnf)
    print(f"p cnf {vars} {clauses}")

    for clause in cnf:
        clause_string = " ".join(str(x) for x in clause)
        print(clause_string + " 0")

def main():
    if len(argv) != 2:
        print("Invalid arguments!", file=stderr)
        print("Usage: $FILENAME N", file=stderr)
        return 1

    try:
        n = int(argv[1])
        if n < 1:
            print("The argument must be a nonzero integer", file=stderr)
        else:
            generate(n)

    except ValueError:
        print("The argument must be a number", file=stderr)
        return 1

if __name__ == '__main__':
    main()
