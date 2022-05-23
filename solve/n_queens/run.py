#!/usr/bin/env python3

from timeit import default_timer as timer
import subprocess
from collections import defaultdict
import matplotlib.pyplot as plt
import csv

def prepare(n):
    filename = f"build/n_queens-{n}.cnf"
    with open(filename, "w") as f:
        result = subprocess.call(["./generate.py", str(n)], stdout=f)
    return filename

def run(solver, file):
    start = timer()
    subprocess.call([solver, file], stdout=subprocess.DEVNULL)
    end = timer()
    return end - start

SOLVERS = ["minisat", "glucose", "cadical"]
MAX = 100

def plot(times):
    print("Drawing!")
    for (solver, measuredTimes) in times.items():
        plt.plot(list(range(1, MAX + 1)), measuredTimes, label=solver)

    plt.xlabel("n (size of chessboard)")
    plt.ylabel("time in seconds")
    plt.legend()

    plt.savefig('n_queens.png')

def main():
    print("Generating inputs!")
    prepared = [prepare(n) for n in range(1, 101)]
    print("Generated all inputs!")

    times = defaultdict(list)

    for file in prepared:
        print(f"Currently solving {file}!")
        for solver in SOLVERS:
            time = run(solver, file)
            times[solver].append(time)

    with open('times.csv', 'w') as f:
        writer = csv.writer(f)
        for (solver, measuredTimes) in times.items():
            writer.writerow([solver] + measuredTimes)

    plot(times)

if __name__ == '__main__':
    main()
