#!/usr/bin/env python3

from timeit import default_timer as timer
import subprocess
from collections import defaultdict
import matplotlib.pyplot as plt
import csv

def run(mode, file):
    start = timer()
    subprocess.call(["cabal", "run", "sat", "--", mode, file], stdout=subprocess.DEVNULL)
    end = timer()
    return end - start

MAX = 100

def main():
    mode = "watched"
    groups = [ "uf50", "uuf50", "uf75", "uuf75", "uf100", "uuf100", "uf125", "uuf125" ]
    times = defaultdict(list)

    for g in groups:
        print(f"Currently solving {g}!")
        prepared = [f"test/data/cnf/{g}/{g}-0{n}.cnf" for n in range(1, MAX + 1)]

        for file in prepared:
            time = run(mode, file)
            times[g].append(time)

        print(g, sum(times[g]) / float(MAX))

if __name__ == '__main__':
    main()
