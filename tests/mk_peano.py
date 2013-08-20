#!/usr/bin/env python2

"""Generate a Peano problem of given size"""

import sys

def print_n(n):
    for _ in xrange(n):
        sys.stdout.write("s(")
    sys.stdout.write("zero")
    for _ in xrange(n):
        sys.stdout.write(")")

def print_op(op, n, m):
    sys.stdout.write(op+"(")
    print_n(n)
    sys.stdout.write(", ")
    print_n(m)
    sys.stdout.write(")")

def generate(n):
    sys.stdout.write("fof(goal, conjecture, ")
    print_op("plus", n, n)
    sys.stdout.write("=")
    print_op("mult", 2, n)
    sys.stdout.write(").\n")
    sys.stdout.flush()

def main():
    if len(sys.argv) < 2:
        n = 10
    else:
        n = int(sys.argv[1])
    print "%% Peano problem of size %d" % n 
    generate(n)

if __name__ == '__main__':
    main()
