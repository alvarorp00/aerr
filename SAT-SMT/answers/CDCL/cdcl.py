#!/usr/bin/env python3

from z3 import Solver, Bool, Or, Not, sat

# Let us create a solver
s = Solver()

# We create a list 'p' with the propositional symbols p1, p2, ..., p6
# Since we do not have a p0 in our set, and list indices in Python start
# with 0, we insert a first dummy element in our list

p = []
p.append(None) # 0th element
for i in range(1, 7):
    p.append(Bool(f'p_{i}'))

# Use append() method to assert formulas

s.append(Not(p[5]))                     # Clause 1
s.append(Or(p[5], Not(p[6])))           # Clause 2
s.append(Or(p[1], p[2]))                # Clause 3
s.append(Or(Not(p[1]), p[3]))           # Clause 4
s.append(Or(Not(p[3]), p[2]))           # Clause 5
s.append(Or(Not(p[2]), p[4]))           # Clause 6
s.append(Or(Not(p[4]), Not(p[2])))      # Clause 7

# Now we call check() to check satisfiability. It returns
# the string "sat", "unsat" or "unknown"
result = s.check()

if result == sat:
    print("Set is satisfiable")
    print("Model: ", s.model())
else:
    print("Set is unsatisfiable")


