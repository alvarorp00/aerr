# import z3

# solver = z3.Solver()

# x = z3.Int('x')
# y = z3.Int('y')

# solver.append(x>2)
# solver.append(y<10)
# solver.append(x+2*y==7)

# res = solver.check()

# print(f'Is SAT? {res == z3.sat}')

# n = x+y >= 3

# print(f"num_args: {n.decl().name()}")

from operator import concat
import z3

p = concat([None], [z3.Bool(f'p_{i}') for i in range(1,9)])

solver = z3.Solver()

solver.append(z3.Or(z3.Not(p[1]), z3.Not(p[2])))
solver.append(z3.Or(z3.Not(p[1]), p[3]))
solver.append(z3.Or(z3.Not(p[4]), z3.Not(p[3]), z3.Not(p[5])))
solver.append(z3.Or(p[2], p[5], p[6]))
solver.append(z3.Or(p[5], z3.Not(p[7])))
solver.append(z3.Or(z3.Not(p[6]), p[7]))
solver.append(z3.Or(z3.Not(p[5]), z3.Not(p[8]), z3.Not(p[3])))

result = solver.check()

if result == z3.sat:
    print("Satisfiable")
    print(f"Model: {solver.model()}")
else:
    print("Unsatisfiable")

