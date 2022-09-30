V = {'a', 'b', 'c', 'd'}  # vertex
E = {'ab', 'ac', 'bd', 'de'}  # conns

# Graph example gathered from tutorialspoint

# G = {
#     'a': ['b', 'c'],
#     'b': ['a', 'd'],
#     'c': ['a', 'd'],
#     'd': ['e'],
#     'e': ['d']
# }

# Graph from ColorGraph exercise

G = {
    'a': ['b', 'c'],
    'b': ['a', 'd'],
    'c': ['a', 'd', 'e'],
    'd': ['b', 'c', 'e', 'f'],
    'e': ['c', 'd', 'f'],
    'f': ['d', 'e']
}

N = 3  # n-colour

# Python script --> Z3 bindings

from operator import concat
import z3

solver = z3.Solver()

colours = [i for i in range(1, N+1)]

p = {}
for k in G.keys():
    # _p = [None]
    _p = []
    for c in colours:
        _p.append(z3.Bool(f'p_{k}_{c}'))  # append by node and colour
        p[k] = _p

# TODO -> solver.append(clauses)

for k in G.keys():
    solver.append(z3.Or(p[k]))

for k in G.keys():
    for i in range(0, len(p[k])):
        left = p[k][i]
        right = concat(p[k][:i], p[k][i+1:])
        nots = [z3.Not(_p) for _p in right]
        solver.append(z3.Implies(left, z3.And(nots)))

print(solver)