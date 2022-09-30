from operator import concat
from tabnanny import check
from tkinter import END
from random_word import RandomWords
import re
r = RandomWords()

N_OPTIONS = 50
USE_RANDOM = False
PRINT_WORDS = True

options = [r.get_random_word() for _ in range(0, N_OPTIONS)] if USE_RANDOM else ['Undo', 'Redo', 'Cut', 'Copy', 'Paste', 'Delete', 'Select All']
chars = [list(dict.fromkeys([c.lower() for c in option])) for option in options]

UND_ESCP = '\033[1;4m'
END_ESCP = '\033[0m'

import z3

solver = z3.Solver()

p = [[z3.Bool(f'p_{i+1}_{chars[i][j]}') for j in range(len(chars[i]))] for i in range(len(chars))]

for _p in p:
    solver.append(z3.Or(_p))

for i in range(len(p)):
    for j in range(len(p[i])):
        __left = p[i][j]
        __right = concat(p[i][:j], p[i][j+1:])
        __nots = [z3.Not(__p) for __p in __right]
        if len(__nots) > 0:
            solver.append(z3.Implies(__left, z3.And(__nots)))

checked_chars = set()
for i in range(0, len(p)):
    for j in range(0, len(p[i])):
        __match_set = set()
        for k in range(len(p)):
            if k == i: continue
            for l in range(len(p[k])):
                if chars[i][j] == chars[k][l]:
                    __match_set.add(p[k][l])
        __nots = [z3.Not(__p) for __p in __match_set]
        if len(__nots) > 0:
            solver.append(z3.Implies(p[i][j], z3.And(__nots)))
        checked_chars.add(chars[i][j])

res = solver.check()

if res == z3.sat:
    # source words <-- words not lowercased
    print('SAT --> Model:')
    if PRINT_WORDS is True:
        __model = solver.model()
        __und_chars = {}
        for __m in __model:
            __rtrv_chr = lambda __p: __p.__str__()[-1]
            __rtrv_idx = lambda __p: int(__p.__str__()[2:-2])
            if z3.is_true(__model[__m]):
                __und_chars[__rtrv_idx(__m)] = __rtrv_chr(__m)
        __mnemonics = []
        for k, v in __und_chars.items():
            __word = ''
            __found = False
            for i in range(0, len(options[k-1])):
                if options[k-1][i].lower() == v and __found is False:
                    __word += UND_ESCP + v + END_ESCP
                    __found = True
                    __mnemonics.append(v)
                else:
                    __word += options[k-1][i]
            print(__word)
        # print("===", __mnemonics, "===")
        print('', solver.model())
    else:
        print(solver.model())
else:
    print('UNSAT')