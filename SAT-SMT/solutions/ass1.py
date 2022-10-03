from operator import concat
from random_word import RandomWords
import z3
r = RandomWords()  # Random words for testing

N_OPTIONS = 50
USE_RANDOM = False  # Choose words randomly, if false default words will be chosen
PRINT_WORDS = True  # Print words with mnemonics if possible, if false only sat model will be displayed

DEFAULT_WORDS = ['Undo', 'Redo', 'Cut', 'Copy', 'Paste', 'Delete', 'Select All']  # default words to check

# Select words either randomly or from defined set
options = [r.get_random_word() for _ in range(0, N_OPTIONS)] if USE_RANDOM else DEFAULT_WORDS

# split words into char arrays and delete letter repetition: chars only appear once per word
chars = [list(dict.fromkeys([c.lower() for c in option])) for option in options]

UND_ESCP = '\033[1;4m'  # underscore scape character for printing
END_ESCP = '\033[0m'  # restore default print (stop underscore when used)

solver = z3.Solver()

"""
Create the propositions by 'p_{word_index}_{correspondant_char} associated to each letter of
each word. Nested arrays: array of arrays (inner level contains p's of word letters)
"""
p = [[z3.Bool(f'p_{i+1}_{chars[i][j]}') for j in range(len(chars[i]))] for i in range(len(chars))]


"""
1ST REQUIREMENT: At least one mnemonic per word

For each word, at least one char must be true so each _p corresponds to each letter of each word:
    (abcd) : (a) v (b) v (c) v (d)
"""
for _p in p:
    solver.append(z3.Or(_p))


"""
2ND REQUIREMENT: If one is a word mnemonic, no others can be

If a letter is a mnemonic, the rest of the same word can not be, so:
    (abc): IsMnemonic(a) => ¬b^¬c [not actually using a function btw]
__left : each letter of each word that's going to be set as left part of the imply
__right : set of props of letters (each prop sets the truth of that letter to be a mnemonic)
        excluding the one of the __left part
__nots : negate each of the prop associated to the letters, each prop of this type
        is included in __right

Finally, build the clause as: (__left) => And(__nots) <equiv> (__left) => And(Not(__right))
"""
for i in range(len(p)):
    for j in range(len(p[i])):
        __left = p[i][j]
        __right = concat(p[i][:j], p[i][j+1:])
        __nots = [z3.Not(__p) for __p in __right]
        if len(__nots) > 0:
            solver.append(z3.Implies(__left, z3.And(__nots)))


"""
3RD REQUIREMENT: Same letter can't be mnemonic for multiple words

Per each word, look for this in other words and say that: be A letter in word W, A_w => ¬A'_w' ^ ¬A''_w''...
__match_set: props associated to letters found in other words
__nots: negate each of the props inside __match_set

Finally, if __nots not empty build the clause as:
    (prop_of_current_letter_current_word) => And(__nots) <equiv>
        (prop_of_current_letter_current_word) => And(Not(prop_of_current_letter_in_other_words))
"""
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


"""
Check satisfiability

If SAT -> print 'SAT'
If PRINT_WORDS:
    - From source words (without having cleaned letter repetition):
        - Find first ocurrence for mnemonic of this word
        - Concat this with code of underscore
"""
if solver.check() == z3.sat:
    print('SAT --> Model:')
    if PRINT_WORDS is True:
        __model = solver.model()  # Truth of props in prop array p
        __und_chars = {}  # Chars that must be underscored
        for __m in __model:
            __rtrv_chr = lambda __p: __p.__str__()[-1]  # from p: p_{word_idx}_{char} return {char}
            __rtrv_idx = lambda __p: int(__p.__str__()[2:-2])  # from p: p_{word_idx}_{char} return {word_idx}
            if z3.is_true(__model[__m]):  # Check if p[word_idx]['char'] is True so it's a mnemonic
                __und_chars[__rtrv_idx(__m)] = __rtrv_chr(__m)  # Insert char to be underscored
        __mnemonics = []
        for k, v in __und_chars.items():
            __word = ''
            __found = False  # Flag to assure mnemonic appears only once as _options_ array contains letter repetition
            for i in range(0, len(options[k-1])):
                if options[k-1][i].lower() == v and __found is False:
                    __word += UND_ESCP + v + END_ESCP
                    __found = True
                    __mnemonics.append(v)
                else:
                    __word += options[k-1][i]
            print(__word)
        print('', "========== MODEL ==========", '')
        print('', solver.model())
        print('', "===========================", '')
    else:
        print(solver.model())
else:
    print('UNSAT')