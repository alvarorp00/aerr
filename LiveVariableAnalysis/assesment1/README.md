# Assesment 3 - Static Analysis and Constraint Solving | UCM 22-23

Tree view of the project:

```
.
├── app
│   └── Main.hs
├── assesment1.cabal
├── CHANGELOG.md
├── LICENSE
├── README.md
└── src
    └── Lib.hs
```

- _Main.hs_ contains the _main_ function of the program. This _main_ function calls _mainLib_ function of _Lib.hs_.
- _Lib.hs_ contains the implemented **Live Variable Analysis**. Further details at the end of this file.
- _assesment1.cabal_ contains the needed configuration for building and running the project.
- Other files such as LICENSE and CHANGELOG were generated automathically by doing `cabal init` and thus can be ignored.

## Building & Compiling

As Cabal has been used the next steps should be followed for running the program:

### Building

-> Builds the project

> `cabal build`

### Running

-> Runs the project

> `cabal run`

- Note: calls `cabal build` if project is not built on its last version

### Cleaning

-> Cleans the build of the project

> `cabal clean`

## Some comments about the implementation

This part of the README tries to provide a quick view about the structure followed while designing the 'language', by defining the structures required to satisfy the expressions shown in the statement of the exercise.