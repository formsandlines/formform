# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Added

- `calc/vmap-dimension` to calculate the dimension of vmaps (like `dna-dimension`) or get it from meta `:dim` cached by a vmap constructor (not for zero-dimension vmaps)
- `expr/permute-vars` to generate all permutations of a variable ordering
- `expr/formDNA-perspectives` as a complement to `calc/dna-perspectives` which does the same thing but while transforming a formDNA expressions into an arrangement of formDNA expressions as its perspectives (keeping variable names)
- `io/print-dna` and `io/print-const` functions such that formDNA and constants not wrapped in expressions can be printed properly in formula notation

### Changed

- `=>*` aka `eval->expr-all` does now return formDNA as a proper vector that is already reversed at creation-time instead of adding performance cost with an `rseq`

### Fixed

- Macros not recognized in ClojureScript
- `calc/permute-dna-seq`, which had an incorrect implementation and lack of testing

## [0.1.0] - 2023-05-07

First alpha release of the rewrite on Clojars. I advice against using it in any serious manner just yet, as there are still some rough edges, missing tests, unresolved issues, undecided questions and maybe also some outdated documentation.

However, you are welcome to experiment with the library (take a look at the [introduction](https://formform.dev/notebooks/introduction.html) first) and I am thankful for feedback of any kind, as this is my first real Clojure project.
