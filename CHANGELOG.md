# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Added

- `calc/vmap-dimension` to calculate the dimension of vmaps (like `dna-dimension`) or get it from meta `:dim` cached by a vmap constructor (not for zero-dimension vmaps)
- `expr/permute-vars` to generate all permutations of a variable ordering
- `expr/formDNA-perspectives` as a complement to `calc/dna-perspectives` which does the same thing but while transforming a formDNA expressions into an arrangement of formDNA expressions as its perspectives (keeping variable names)
- `io/print-dna` and `io/print-const` functions such that formDNA and constants not wrapped in expressions can be printed properly in formula notation
- `calc/dna-get` as a convenience function to quickly get a specific value from a dna (also because it isn’t straightforward that `filter-dna` can do the same thing with a little more effort)
- `calc/vmap-perspectives` to complement `calc/dna-perspectives` (which it takes as input)
- New operator `:tsds` provides a syntactic shortcut to build triple selective decision systems with binary selections. The 3 terms can be arbitrary expressions. Its FORMula notation looks like this: `[:tsds 100100 L,E,R]`
- New `formform.emul` module to (declaratively) represent, compose and calculate cellular automata with FORM logic. The API is mostly complete, but small changes may still happen. An introduction and more detailled explanation is in progress.
- `nuim-code` and `nmui-code` definitions in the API for convenience
- `rand-const`, `rand-dna` and `rand-vpoint` can now all take a seed for reproducability in randomization
- Added `rand-const-weighted`, `rand-dna-weighted` and `rand-vpoint-weighted` variants, which take a weights argument for uneven probability in random constant generation
- `eval→val`/`==>` and `eval→val-all`/`==>*` are two new functions in the `expr` module. The former only returns simple values, including “holes” `:_`. The latter returns only complex values, i.e. raw formDNA (possibly with holes). In essence, they return output that can be processed by functions in the `calc` as well as the new `emul` module.
- `eval-tsds->val-all` behaves just like `==>*`, but takes a 6-digit binary selection for a TsDS expr., which is especially convenient for `emul` functions

### Changed

- Changed constant/formDNA to lowercase in FORMula notation as well as in formform syntax. It is now much more pleasent to read than with uppercase and more aligned with the conventions in mathematical texts such as uFORM iFORM and LoF, which was the primary reason for this breaking change. Please also read about the change to formDNA notation below that went along with this one.
- Reversed the order of formDNA, which now reads left-to-right instead of right-to-left. The main reason for this change is to make formDNA more intuitive in reading and writing and reduce the mental load to reverse the order in your head. I apologize for this breaking change if you were already using formDNA; there will most certainly be no further changes to the notation. It went along with the new lowercase convention for constants, so that your existing formDNA data will not suddenly become incorrect and instead throws an exception, so you can safely make the transition. 
- Removed 0-arity in `rand-vpoint` because the new random functions don’t return an infinite lazy seq (I believe that has never been useful anyway).
- `rand-dna` doesn’t take an `elems` vector anymore to select from, which could have arbitrary elements. Its primary function is now better served by the `-weighted` variant and this ensures that it always returns a valid formDNA.
- `expr/mark-exprs` is deprecated to be replaced by `expr/form-marked` and `expr/make-marked` (the “unmarked” variant), to avoid the need for an options map.
- `expr/nest-exprs` is deprecated to be replaced by `expr/form-nested-l`, `expr/form-nested-r` (l/r distinguishes the nesting direction) and `expr/make-nested-l`, `expr/make-nested-r` (the “unmarked” variants), again to avoid the need for an options map.
- `expr/simplify-expr-chain` is deprecated to be replaced by `expr/simplify-nested-l` and `expr/simplify-nested-r` (l/r distinguishes, again, the nesting direction). This is in line with the change above.
- `expr/eval->expr-all` (alias `expr/=>*`), `expr/eval-all`, `calc/permute-dna`, `calc/dna-perspectives`, `calc/vdict` and `calc/dna->vdict` now take `opts` as their last argument instead of the first one. This is to make argument order of the options map consistent with other functions in formform and with the convention in the Clojure community.
- `expr/eval→expr` / `expr/=>` now returns the simplified expression instead of a hole `:_` for uninterpretable input (such as unregistered symbols or uninterpreted variables). Likewise, `expr/eval→expr-all` / `expr/=>*` returns the pre-simplified, uninterpreted expression if it could not be determined to a value in any of its interpretations. It will also just return simple values instead of wrapping them in a formDNA expr. There are some new options explained in the docstring.
- `evaluate` now separates output values in `:result` and the simplified expression in `:simplified`. The result is now `nil` if it could not be determined to a value. `eval-all` now by default returns a map for `:results` instead of a seq (the option `:ordered-results?` can switch it back if order matters). Results are value constants or `nil`. Additional options are explained in the docstring.


### Fixed

- Macros not recognized in ClojureScript
- `calc/permute-dna-seq`, which had an incorrect implementation and lack of testing
- `calc/filter-dna` to handle the edge-case where formDNA dimension is 0, i.e. the dna is equivalent to a constant
- `calc/dna-perspectives` now retains its original key-order in metadata after being converted to a map, which is useful if the order in which perspectives are listed matters

## [0.1.0] - 2023-05-07

First alpha release of the rewrite on Clojars. I advice against using it in any serious manner just yet, as there are still some rough edges, missing tests, unresolved issues, undecided questions and maybe also some outdated documentation.

However, you are welcome to experiment with the library (take a look at the [introduction](https://formform.dev/notebooks/introduction.html) first) and I am thankful for feedback of any kind, as this is my first real Clojure project.
