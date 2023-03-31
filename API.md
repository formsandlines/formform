# Table of contents
-  [`formform.calc`](#formform.calc)  - API for the <code>calc</code> module of <code>formform</code>.
    -  [`--`](#formform.calc/--) - Alias to <code>rel</code>.
    -  [`char->const`](#formform.calc/char->const) - Coerces a char to a corresponding constant.
    -  [`chars->dna`](#formform.calc/chars->dna) - Converts a <code>seqable?</code> of chars to formDNA.
    -  [`compare-consts`](#formform.calc/compare-consts)
    -  [`const->digit`](#formform.calc/const->digit) - Converts a constant to a digit corresponding to an optional <code>sort-code</code> or the default <code>nuim-code</code>.
    -  [`const?`](#formform.calc/const?) - Checks if the argument is a valid constant.
    -  [`consts`](#formform.calc/consts)
    -  [`consts->quaternary`](#formform.calc/consts->quaternary) - Converts a sequence of constants to a corresponding quaternary number (as a string, prefixed by '4r').
    -  [`digit->const`](#formform.calc/digit->const) - Converts a digit to its corresponding constant representation.
    -  [`digits->dna`](#formform.calc/digits->dna) - Converts a <code>seqable?</code> of digits (as string/char or integer) to formDNA.
    -  [`dna->digits`](#formform.calc/dna->digits) - Converts formDNA to a sequence of digits corresponding to a <code>sort-code</code>.
    -  [`dna->vdict`](#formform.calc/dna->vdict) - Generates a vdict from a given dna.
    -  [`dna->vmap`](#formform.calc/dna->vmap)
    -  [`dna-dimension`](#formform.calc/dna-dimension) - Calculates the dimension of a formDNA/<code>dna-seq</code> (corresponds to the number of variables in a FORM).
    -  [`dna-dimension?`](#formform.calc/dna-dimension?)
    -  [`dna-perspectives`](#formform.calc/dna-perspectives)
    -  [`dna-seq-perspectives`](#formform.calc/dna-seq-perspectives)
    -  [`dna?`](#formform.calc/dna?)
    -  [`equal-dna`](#formform.calc/equal-dna) - Equality check for formDNA.
    -  [`equiv-dna`](#formform.calc/equiv-dna) - Equivalence check for formDNA.
    -  [`expand-dna-seq`](#formform.calc/expand-dna-seq) - Expands a <code>dna-seq</code> to a given target dimension by repeating elements.
    -  [`filter-dna`](#formform.calc/filter-dna) - Filters a <code>dna</code> by selecting specific parts corresponding to a given <code>vpoint</code>, which acts as a coordinate vector in its value space.
    -  [`filter-dna-seq`](#formform.calc/filter-dna-seq)
    -  [`inv`](#formform.calc/inv) - Inverts the value of a every constant in a formDNA.
    -  [`make-compare-consts`](#formform.calc/make-compare-consts) - Given a <code>sort-code</code> (try <code>calc.nuim-code</code> or <code>calc.nmui-code</code>), returns a comparator function to sort single constants, formDNA or arbitrary sequences of constants (can be mixed).
    -  [`make-dna`](#formform.calc/make-dna) - Creates a formDNA from arguments, which may be valid chars, keywords, integers or sequences thereof.
    -  [`nmui-code`](#formform.calc/nmui-code)
    -  [`nuim-code`](#formform.calc/nuim-code)
    -  [`permute-dna`](#formform.calc/permute-dna)
    -  [`permute-dna-seq`](#formform.calc/permute-dna-seq)
    -  [`rand-const`](#formform.calc/rand-const) - Generates a random constant.
    -  [`rand-dna`](#formform.calc/rand-dna) - Generates a random formDNA/<code>dna-seq</code> of dimension <code>dim</code>.
    -  [`rand-vpoint`](#formform.calc/rand-vpoint) - Generates a random vpoint either as an infinite lazy seq or with given dimension <code>dim</code>.
    -  [`reduce-dna-seq`](#formform.calc/reduce-dna-seq) - Reduces a <code>dna-seq</code> by eliminating redundant/contingent terms.
    -  [`rel`](#formform.calc/rel) - Relates the values of 2 constants in a formDNA to each other.
    -  [`reorder-dna-seq`](#formform.calc/reorder-dna-seq) - Reorders given formDNA/<code>dna-seq</code> from <code>sort-code-from</code> to <code>sort-code-to</code>.
    -  [`reverse-dna`](#formform.calc/reverse-dna) - Reverses a formDNA (returns an rseq) - make sure the input is a vector for constant-time reverse.
    -  [`sort-code?`](#formform.calc/sort-code?)
    -  [`spec--dna-args`](#formform.calc/spec--dna-args)
    -  [`spec--dna-seq-args`](#formform.calc/spec--dna-seq-args)
    -  [`var-const`](#formform.calc/var-const)
    -  [`vdict`](#formform.calc/vdict) - Generates a vdict given a map from vpoint to result (constant).
    -  [`vdict->vmap`](#formform.calc/vdict->vmap) - Generates a vmap from a given vdict.
    -  [`vdict?`](#formform.calc/vdict?)
    -  [`vmap?`](#formform.calc/vmap?)
    -  [`vpoint?`](#formform.calc/vpoint?)
    -  [`vspace`](#formform.calc/vspace) - Generates a vspace of dimension <code>dim</code>, optionally with custom <code>sort-code</code>.
    -  [`vspace?`](#formform.calc/vspace?)
    -  [`|`](#formform.calc/|) - Alias to <code>inv</code>.
-  [`formform.expr`](#formform.expr)  - API for the <code>expr</code> module of <code>formform</code>.
    -  [`=>`](#formform.expr/=>) - Evaluates a FORM expression with an optional <code>env</code> and returns a Constant expression with attached metadata including the maximally reduced expression in <code>:expr</code> and the environment in <code>:env</code>.
    -  [`=>*`](#formform.expr/=>*) - Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression.
    -  [`arrangement?`](#formform.expr/arrangement?)
    -  [`cnt>`](#formform.expr/cnt>) - Simplifies a FORM content recursively until it cannot be further simplified.
    -  [`const->isolator`](#formform.expr/const->isolator)
    -  [`ctx>`](#formform.expr/ctx>) - Simplifies a FORM context recursively until it cannot be further simplified.
    -  [`defoperator`](#formform.expr/defoperator) - Defines a new operator by its symbol (a keyword), a vector of arguments and an interpretation function.
    -  [`defsymbol`](#formform.expr/defsymbol) - Defines a new expression symbol by its symbol (a keyword) and an interpretation function.
    -  [`equal`](#formform.expr/equal) - Equality check for expressions.
    -  [`equiv`](#formform.expr/equiv) - Equivalence check for expressions.
    -  [`eval-all`](#formform.expr/eval-all)
    -  [`evaluate`](#formform.expr/evaluate)
    -  [`expr->const`](#formform.expr/expr->const) - Given an expression, returns the corresponding constant value.
    -  [`expr-symbol?`](#formform.expr/expr-symbol?)
    -  [`expression?`](#formform.expr/expression?)
    -  [`find-subexprs`](#formform.expr/find-subexprs) - Finds all subexpressions in <code>expr</code> that match any element of the given set <code>subexprs</code>.
    -  [`find-vars`](#formform.expr/find-vars) - Finds all variables in an expresson or returns the expression itself if it is a variable.
    -  [`form`](#formform.expr/form) - Constructor for FORM expressions.
    -  [`form?`](#formform.expr/form?)
    -  [`gen-vars`](#formform.expr/gen-vars) - Generates a number of variables with random names.
    -  [`interpret`](#formform.expr/interpret) - Interprets an expression of any kind.
    -  [`interpret*`](#formform.expr/interpret*) - Like <code>interpret</code>, but repeats substitution on interpreted expressions until they cannot be interpreted any further.
    -  [`interpret-op`](#formform.expr/interpret-op) - Interprets a symbolic expression with a registered operator.
    -  [`interpret-sym`](#formform.expr/interpret-sym) - Interprets a registered symbol.
    -  [`interpret-walk`](#formform.expr/interpret-walk) - Recursively calls <code>interpret</code> on given expression and all its subexpressions with a depth-first walk.
    -  [`interpret-walk*`](#formform.expr/interpret-walk*) - Like <code>interpret-walk</code>, but repeats substitution on interpreted (sub-)expressions until they cannot be interpreted any further.
    -  [`literal-expr?`](#formform.expr/literal-expr?)
    -  [`make`](#formform.expr/make) - Constructor for expressions of any kind.
    -  [`make-op`](#formform.expr/make-op) - Constructs a symbolic expression given a registered operator and parameters.
    -  [`mark-exprs`](#formform.expr/mark-exprs) - Chains expressions like <code>((a)(b)…)</code> or <code>(a)(b)…</code> if {:unmarked? true}<code> - group expressions with arrangements: </code>[:- x y …]`.
    -  [`memory`](#formform.expr/memory) - Constructs a memory FORM.
    -  [`memory-extend`](#formform.expr/memory-extend)
    -  [`memory-replace`](#formform.expr/memory-replace)
    -  [`nest-exprs`](#formform.expr/nest-exprs) - Nests expressions leftwards <code>(((…)a)b)</code> or rightwards <code>(a(b(…)))</code> if <code>{:ltr? true}</code> - use <code>nil</code> for empty expressions - use an arrangement <code>(make x y …)</code> to add multiple exprs.
    -  [`op-data`](#formform.expr/op-data) - Gets all parameters from a symbolic expression with a registered operator as a map.
    -  [`op-get`](#formform.expr/op-get) - Gets a specified part from a symbolic expression with a registered operator.
    -  [`op-spec`](#formform.expr/op-spec)
    -  [`op-symbol`](#formform.expr/op-symbol) - Returns the symbol of an operator.
    -  [`op-symbol?`](#formform.expr/op-symbol?)
    -  [`operator?`](#formform.expr/operator?)
    -  [`pure-form?`](#formform.expr/pure-form?)
    -  [`rem-pairs?`](#formform.expr/rem-pairs?)
    -  [`sel`](#formform.expr/sel)
    -  [`seq-re`](#formform.expr/seq-re) - Constructs a self-equivalent re-entry FORM given the arguments: - <code>specs</code>: either a <code>seq-reentry-signature</code> or an options map - <code>nested-exprs</code>: zero or more expressions intended as a nested sequence.
    -  [`seq-reentry-defaults`](#formform.expr/seq-reentry-defaults)
    -  [`seq-reentry-opts->sign`](#formform.expr/seq-reentry-opts->sign) - Inverse map of seq-reentry-sign->opts with default args.
    -  [`seq-reentry-opts?`](#formform.expr/seq-reentry-opts?)
    -  [`seq-reentry-sign->opts`](#formform.expr/seq-reentry-sign->opts) - Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps.
    -  [`seq-reentry-signature?`](#formform.expr/seq-reentry-signature?)
    -  [`simplify-expr-chain`](#formform.expr/simplify-expr-chain) - Reduces a sequence of expressions, intended to be linked in a <code>chain</code>, to a sequence of simplified expressions, possibly spliced or shortened via inference.
    -  [`simplify-op`](#formform.expr/simplify-op) - Simplifies a symbolic expression with a registered operator given an optional environment.
    -  [`simplify-sym`](#formform.expr/simplify-sym) - Simplifies a registered symbol given an optional environment.
    -  [`splice-ctx`](#formform.expr/splice-ctx) - Dissolves arrangements in given context such that their elements become direct children of the context itself.
    -  [`struct-expr?`](#formform.expr/struct-expr?)
    -  [`substitute-expr`](#formform.expr/substitute-expr) - Substitutes an expression by a matching expression in given environment.
    -  [`unclear?`](#formform.expr/unclear?)
    -  [`valid-op?`](#formform.expr/valid-op?) - Validates the shape of a symbolic expression with a registered operator.
    -  [`variable?`](#formform.expr/variable?)
-  [`formform.io`](#formform.io)  - API for the <code>io</code> module of <code>formform</code>.
    -  [`parse-tree`](#formform.io/parse-tree)
    -  [`print-expr`](#formform.io/print-expr) - Given an expression, returns a string of its representation in <code>formula</code> notation.
    -  [`read-expr`](#formform.io/read-expr) - Given a string in <code>formula</code> notation, returns the corresponding data structure that can be processed by <code>formform.expr</code>.
    -  [`uniform-expr`](#formform.io/uniform-expr) - Given an expression, returns a <code>uniform</code> data structure that is a nested map with the following pattern: <code></code><code> {:type <expr-type> … :children [<uniform> …]} </code><code></code> Can be given an option map to support various customizations (see source), e.g.

-----
# <a name="formform.calc">formform.calc</a>


API for the `calc` module of `formform`.




## <a name="formform.calc/--">`--`</a><a name="formform.calc/--"></a>




Alias to [`rel`](#formform.calc/rel).
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L544-L544">Source</a></sub></p>

## <a name="formform.calc/char->const">`char->const`</a><a name="formform.calc/char->const"></a>




Coerces a char to a corresponding constant.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L167-L168">Source</a></sub></p>

## <a name="formform.calc/chars->dna">`chars->dna`</a><a name="formform.calc/chars->dna"></a>




Converts a `seqable?` of chars to formDNA.
  
Note that [`nuim-code`](#formform.calc/nuim-code) is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L318-L321">Source</a></sub></p>

## <a name="formform.calc/compare-consts">`compare-consts`</a><a name="formform.calc/compare-consts"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L222-L222">Source</a></sub></p>

## <a name="formform.calc/const->digit">`const->digit`</a><a name="formform.calc/const->digit"></a>




Converts a constant to a digit corresponding to an optional `sort-code` or the default [`nuim-code`](#formform.calc/nuim-code).
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L175-L176">Source</a></sub></p>

## <a name="formform.calc/const?">`const?`</a><a name="formform.calc/const?"></a>




Checks if the argument is a valid constant.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L148-L149">Source</a></sub></p>

## <a name="formform.calc/consts">`consts`</a><a name="formform.calc/consts"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L145-L145">Source</a></sub></p>

## <a name="formform.calc/consts->quaternary">`consts->quaternary`</a><a name="formform.calc/consts->quaternary"></a>




Converts a sequence of constants to a corresponding quaternary number (as a string, prefixed by '4r').
- use `read-string` to obtain the decimal value as a BigInt
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L181-L183">Source</a></sub></p>

## <a name="formform.calc/digit->const">`digit->const`</a><a name="formform.calc/digit->const"></a>




Converts a digit to its corresponding constant representation.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L159-L160">Source</a></sub></p>

## <a name="formform.calc/digits->dna">`digits->dna`</a><a name="formform.calc/digits->dna"></a>




Converts a `seqable?` of digits (as string/char or integer) to formDNA.
  
Note that [`nuim-code`](#formform.calc/nuim-code) is the default ordering. If a different `sort-code` is specified, `digits` will be reordered to match the code.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L308-L311">Source</a></sub></p>

## <a name="formform.calc/dna->digits">`dna->digits`</a><a name="formform.calc/dna->digits"></a>




Converts formDNA to a sequence of digits corresponding to a `sort-code`.
  
Note that [`nuim-code`](#formform.calc/nuim-code) is the default ordering. If a different `sort-code` is specified, `dna` will be reordered to match the code.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L328-L331">Source</a></sub></p>

## <a name="formform.calc/dna->vdict">`dna->vdict`</a><a name="formform.calc/dna->vdict"></a>




Generates a vdict from a given dna.
- optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L505-L507">Source</a></sub></p>

## <a name="formform.calc/dna->vmap">`dna->vmap`</a><a name="formform.calc/dna->vmap"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L530-L530">Source</a></sub></p>

## <a name="formform.calc/dna-dimension">`dna-dimension`</a><a name="formform.calc/dna-dimension"></a>




Calculates the dimension of a formDNA/`dna-seq` (corresponds to the number of variables in a FORM). The length of a `dna-seq` is 4^d for its dimension d.
- the input sequence can have any type of elements
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L239-L241">Source</a></sub></p>

## <a name="formform.calc/dna-dimension?">`dna-dimension?`</a><a name="formform.calc/dna-dimension?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L76-L76">Source</a></sub></p>

## <a name="formform.calc/dna-perspectives">`dna-perspectives`</a><a name="formform.calc/dna-perspectives"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L444-L444">Source</a></sub></p>

## <a name="formform.calc/dna-seq-perspectives">`dna-seq-perspectives`</a><a name="formform.calc/dna-seq-perspectives"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L437-L437">Source</a></sub></p>

## <a name="formform.calc/dna?">`dna?`</a><a name="formform.calc/dna?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L77-L77">Source</a></sub></p>

## <a name="formform.calc/equal-dna">`equal-dna`</a><a name="formform.calc/equal-dna"></a>




Equality check for formDNA. Two formDNAs are considered equal, if they contain the same constants in the same order. Stricter than [`equiv-dna`](#formform.calc/equiv-dna), where permutations are considered equal.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L449-L450">Source</a></sub></p>

## <a name="formform.calc/equiv-dna">`equiv-dna`</a><a name="formform.calc/equiv-dna"></a>




Equivalence check for formDNA. Two formDNAs are considered equivalent, if they belong to the same equivalence-class of [`dna-perspectives`](#formform.calc/dna-perspectives) (i.e. if they are permutations of each other).
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L455-L456">Source</a></sub></p>

## <a name="formform.calc/expand-dna-seq">`expand-dna-seq`</a><a name="formform.calc/expand-dna-seq"></a>




Expands a `dna-seq` to a given target dimension by repeating elements.

Note: `dna-seq` can have any type of elements (not only constants)
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L344-L347">Source</a></sub></p>

## <a name="formform.calc/filter-dna">`filter-dna`</a><a name="formform.calc/filter-dna"></a>




Filters a `dna` by selecting specific parts corresponding to a given `vpoint`, which acts as a coordinate vector in its value space.
- use holes `:_` to indicate a variable selection
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L394-L396">Source</a></sub></p>

## <a name="formform.calc/filter-dna-seq">`filter-dna-seq`</a><a name="formform.calc/filter-dna-seq"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L386-L386">Source</a></sub></p>

## <a name="formform.calc/inv">`inv`</a><a name="formform.calc/inv"></a>




Inverts the value of a every constant in a formDNA.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L551-L552">Source</a></sub></p>

## <a name="formform.calc/make-compare-consts">`make-compare-consts`</a><a name="formform.calc/make-compare-consts"></a>




Given a `sort-code` (try `calc.nuim-code` or `calc.nmui-code`), returns a comparator function to sort single constants, formDNA or arbitrary sequences of constants (can be mixed).
- can also compare map-entries by keys of comparable types
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L219-L221">Source</a></sub></p>

## <a name="formform.calc/make-dna">`make-dna`</a><a name="formform.calc/make-dna"></a>




Creates a formDNA from arguments, which may be valid chars, keywords, integers or sequences thereof.
- valid chars are: \n \u \i \m (upper- or lowercase) and \0 \1 \2 \3
- valid integers are: 0 1 2 3
- valid keywords are: :N :U :I :M
- total argument count (including count of sequence args) must match a valid formDNA length, which is 4^d, where d is a natural number
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L372-L377">Source</a></sub></p>

## <a name="formform.calc/nmui-code">`nmui-code`</a><a name="formform.calc/nmui-code"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L152-L152">Source</a></sub></p>

## <a name="formform.calc/nuim-code">`nuim-code`</a><a name="formform.calc/nuim-code"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L151-L151">Source</a></sub></p>

## <a name="formform.calc/permute-dna">`permute-dna`</a><a name="formform.calc/permute-dna"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L429-L429">Source</a></sub></p>

## <a name="formform.calc/permute-dna-seq">`permute-dna-seq`</a><a name="formform.calc/permute-dna-seq"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L428-L428">Source</a></sub></p>

## <a name="formform.calc/rand-const">`rand-const`</a><a name="formform.calc/rand-const"></a>
``` clojure

(rand-const)
```

Generates a random constant.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L20-L22">Source</a></sub></p>

## <a name="formform.calc/rand-dna">`rand-dna`</a><a name="formform.calc/rand-dna"></a>




Generates a random formDNA/`dna-seq` of dimension `dim`. A vector of 4 custom elements can be provided as a second argument.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L250-L251">Source</a></sub></p>

## <a name="formform.calc/rand-vpoint">`rand-vpoint`</a><a name="formform.calc/rand-vpoint"></a>




Generates a random vpoint either as an infinite lazy seq or with given dimension `dim`.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L468-L469">Source</a></sub></p>

## <a name="formform.calc/reduce-dna-seq">`reduce-dna-seq`</a><a name="formform.calc/reduce-dna-seq"></a>




Reduces a `dna-seq` by eliminating redundant/contingent terms.
- returns a tuple `[terms dna-seq]`, where `terms` is a sequence that represents the remaining terms after reduction
- takes an optional `terms` sequence of any kind of items that will be used instead of the default arithmetic sequence `[0 1 2 …]` to represent each term (length has to match the formDNA dimension)

Note: `dna-seq` can have any type of elements (not only constants)
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L359-L364">Source</a></sub></p>

## <a name="formform.calc/rel">`rel`</a><a name="formform.calc/rel"></a>




Relates the values of 2 constants in a formDNA to each other.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L541-L542">Source</a></sub></p>

## <a name="formform.calc/reorder-dna-seq">`reorder-dna-seq`</a><a name="formform.calc/reorder-dna-seq"></a>




Reorders given formDNA/`dna-seq` from `sort-code-from` to `sort-code-to`.

Note:
- `dna-seq` can have any type of elements (not only constants)
- does NOT change the encoding of the elements, just their ordering
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L267-L272">Source</a></sub></p>

## <a name="formform.calc/reverse-dna">`reverse-dna`</a><a name="formform.calc/reverse-dna"></a>




Reverses a formDNA (returns an rseq)
- make sure the input is a vector for constant-time reverse
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L253-L255">Source</a></sub></p>

## <a name="formform.calc/sort-code?">`sort-code?`</a><a name="formform.calc/sort-code?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L60-L60">Source</a></sub></p>

## <a name="formform.calc/spec--dna-args">`spec--dna-args`</a><a name="formform.calc/spec--dna-args"></a>
``` clojure

(spec--dna-args)
```
Function.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L284-L287">Source</a></sub></p>

## <a name="formform.calc/spec--dna-seq-args">`spec--dna-seq-args`</a><a name="formform.calc/spec--dna-seq-args"></a>
``` clojure

(spec--dna-seq-args spec)
```
Function.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L275-L282">Source</a></sub></p>

## <a name="formform.calc/var-const">`var-const`</a><a name="formform.calc/var-const"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L146-L146">Source</a></sub></p>

## <a name="formform.calc/vdict">`vdict`</a><a name="formform.calc/vdict"></a>




Generates a vdict given a map from vpoint to result (constant).
- if the corresponding vspace is not a subset of the set of keys from `vp->r`, the remaining results will be filled with :N or a given default constant
- optional `sorted?` defaults to false since sorting large vspace dimensions can be expensive
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L495-L498">Source</a></sub></p>

## <a name="formform.calc/vdict->vmap">`vdict->vmap`</a><a name="formform.calc/vdict->vmap"></a>




Generates a vmap from a given vdict.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L521-L522">Source</a></sub></p>

## <a name="formform.calc/vdict?">`vdict?`</a><a name="formform.calc/vdict?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L121-L121">Source</a></sub></p>

## <a name="formform.calc/vmap?">`vmap?`</a><a name="formform.calc/vmap?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L130-L130">Source</a></sub></p>

## <a name="formform.calc/vpoint?">`vpoint?`</a><a name="formform.calc/vpoint?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L99-L99">Source</a></sub></p>

## <a name="formform.calc/vspace">`vspace`</a><a name="formform.calc/vspace"></a>




Generates a vspace of dimension `dim`, optionally with custom `sort-code`.
- returns a lazy-seq which may be too memory-expensive to fully realize for dimensions greater than 11 (> 200 Mio. elements in total!)
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L479-L481">Source</a></sub></p>

## <a name="formform.calc/vspace?">`vspace?`</a><a name="formform.calc/vspace?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L113-L113">Source</a></sub></p>

## <a name="formform.calc/|">`|`</a><a name="formform.calc/|"></a>




Alias to [`inv`](#formform.calc/inv).
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/calc.cljc#L554-L554">Source</a></sub></p>

-----
# <a name="formform.expr">formform.expr</a>


API for the `expr` module of `formform`.




## <a name="formform.expr/=>">`=>`</a><a name="formform.expr/=>"></a>




Evaluates a FORM expression with an optional `env` and returns a Constant expression with attached metadata including the maximally reduced expression in `:expr` and the environment in `:env`.
- `env` must be a map with a content/variable in `expr` as a key
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L427-L429">Source</a></sub></p>

## <a name="formform.expr/=>*">`=>*`</a><a name="formform.expr/=>*"></a>




Evaluates a FORM expression for all possible interpretations of any occurring variable in the expression. Returns a formDNA expression by default.
- if `to-fdna?` is false, returns a seq of results as returned by [`=>`](#formform.expr/=>) in the order of the corresponding `vspace` ordering
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L439-L441">Source</a></sub></p>

## <a name="formform.expr/arrangement?">`arrangement?`</a><a name="formform.expr/arrangement?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L235-L235">Source</a></sub></p>

## <a name="formform.expr/cnt>">`cnt>`</a><a name="formform.expr/cnt>"></a>




Simplifies a FORM content recursively until it cannot be further simplified.
All deductions are justified by the axioms of FORM logic.
- if `x` is a complex FORM, calls `simplify-context` on `x`
- if no simplification applies, tries to retrieve the value from given `env`
- if retrieval was unsuccessful, returns `x` as is
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L404-L409">Source</a></sub></p>

## <a name="formform.expr/const->isolator">`const->isolator`</a><a name="formform.expr/const->isolator"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L631-L631">Source</a></sub></p>

## <a name="formform.expr/ctx>">`ctx>`</a><a name="formform.expr/ctx>"></a>




Simplifies a FORM context recursively until it cannot be further simplified.
All deductions are justified by the axioms of FORM logic.
- for complex expressions, calls `simplify-content` on every unique element
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L416-L419">Source</a></sub></p>

## <a name="formform.expr/defoperator">`defoperator`</a><a name="formform.expr/defoperator"></a>




Defines a new operator by its symbol (a keyword), a vector of arguments and an interpretation function. Takes additional key-value pairs for options.

Registers various methods for the operator:
- [`interpret-op`](#formform.expr/interpret-op) to access the interpretation function
- [`make-op`](#formform.expr/make-op) -> constructor (either uses the provided `args` or a custom constructor function via the option `:constructor`)
- [`simplify-op`](#formform.expr/simplify-op) -> simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)
- [`valid-op?`](#formform.expr/valid-op?) -> validator (provided by the `:predicate` option)
- [`op-data`](#formform.expr/op-data) -> returns a key-value map of the operator arguments
- [`op-get`](#formform.expr/op-get) -> returns a specific value by a given argument-key
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L201-L210">Source</a></sub></p>

## <a name="formform.expr/defsymbol">`defsymbol`</a><a name="formform.expr/defsymbol"></a>




Defines a new expression symbol by its symbol (a keyword) and an interpretation function. Takes additional key-value pairs for options.

Registers various methods for the expression symbol:
- [`interpret-sym`](#formform.expr/interpret-sym) -> to access the interpretation function
- [`simplify-sym`](#formform.expr/simplify-sym) -> simplifier (either defaults to the given interpretation function or uses a custom reducer via the option `:reducer`)
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L225-L230">Source</a></sub></p>

## <a name="formform.expr/equal">`equal`</a><a name="formform.expr/equal"></a>




Equality check for expressions. Two expressions are considered equal, if their formDNAs are equal. Compares formDNAs from evaluation results of each expression by calling `calc/equal-dna`.
- ordering of variable names in formDNA matters, see [`find-vars`](#formform.expr/find-vars)
- stricter than [`equiv`](#formform.expr/equiv), which compares by `calc/equiv-dna`
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L474-L477">Source</a></sub></p>

## <a name="formform.expr/equiv">`equiv`</a><a name="formform.expr/equiv"></a>




Equivalence check for expressions. Two expressions are considered equivalent, if their formDNAs are equivalent. Compares formDNAs from evaluation results of each expression by calling `calc/equiv-dna`.
- ordering of variable names in formDNA is irrelevant
- looser than [`equal`](#formform.expr/equal), which compares by `calc/equal-dna`
- can be slow on expressions with 6+ variables
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L482-L486">Source</a></sub></p>

## <a name="formform.expr/eval-all">`eval-all`</a><a name="formform.expr/eval-all"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L469-L469">Source</a></sub></p>

## <a name="formform.expr/evaluate">`evaluate`</a><a name="formform.expr/evaluate"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L451-L451">Source</a></sub></p>

## <a name="formform.expr/expr->const">`expr->const`</a><a name="formform.expr/expr->const"></a>




Given an expression, returns the corresponding constant value.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L237-L238">Source</a></sub></p>

## <a name="formform.expr/expr-symbol?">`expr-symbol?`</a><a name="formform.expr/expr-symbol?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L232-L232">Source</a></sub></p>

## <a name="formform.expr/expression?">`expression?`</a><a name="formform.expr/expression?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L247-L247">Source</a></sub></p>

## <a name="formform.expr/find-subexprs">`find-subexprs`</a><a name="formform.expr/find-subexprs"></a>




Finds all subexpressions in `expr` that match any element of the given set `subexprs`.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L325-L326">Source</a></sub></p>

## <a name="formform.expr/find-vars">`find-vars`</a><a name="formform.expr/find-vars"></a>




Finds all variables in an expresson or returns the expression itself if it is a variable.

Options:
- {:ordered true} to return variables in: type order -> alphanumeric order
- {:vars #{…}} can be given a set of specific variables to find
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L332-L337">Source</a></sub></p>

## <a name="formform.expr/form">`form`</a><a name="formform.expr/form"></a>




Constructor for FORM expressions. Calls [`make`](#formform.expr/make) on arguments.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L271-L272">Source</a></sub></p>

## <a name="formform.expr/form?">`form?`</a><a name="formform.expr/form?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L244-L244">Source</a></sub></p>

## <a name="formform.expr/gen-vars">`gen-vars`</a><a name="formform.expr/gen-vars"></a>




Generates a number of variables with random names.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L342-L343">Source</a></sub></p>

## <a name="formform.expr/interpret">`interpret`</a><a name="formform.expr/interpret"></a>




Interprets an expression of any kind. Returns the original expression if it cannot be interpreted.

Can be given an `env` map to interpret variables (as keys). This map can have an optional `--defocus` entry whose value is a set of items that should not be interpreted and a complementary `--focus` entry to only interpret the items specified in its set and nothing else.
- the keywords `:ops` / `:syms` / `:vars` designate _all_ operations / expression symbols / variables
- an operator symbol can provided to designate a specific operator
- any other expression (like a variable) can be designated as itself
- `--focus` and `--defocus` can cancel each other out if they contain the same item so you usually pick one or the other
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L286-L293">Source</a></sub></p>

## <a name="formform.expr/interpret*">`interpret*`</a><a name="formform.expr/interpret*"></a>




Like [`interpret`](#formform.expr/interpret), but repeats substitution on interpreted expressions until they cannot be interpreted any further.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L300-L301">Source</a></sub></p>

## <a name="formform.expr/interpret-op">`interpret-op`</a><a name="formform.expr/interpret-op"></a>




Interprets a symbolic expression with a registered operator.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L173-L174">Source</a></sub></p>

## <a name="formform.expr/interpret-sym">`interpret-sym`</a><a name="formform.expr/interpret-sym"></a>




Interprets a registered symbol.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L215-L216">Source</a></sub></p>

## <a name="formform.expr/interpret-walk">`interpret-walk`</a><a name="formform.expr/interpret-walk"></a>




Recursively calls [`interpret`](#formform.expr/interpret) on given expression and all its subexpressions with a depth-first walk.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L308-L309">Source</a></sub></p>

## <a name="formform.expr/interpret-walk*">`interpret-walk*`</a><a name="formform.expr/interpret-walk*"></a>




Like [`interpret-walk`](#formform.expr/interpret-walk), but repeats substitution on interpreted (sub-)expressions until they cannot be interpreted any further.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L316-L317">Source</a></sub></p>

## <a name="formform.expr/literal-expr?">`literal-expr?`</a><a name="formform.expr/literal-expr?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L250-L250">Source</a></sub></p>

## <a name="formform.expr/make">`make`</a><a name="formform.expr/make"></a>




Constructor for expressions of any kind. Validates its input. If the first argument (or the first after the options map) is a keyword of a registered operator, will call the constructor for that operator

Can be given an options map as first argument:
- `mark?` (default: false) marks the whole expression, creating a FORM
- `splice?` (default: true) dissolves all top-level arrangements
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L261-L266">Source</a></sub></p>

## <a name="formform.expr/make-op">`make-op`</a><a name="formform.expr/make-op"></a>




Constructs a symbolic expression given a registered operator and parameters.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L161-L162">Source</a></sub></p>

## <a name="formform.expr/mark-exprs">`mark-exprs`</a><a name="formform.expr/mark-exprs"></a>




Chains expressions like `((a)(b)…)` or `(a)(b)…` if {:unmarked? true}`
- group expressions with arrangements: `[:- x y …]`
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L498-L500">Source</a></sub></p>

## <a name="formform.expr/memory">`memory`</a><a name="formform.expr/memory"></a>




Constructs a memory FORM.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L579-L580">Source</a></sub></p>

## <a name="formform.expr/memory-extend">`memory-extend`</a><a name="formform.expr/memory-extend"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L557-L557">Source</a></sub></p>

## <a name="formform.expr/memory-replace">`memory-replace`</a><a name="formform.expr/memory-replace"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L551-L551">Source</a></sub></p>

## <a name="formform.expr/nest-exprs">`nest-exprs`</a><a name="formform.expr/nest-exprs"></a>




Nests expressions leftwards `(((…)a)b)` or rightwards `(a(b(…)))` if `{:ltr? true}`
- use `nil` for empty expressions
- use an arrangement `(make x y …)` to add multiple exprs. to the same level
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L516-L519">Source</a></sub></p>

## <a name="formform.expr/op-data">`op-data`</a><a name="formform.expr/op-data"></a>




Gets all parameters from a symbolic expression with a registered operator as a map.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L193-L194">Source</a></sub></p>

## <a name="formform.expr/op-get">`op-get`</a><a name="formform.expr/op-get"></a>




Gets a specified part from a symbolic expression with a registered operator.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L187-L188">Source</a></sub></p>

## <a name="formform.expr/op-spec">`op-spec`</a><a name="formform.expr/op-spec"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L18-L18">Source</a></sub></p>

## <a name="formform.expr/op-symbol">`op-symbol`</a><a name="formform.expr/op-symbol"></a>




Returns the symbol of an operator.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L198-L199">Source</a></sub></p>

## <a name="formform.expr/op-symbol?">`op-symbol?`</a><a name="formform.expr/op-symbol?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L233-L233">Source</a></sub></p>

## <a name="formform.expr/operator?">`operator?`</a><a name="formform.expr/operator?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L234-L234">Source</a></sub></p>

## <a name="formform.expr/pure-form?">`pure-form?`</a><a name="formform.expr/pure-form?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L245-L245">Source</a></sub></p>

## <a name="formform.expr/rem-pairs?">`rem-pairs?`</a><a name="formform.expr/rem-pairs?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L545-L545">Source</a></sub></p>

## <a name="formform.expr/sel">`sel`</a><a name="formform.expr/sel"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L640-L640">Source</a></sub></p>

## <a name="formform.expr/seq-re">`seq-re`</a><a name="formform.expr/seq-re"></a>




Constructs a self-equivalent re-entry FORM given the arguments:
- `specs`: either a `seq-reentry-signature` or an options map
- `nested-exprs`: zero or more expressions intended as a nested sequence
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L625-L628">Source</a></sub></p>

## <a name="formform.expr/seq-reentry-defaults">`seq-reentry-defaults`</a><a name="formform.expr/seq-reentry-defaults"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L582-L582">Source</a></sub></p>

## <a name="formform.expr/seq-reentry-opts->sign">`seq-reentry-opts->sign`</a><a name="formform.expr/seq-reentry-opts->sign"></a>




Inverse map of seq-reentry-sign->opts with default args.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L596-L597">Source</a></sub></p>

## <a name="formform.expr/seq-reentry-opts?">`seq-reentry-opts?`</a><a name="formform.expr/seq-reentry-opts?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L600-L600">Source</a></sub></p>

## <a name="formform.expr/seq-reentry-sign->opts">`seq-reentry-sign->opts`</a><a name="formform.expr/seq-reentry-sign->opts"></a>




Maps signatures for self-equivalent re-entry FORMs to their corresponding option-maps.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L588-L589">Source</a></sub></p>

## <a name="formform.expr/seq-reentry-signature?">`seq-reentry-signature?`</a><a name="formform.expr/seq-reentry-signature?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L599-L599">Source</a></sub></p>

## <a name="formform.expr/simplify-expr-chain">`simplify-expr-chain`</a><a name="formform.expr/simplify-expr-chain"></a>




Reduces a sequence of expressions, intended to be linked in a `chain`, to a sequence of simplified expressions, possibly spliced or shortened via inference.
- assumes rightward-nesting, e.g. `(…(…(…)))`
- for leftward-nesting, e.g. `(((…)…)…)`, pass `{:rtl? true}`
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L528-L531">Source</a></sub></p>

## <a name="formform.expr/simplify-op">`simplify-op`</a><a name="formform.expr/simplify-op"></a>




Simplifies a symbolic expression with a registered operator given an optional environment.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L180-L181">Source</a></sub></p>

## <a name="formform.expr/simplify-sym">`simplify-sym`</a><a name="formform.expr/simplify-sym"></a>




Simplifies a registered symbol given an optional environment.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L222-L223">Source</a></sub></p>

## <a name="formform.expr/splice-ctx">`splice-ctx`</a><a name="formform.expr/splice-ctx"></a>




Dissolves arrangements in given context such that their elements become direct children of the context itself.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L255-L256">Source</a></sub></p>

## <a name="formform.expr/struct-expr?">`struct-expr?`</a><a name="formform.expr/struct-expr?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L249-L249">Source</a></sub></p>

## <a name="formform.expr/substitute-expr">`substitute-expr`</a><a name="formform.expr/substitute-expr"></a>




Substitutes an expression by a matching expression in given environment. Returns the original expression if match failed.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L278-L279">Source</a></sub></p>

## <a name="formform.expr/unclear?">`unclear?`</a><a name="formform.expr/unclear?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L537-L537">Source</a></sub></p>

## <a name="formform.expr/valid-op?">`valid-op?`</a><a name="formform.expr/valid-op?"></a>




Validates the shape of a symbolic expression with a registered operator.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L167-L168">Source</a></sub></p>

## <a name="formform.expr/variable?">`variable?`</a><a name="formform.expr/variable?"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/expr.cljc#L246-L246">Source</a></sub></p>

-----
# <a name="formform.io">formform.io</a>


API for the `io` module of `formform`.




## <a name="formform.io/parse-tree">`parse-tree`</a><a name="formform.io/parse-tree"></a>



<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/io.cljc#L65-L65">Source</a></sub></p>

## <a name="formform.io/print-expr">`print-expr`</a><a name="formform.io/print-expr"></a>




Given an expression, returns a string of its representation in `formula` notation.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/io.cljc#L82-L83">Source</a></sub></p>

## <a name="formform.io/read-expr">`read-expr`</a><a name="formform.io/read-expr"></a>




Given a string in `formula` notation, returns the corresponding data structure that can be processed by [`formform.expr`](#formform.expr).
  
Can be given a map with the following options:
- `:sort-code` -> to specify a different `sort-code` for `formDNA` interpretation (see [`formform.calc/sort-code?`](#formform.calc/sort-code?))
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/io.cljc#L73-L77">Source</a></sub></p>

## <a name="formform.io/uniform-expr">`uniform-expr`</a><a name="formform.io/uniform-expr"></a>




Given an expression, returns a `uniform` data structure that is a nested map with the following pattern:
```
{:type <expr-type>
 …
 :children [<uniform> …]}
```
  
Can be given an option map to support various customizations (see source), e.g. the `:legacy?` flag can be set to output a map that can be used as `formJSON` for backward compatibility with formform 1.
<p><sub><a href="https://github.com/formsandlines/formform/blob/main/src/formform/io.cljc#L104-L112">Source</a></sub></p>
