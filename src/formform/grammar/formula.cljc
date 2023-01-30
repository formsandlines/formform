;; !! This is an auto-generated file and should not be edited directly !!
;;
;; Please edit the source file of the grammar instead:
;;   `src/formform/grammar/formula.ebnf`
;; and then execute the dedicated task in `bb.edn` to re-generate this file.

(ns formform.grammar.formula
  (:require #?(:clj  [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])))

(defparser parser
"(* formula notation for FORM logic
   -- created 11/2022, (c) Peter Hofmann *)

EXPR        = [LITERAL] CTX;
<CTX>       = NODE [LITERAL] CTX
            | <#'\\s+'> LITERAL CTX
            | Epsilon;
<NODE>      = FORM | OPERATOR | SEQRE | UNCLEAR | VAR_QUOT;
<LITERAL>   = FDNA_LIT | VAR | SYMBOL;
FORM        = <'('> [LITERAL] CTX <')'>;

VAR         = #'[a-zA-Z]\\w*';
VAR_QUOT    = <'\"'> TEXT <'\"'> | <\"'\"> TEXT <\"'\">;
<TEXT>      = #'[^\\(\\)\\[\\]\\{\\}\\/\\`\\'\\\"]+';
SYMBOL      = !(SEQRE_SYM | FDNA_SYM | MEMORY_SYM)
              #':[^ \\(\\)\\[\\]\\{\\}\\,\\.;:⁘\\`\\'\\\"@~]+';
OPERATOR    = <'['> (OP_SPEC | SYMBOL [LITERAL] CTX) <']'>;
<OP_SPEC>   = SEQRE_SYM SEQRE_SPEC | FDNA_SYM FDNA_SPEC | MEMORY_SYM MEMORY_SPEC
(* UNPARSED  = #'.*?' *)

UNCLEAR     = <'/'> TEXT <'/'>;

SEQRE       = <'{'> SEQRE_SPEC <'}'>;
SEQRE_SYM   = ':seq-re';
<SEQRE_SPEC> = [RE_SIGN | RE_OPTS <'|'>] EXPR {<','> EXPR};
RE_SIGN     = #'@~?_?' | #'\\.\\.@~?\\.?_?';
RE_OPTS     = RE_OPT {<'|'> RE_OPT};
<RE_OPT>    = #'2r(\\+1)?' | 'alt' | 'open';

FDNA        = #'::[NUIM]+' | #'::[0123]+';
FDNA_LIT    = FDNA;
FDNA_SYM    = ':fdna';
<FDNA_SPEC> = VARLIST FDNA;
VARLIST     = <'['> [(VAR_QUOT | VAR) {<','> (VAR_QUOT | VAR)}] <']'>;

MEMORY_SYM  = ':mem';
<MEMORY_SPEC> = REMLIST EXPR;
REMLIST     = [REM {<','> REM}] <'|'>;
REM         = EXPR <'='> EXPR;
(* REM         = <'['> (VAR | VAR_QUOT) EXPR <']'>; *)
"
  :auto-whitespace :standard)
