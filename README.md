# formform

<br/>

**formform** is a modular Clojure/ClojureScript library all about the 4-valued logic of cognition first introduced 2017 by Ralf Peyn in [uFORM iFORM](https://uformiform.info). In its core, the purpose of the library is to enable the representation, evaluation and algebraic simplification of all (undetermined, imaginary, unclear, …) FORMs that have been introduced in the book. 

[![Clojars Project](https://img.shields.io/clojars/v/eu.formsandlines/formform.svg)](https://clojars.org/eu.formsandlines/formform)

- [Introduction](https://formform.dev/notebooks/introduction.html)
- [API Docs](https://formform.dev/docs)

---

*Please note that this library is still in an alpha state and things are changing all the time, so do not rely on it for your projects yet. It is a complete rewrite of my previous (stable-ish) JavaScript version, which is still available [here](https://github.com/formsandlines/formform-js). Prior to this (final) one, I have attempted a rewrite in ReScript, which is also still [available](https://github.com/formsandlines/formform-rescript), for historical reasons.*

---

The main goals of formform are:

* to be correct and complete with regards to the calculus
* to be flexible and extendable enough for any imaginable application
* to provide a clear, simple and efficient API
* to provide reliable output that can be easily transformed and shared

Although I constantly try to improve performance, it is not a main goal and I am not qualified enough for sophisticated optimizations. You are very welcome to contribute in this area and correct my mistakes.

The library is divided into 3 core modules:

* `calc`: calculate with the 4 value constants or create and transform value structures such as value tables/maps and what I call *formDNA*
* `expr`: construct, simplify and evaluate various FORM expressions or define and use powerful abstractions I call *symbolic expressions*
* `io`: read and print strings in *FORMula notation*, which is easier for end users to read and write

*Work in progress:*
* `alg`: define substitution rules using a pattern language to represent and perform stepwise algebraic deductions

It is extendable with more specialized modules for different tasks such as visualization or simulation/analysis with cellular automata.

As a helpful tool for researchers and enthusiasts and as a demonstration of the library's capabilities I have also created the [**FORM tricorder**](https://github.com/formsandlines/form-tricorder) (it still uses an old, but stable version of formform). It can calculate, represent and visualize FORMs using my own `formula` syntax, which is explained in the app (click on `Show explanations`). Further applications (like a cellular automaton) are listed on the [formform website](https://formform.dev).


<br/>

## Further information

If you want to learn more about the calculus, ideas and theories *formform* is based upon, here are some helpful resources:

- [About uFORM iFORM](https://uformiform.info) (mostly German as is the language of the book by Ralf Peyn, but you can try [DeepL](https://www.deepl.com/translator) to translate the gist of it)
- [Here is a list of links](https://uformiform.info/#section_recommendations) on the theoretical background behind uFORM iFORM (mostly German resources, but you can just translate the keywords and google them)
- [3-dimensional FORM animations and FORM-builder](https://uformiform.info/animations) (a project I made back in 2017 that greatly influenced my approach to formform)
- [Blog s y s t e m z e i t](https://carl-auer-akademie.com/blogs/systemzeit/) by Gitta Peyn – German and English articles about systemic research based on uFORM iFORM
- [About FORMWELT](https://formwelt.info) – a coding language for language and meaning founded on the logic of cognition introduced in uFORM iFORM (by the way, we appreciate any support for the development of [FORMWELT Online](https://formwelt.info/formwelt-online)!)

<br/>

## History and Motivation

This library has become my personal project since I first began studying [uFORM iFORM](https://uformiform.info), published by Ralf Peyn in 2017. Ralf's “SelFis” *(visual interpretation of a partial System of the self-referential System of the FORM)* inspired me to develop my own [cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton) in the programming environment [Processing](https://processing.org/) to dig deeper and gain a fuller unterstanding of these systems. 

Working with lookup-tables for FORM calculations was okay for a while, but also very tedious and impractical for my research, so I began working on some functions that would do the calculations for me. In 2018 I was finally able to implement an algorithm to calculate all self-equivalent re-entry FORMs as described in uFORM iFORM. I immediately did countless calculations by hand and let Ralf also check that the algorithm is solid and its results are correct.

As soon as I was able to automate calculation with undetermined FORMs, I saw that there was much more potential in this and that it could be very helpful for other people who want to work with FORM logic as well. So I began working on a JavaScript library to elaborate my ideas, which gradually became *formform*. Since its early development, formform has always evolved in a fruitful interplay with the applications built on top of it.

A first application that I have developed in parallel from the beginning was the [**FORM tricorder**](https://tricorder.formform.dev) – a swiss army knife for FORM calculation, representation and visualization. In September 2019 I was finally able to develop a new [cellular automaton for FORM logic SelFis](https://plotter.formform.dev) with *formform* that is much more user-friendly and much more versatile than what I have done two years earlier. My experimentation with rule extraction by bitmasking in CAs led me to a code format I call [**formDNA**](https://observablehq.com/@formsandlines/the-dna-of-4-valued-forms) that is an abstraction of the value table. It not only made my CA faster and more flexible, it also inspired me to create the **vmap**: a recursive variable/value map to visualize formDNA, that has great potential for pattern recognition in FORMs.

<br/>

## Support

If you want to support my work, consider [buying me a coffee](https://www.buymeacoffee.com/formsandlines). ☕

---

(c) 2018–2024 by Peter Hofmann

License: MIT
