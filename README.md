# formform

<br/>

**formform** is a modular Clojure/ClojureScript/JavaScript library all about the 4-valued logic of cognition first introduced 2017 by Ralf Peyn in [uFORM iFORM](https://uformiform.info). In its core, the purpose of the library is to calculate with all 5 FORMs (marked, unmarked, undetermined, imaginary and unclear) introduced in the book and is meant to be extended with more specialized modules for different tasks such as FORM representation, algebra, visualization or simulation and analysis using CAs.

As a helpful tool for researchers and enthusiasts and as a demonstration of the library's capabilities I have also created the [**FORM tricorder**](https://github.com/formsandlines/form-tricorder). It can calculate, represent and visualize FORMs using my special `formula` syntax (described below under *formform.form*). Further applications (like a cellular automaton) are listed on the [formform website](https://formform.dev).

Please note that my library as well as my apps are still *work in progress*. The library is currently in the process of a complete rewrite in Clojure and you may want to check out the stable-ish previous JavaScript version on the master branch or wait for a stable release if you intent to use it in your projects. Although I am very passionate about this, I am not a formally trained developer and cannot yet afford to do this full-time.


<br/>

## Further information

If you want to learn more about the calculus, ideas and theories *formform* is based on, here are some helpful resources:

- [About uFORM iFORM](https://uformiform.info) (mostly German as is the language of the book by Ralf Peyn, but you can try [DeepL](https://www.deepl.com/translator) to translate the gist of it)
- [Here is a list of links](https://uformiform.info/#section_recommendations) on the theoretical background behind uFORM iFORM (mostly German resources, but you can just translate the keywords and google them)
- [3-dimensional FORM animations and FORM-builder](https://uformiform.info/animations) (a project I made back in 2017 that greatly influenced my approach to formform)
- [Blog s y s t e m z e i t](https://carl-auer-akademie.com/blogs/systemzeit/) by Gitta Peyn – German and English articles about systemic research based on uFORM iFORM
- [About FORMWELT](https://formwelt.info) – a coding language for language and meaning founded on the logic of cognition introduced in uFORM iFORM (by the way, we appreciate any support for the development of [FORMWELT Online](https://formwelt.info/formwelt-online)!)

<br/>

## History

This library has become my personal project since I first began studying [uFORM iFORM](https://uformiform.info), published by Ralf Peyn in 2017. Ralf's “SelFis” *(visual interpretation of a partial System of the self-referential System of the FORM)* inspired me to develop my own [cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton) in the programming environment [Processing](https://processing.org/) to dig deeper and gain a fuller unterstanding of these systems. 

Working with lookup-tables for FORM calculations was okay for a while, but also very tedious and impractical for my research, so I began working on some functions that would do the calculations for me. In 2018 I was finally able to implement an algorithm to calculate all self-equivalent re-entry FORMs as described in uFORM iFORM. I immediately did countless calculations by hand and let Ralf also check that the algorithm is solid and its results are correct.

As soon as I was able to automate calculation with undetermined FORMs, I saw that there was much more potential in this and that it could be very helpful for other people who want to work with FORM logic as well. So I began working on a JavaScript library to elaborate my ideas, which gradually became *formform*. Since its early development, formform has always evolved in a fruitful interplay with the applications built on top of it.

A first application that I have developed in parallel from the beginning was the [**FORM tricorder**](https://tricorder.formform.dev) – a swiss army knife for FORM calculation, representation and visualization. In September 2019 I was finally able to develop a new [cellular automaton for FORM logic SelFis](https://plotter.formform.dev) with *formform* that is much more user-friendly and much more versatile than what I have done two years earlier. My experimentation with rule extraction by bitmasking in CAs led me to a code format I call [**formDNA**](https://observablehq.com/@formsandlines/the-dna-of-4-valued-forms) that is an abstraction of the value table. It not only made my CA faster and more flexible, it also inspired me to create the **vmap**: a recursive variable/value map to visualize formDNA, that has great potential for pattern recognition in FORMs.

Driven by my own curiosity and some helpful suggestions from users, I continuously work on implementing new ideas and features into the library. Nowadays, I am prototyping most of these ideas in my [Observable notebooks](https://observablehq.com/@formsandlines) and announce new developments, bugfixes and changes on my [Twitter account](https://twitter.com/diagramaniac).

In the near future I want to rewrite my CA for SelFis as a more professional standalone application and also develop other CAs for [decisionFORMs, lifeFORMs and mindFORMs](https://www.carl-auer.de/magazin/systemzeit/how-does-system-function-operate-5). I also want to find a way to algorithmically generate spirals for circular re-entry FORMs, to have a more iconic representation closer to Ralfs notation in uFORM iFORM. There are many more ideas in the pipeline, that I hope to realize as time and other resources allow.

<br/>

## Support

If you want to support my work, consider [buying me a coffee](https://www.buymeacoffee.com/formsandlines). ☕

---

(c) 2018–2022 by Peter Hofmann

License: MIT
