paip-el
=======

This code is a port of the Common Lisp programs found in the book [Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html), written by Peter Norvig. The goal of this project is to enable Emacs extension developers to use programming techniques in PAIP easily. This project focuses on providing them with good modular software tools rather than helping Emacs extention developers to understand AI programming techniques. If you would like to learn PAIP, I recommend you install the [SBCL](http://www.sbcl.org/), a Common Lisp implementation, to run and hack the code by Norvig in Common Lisp.

The original source was all written by Peter Norvig, and the copyright belongs to him.

Please note that:

- It's under development,
- It's incomplete,
- Not all the stuff, functions and macros, here works.

While it is very early in the development of the modules, all feedback is welcome via github messages and/or as github issues.

[CL to EL Porting Tips](https://github.com/yfuna/paip-el/wiki/CL-to-EL-Porting-Tips) is available on the project wiki.

Installation
------------

Put all elisp files in your load-path and require what you use. For example,


```
(require 'paip-prolog)
```

The Files
---------

| Chapter | Filename         | Impl. | Test | Original Description                                              | EL port Description |
|---------|------------------|-------|------|-------------------------------------------------------------------|---------------------|
|       - | README.md        | WIP   |      | This file: explanation and index                                  |                     |
|       - | paip-examples.el | WIP   |      | A list of example inputs taken from the book                      |                     |
|       - | paip-tutor.el    | Done  |      | An interpreter for running the examples                           |                     |
|       - |                  |       |      |                                                                   |                     |
|       - | paip.el          | Done  |      | Auxiliary functions; load this before anything else (auxfns.lisp) |                     |
|       - | paipx.el         | Done  |      | Additional auxiliary functions for EL port                        |                     |
|       1 | paip-intro.el    | Done  |      | A few simple definitions                                          |                     |
|       2 | paip-simple.el   | Done  |      | Random sentence generator (two versions)                          |                     |
|       3 | paip-overview.el | Done  |      | 14 versions of LENGTH and other examples                          |                     |
|       4 | paip-gps1.el     | Done  |      | Simple version of General Problem Solver                          |                     |
|       4 | paip-gps.el      | Done  |      | Final version of General Problem Solver                           |                     |
|       5 | paip-eliza1.el   | Done  |      | Basic version of Eliza program                                    |                     |
|       5 | paip-eliza.el    | Done  |      | Eliza with more rules; different reader                           |                     |
|       6 | paip-patmatch.el | Done  |      | Pattern Matching Utility                                          |                     |
|       6 | paip-eliza-pm.el | Done  |      | Version of Eliza using utilities                                  |                     |
|       6 | paip-search.el   | Done  |      | Search Utility                                                    |                     |
|       6 | paip-gps-srch.el | Done  |      | Version of GPS using the search utility                           |                     |
|       7 | paip-student.el  | Done  |      | The Student Program                                               |                     |
|       8 | paip-macsyma.el  | Done  |      | The Macsyma Program                                               |                     |
|       8 | paip-macsymar.el | Done  |      | Simplification and integration rules for Macsyma                  |                     |
|    9-10 |                  |       |      | no files; important functions in auxfns.el                        |                     |
|      11 | paip-unify.el    | Done  |      | Unification functions                                             |                     |
|      11 | paip-prolog1.el  | Done  |      | First version of Prolog interpreter                               |                     |
|      11 | paip-prolog.el   | Done  |      | Final version of Prolog interpreter                               |                     |
|      12 | paip-prologc1.el | Won't |      | First version of Prolog compiler                                  |                     |
|      12 | paip-prologc2.el | Won't |      | Second version of Prolog compiler                                 |                     |
|      12 | paip-prologc.el  | Done  |      | Final version of Prolog compiler                                  |                     |
|      12 | paip-prologcp.el | Done  |      | Primitives for Prolog compiler                                    |                     |
|      13 | paip-clos.el     |       |      | Some object-oriented and CLOS code                                |                     |
|      14 | paip-krep1.el    |       |      | Knowledge Representation code: first version                      |                     |
|      14 | paip-krep2.el    |       |      | Knowledge Representation code with conjunctions                   |                     |
|      14 | paip-krep.el     |       |      | Final KR code: worlds and attached functions                      |                     |
|      15 | paip-cmacsyma.el |       |      | Efficient Macsyma with canonical form                             |                     |
|      16 | paip-mycin.el    |       |      | The Emycin expert system shell                                    |                     |
|      16 | paip-mycin-r.el  |       |      | Some rules for a medical application of emycin                    |                     |
|      17 | paip-waltz.el    |       |      | A Line-Labeling program using the Waltz algorithm                 |                     |
|      18 | paip-othello.el  |       |      | The Othello playing program and some strategies                   |                     |
|      18 | paip-othello2.el |       |      | Additional strategies for Othello                                 |                     |
|      18 | paip-edge-tab.el |       |      | Edge table for Iago strategy                                      |                     |
|      19 | paip-syntax1.el  |       |      | Syntactic Parser                                                  |                     |
|      19 | paip-syntax2.el  |       |      | Syntactic Parser with semantics                                   |                     |
|      19 | paip-syntax3.el  |       |      | Syntactic Parser with semantics and preferences                   |                     |
|      20 | paip-unifgram.el |       |      | Unification Parser                                                |                     |
|      21 | paip-grammar.el  |       |      | Comprehensive grammar of English                                  |                     |
|      21 | paip-lexicon.el  |       |      | Sample Lexicon of English                                         |                     |
|      22 | paip-interp1.el  |       |      | Scheme interpreter, including version with macros                 |                     |
|      22 | paip-interp2.el  |       |      | A tail recurive Scheme interpreter                                |                     |
|      22 | paip-interp3.el  |       |      | A Scheme interpreter that handles call/cc                         |                     |
|      23 | paip-compile1.el |       |      | Simple Scheme compiler                                            |                     |
|      23 | paip-compile2.el |       |      | Compiler with tail recursion and primitives                       |                     |
|      23 | paip-compile3.el |       |      | Compiler with peephole optimizer                                  |                     |
|      23 | paip-compopt.el  |       |      | Peephole optimizers for compile3.el                               |                     |
|      24 | paip-loop.el     |       |      | Load this first if your Lisp doesn't support ANSI LOOP            |                     |



