paip-el
=======

This code is a port of the Common Lisp programs found in the book [Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html) written by Peter Norvig. The goal of this project is to enable Emacs extension developers to easily use the programming techniques in PAIP. The project focuses on providing the developers with good modular software tools, rather than helping them to understand AI programming techniques. If you would like to learn it, I recommend you install the [SBCL](http://www.sbcl.org/), a Common Lisp implementation, run and hack the original code by Norvig in Common Lisp.

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

The index below gives the chapter in the book, file name, implementation status, test coverage, and short description for each file.

| **Chapter** | **Filename**                                                                          | **I** | **T** | **Description**                                              |
|---------|-----------------------------------------------------------------------------------|-------|------|-------------------------------------------------------------------|
|       - | [README.md](README.md)               | WIP   |      | This file: explanation and index                                  |
|       - | [paip-examples.el](paip-examples.el) | WIP   |      | A list of example inputs taken from the book                      |
|       - | [paip-tutor.el](paip-tutor.el)       | ✓  |      | An interpreter for running the examples                           |
|       - | [paip.el](paip.el)                   | ✓  |      | Auxiliary functions; load this before anything else (auxfns.lisp) |
|       - | [paipx.el](paipx.el)                 | ✓  |      | Additional auxiliary functions for EL port                        |
|       1 | [paip-intro.el](paip-intro.el)       | ✓  |      | A few simple definitions                                          |
|       2 | [paip-simple.el](paip-simple.el)     | ✓  |      | Random sentence generator (two versions)                          |
|       3 | [paip-overview.el](paip-overview.el) | ✓  |      | 14 versions of LENGTH and other examples                          |
|       4 | [paip-gps1.el](paip-gps1.el)         | ✓  |      | Simple version of General Problem Solver                          |
|       4 | [paip-gps.el](paip-gps.el)           | ✓  |      | Final version of General Problem Solver                           |
|       5 | [paip-eliza1.el](paip-eliza1.el)     | ✓  |      | Basic version of Eliza program                                    |
|       5 | [paip-eliza.el](paip-eliza.el)       | ✓  |      | Eliza with more rules; different reader                           |
|       6 | [paip-patmatch.el](paip-patmatch.el) | ✓  |      | Pattern Matching Utility                                          |
|       6 | [paip-eliza-pm.el](paip-eliza-pm.el) | ✓  |      | Version of Eliza using utilities                                  |
|       6 | [paip-search.el](paip-search.el)     | ✓  |      | Search Utility                                                    |
|       6 | [paip-gps-srch.el](paip-srch.el)     | ✓  |      | Version of GPS using the search utility                           |
|       7 | [paip-student.el](paip-student.el)   | ✓  |      | The Student Program                                               |
|       8 | [paip-macsyma.el](paip-macsyma.el)   | ✓  |      | The Macsyma Program                                               |
|       8 | [paip-macsymar.el](paip-macsymar.el) | ✓  |      | Simplification and integration rules for Macsyma                  |
|    9-10 |                                                                                   |       |      | no files; important functions in paip.el                        |
|      11 | [paip-unify.el](paip-unify.el)       | ✓  |      | Unification functions                                             |
|      11 | [paip-prolog1.el](paip-prolog1.el)   | ✓  |      | First version of Prolog interpreter                               |
|      11 | [paip-prolog.el](paip-prolog.el)     | ✓  |      | Final version of Prolog interpreter                               |
|      12 | paip-prologc1.el                                                                  | Wont |      | First version of Prolog compiler. I see no value to port this half-baked software. Use paip-prologc.el instead                                  |
|      12 | paip-prologc2.el                                                                  | Wont |      | Second version of Prolog compiler. I see no value to port this half-baked software. Use paip-prologc.el instead                                  |
|      12 | [paip-prologc.el](paip-prologc.el)   | ✓  |      | Final version of Prolog compiler                                  |
|      12 | [paip-prologcp.el](paip-prologcp.el) | ✓  |      | Primitives for Prolog compiler                                    |
|      13 | [paip-clos.el](paip-clos.el)                                                                      | WIP      |      | Some object-oriented and CLOS code                                |
|      14 | paip-krep1.el                                                                     |       |      | Knowledge Representation code: first version                      |
|      14 | paip-krep2.el                                                                     |       |      | Knowledge Representation code with conjunctions                   |
|      14 | paip-krep.el                                                                      |       |      | Final KR code: worlds and attached functions                      |
|      15 | paip-cmacsyma.el                                                                  |       |      | Efficient Macsyma with canonical form                             |
|      16 | paip-mycin.el                                                                     |       |      | The Emycin expert system shell                                    |
|      16 | paip-mycin-r.el                                                                   |       |      | Some rules for a medical application of emycin                    |
|      17 | paip-waltz.el                                                                     |       |      | A Line-Labeling program using the Waltz algorithm                 |
|      18 | paip-othello.el                                                                   |       |      | The Othello playing program and some strategies                   |
|      18 | paip-othello2.el                                                                  |       |      | Additional strategies for Othello                                 |
|      18 | paip-edge-tab.el                                                                  |       |      | Edge table for Iago strategy                                      |
|      19 | paip-syntax1.el                                                                   |       |      | Syntactic Parser                                                  |
|      19 | paip-syntax2.el                                                                   |       |      | Syntactic Parser with semantics                                   |
|      19 | paip-syntax3.el                                                                   |       |      | Syntactic Parser with semantics and preferences                   |
|      20 | paip-unifgram.el                                                                  |       |      | Unification Parser                                                |
|      21 | paip-grammar.el                                                                   |       |      | Comprehensive grammar of English                                  |
|      21 | paip-lexicon.el                                                                   |       |      | Sample Lexicon of English                                         |
|      22 | paip-interp1.el                                                                   |       |      | Scheme interpreter, including version with macros                 |
|      22 | paip-interp2.el                                                                   |       |      | A tail recurive Scheme interpreter                                |
|      22 | paip-interp3.el                                                                   |       |      | A Scheme interpreter that handles call/cc                         |
|      23 | paip-compile1.el                                                                  |       |      | Simple Scheme compiler                                            |
|      23 | paip-compile2.el                                                                  |       |      | Compiler with tail recursion and primitives                       |
|      23 | paip-compile3.el                                                                  |       |      | Compiler with peephole optimizer                                  |
|      23 | paip-compopt.el                                                                   |       |      | Peephole optimizers for compile3.el                               |
|      24 | paip-loop.el                                                                      |       |      | Load this first if your Lisp doesn't support ANSI LOOP            |

License
-------

Copyright (C) 2014
Yosuke Funahashi <yosuke@funahashi.cc>

This file is part of paip-el.

paip-el is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

paip-el is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
