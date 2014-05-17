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

