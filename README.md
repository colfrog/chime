# Chime

Chime is a game of chess made in Common Lisp with McCLIM.

To play, all you need is a Common Lisp compiler and quicklisp.

You can compile it to a binary and run it

```bash
# make
# ./chime
```

Or you can load it from a Common Lisp compiler and run chime:main

First you have to make sure McCLIM is installed

```lisp
> (ql:quickload :mcclim)
```

Then you can load chime.asd and run it

```lisp
CL-USER> (load "chime.asd") ; it's important to be in Chime's root directory for it to find the sprites
CL-USER> (asdf:load-system :chime)
CL-USER> (chime:main)
```
