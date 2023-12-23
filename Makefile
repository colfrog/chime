LISP ?= sbcl

build:
	$(LISP) --load chime.asd \
		--eval '(ql:quickload :chime)' \
		--eval '(asdf:make :chime)' \
		--eval '(quit)'
