(asdf:defsystem chime
  :version "1.0.0"
  :author "Laurent Cimon <laurent@nilio.ca>"
  :maintainer "Laurent Cimon <laurent@nilio.ca>"
  :license "bsd-2-clause"
  :description "A game of chess made with McClim"
  :components ((:file "package")
	       (:file "board")
	       (:file "piece")
	       (:file "king")
	       (:file "queen")
	       (:file "rook")
	       (:file "knight")
	       (:file "bishop")
	       (:file "pawn")
	       (:file "field")
	       (:file "ui"))
  :depends-on (#:mcclim)
  :build-operation "program-op"
  :build-pathname "chime"
  :entry-point "chime:main")
