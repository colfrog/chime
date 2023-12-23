(asdf:defsystem chime
  :version "1.0.0"
  :author "Laurent Cimon <laurent@nilio.ca>"
  :maintainer "Laurent Cimon <laurent@nilio.ca>"
  :license "bsd-2-clause"
  :description "A game of chess made with McClim"
  :components ((:file "package")
	       (:file "board")
	       (:file "piece")
	       (:file "pieces/king")
	       (:file "pieces/queen")
	       (:file "pieces/rook")
	       (:file "pieces/knight")
	       (:file "pieces/bishop")
	       (:file "pieces/pawn")
	       (:file "field")
	       (:file "ui"))
  :depends-on (#:mcclim)
  :build-operation "program-op"
  :build-pathname "chime"
  :entry-point "chime:main")
