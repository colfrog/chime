(in-package :chime)

(defclass knight (piece)
  ((kind :initform "knight")))

(defmethod possible-moves ((knight knight) (board board))
  (with-slots (fields) board
    (with-slots (colour field) knight
      (let ((moves '()))
	(dolist (delta '((2 . 1) (-2 . -1) (-2 . 1) (2 . -1)
			    (1 . 2) (-1 . -2) (-1 . 2) (1 . -2)))
	  (let ((row (+ (row field) (car delta)))
		(col (+ (col field) (cdr delta))))
	    (when (and (<= 0 row 7)
		       (<= 0 col 7))
	      (let ((other-field (aref fields row col)))
		(when (or (and (piece other-field)
			       (not (string=
				     colour
				     (colour (piece other-field)))))
			  (not (piece other-field)))
		  (push (cons row col) moves))))))
	moves))))
