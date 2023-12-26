(in-package :chime)

(defclass king (piece)
  ((kind :initform "king")))

(defmethod possible-moves ((king king) (board board))
  (with-slots (fields) board
    (with-slots (colour field) king
      (let ((moves '()))
	(dotimes (i 3)
	  (dotimes (j 3)
	    (let ((row (+ (row field) (1- i)))
		  (col (+ (col field) (1- j))))
	      (when (and (<= 0 row 7)
			 (<= 0 col 7))
		(let ((other-field (aref fields row col)))
		  (when (and (not (is-checked other-field board colour))
			     (or (and (piece other-field)
				      (string/= (colour (piece other-field)) colour))
				 (not (piece other-field))))
		    (push (cons row col) moves)))))))
	moves))))
