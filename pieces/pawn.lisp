(in-package :chime)

(defclass pawn (piece)
  ((kind :initform "pawn")))

(defun en-passant-legal (diag colour)
  (with-slots (history) *application-frame*
    (let ((last-move (car history)))
      (when last-move
	(let ((delta (- (row (to-field last-move))
			(row (from-field last-move)))))
	  (and (= (abs delta) 2)
	       (string/= (colour (piece last-move)) colour)
	       (equal diag (cons (- (row (to-field last-move)) (/ delta 2))
				 (col (from-field last-move))))))))))

(defmethod possible-moves ((pawn pawn) (board board))
  (with-slots (colour field) pawn
    (with-slots (row col) field
      (with-slots (fields) board
	(let ((moves '())
	      (front (if (string= colour "white")
			 (cons (- row 1) col)
			 (cons (+ row 1) col)))
	      (front2 (if (string= colour "white")
			  (cons (- row 2) col)
			  (cons (+ row 2) col)))
	      (diagonals (if (string= colour "white")
			     (list (cons (- row 1) (+ col 1))
				   (cons (- row 1) (- col 1)))
			     (list (cons (+ row 1) (+ col 1))
				   (cons (+ row 1) (- col 1))))))
	  (when (and (or
		      (and (string= colour "black")
			   (= row 1))
		      (and (string= colour "white")
			   (= row 6)))
		     (not (piece (aref fields (car front2) (cdr front2))))
		     (not (piece (aref fields (car front) (cdr front)))))
	    (push front2 moves))
	  (when (and (<= 0 (car front) 7)
		     (<= 0 (cdr front) 7)
		     (not (piece (aref fields (car front) (cdr front)))))
	    (push front moves))
	  (dolist (diag diagonals)
	    (when (and (<= 0 (car diag) 7)
		       (<= 0 (cdr diag) 7))
	      (let ((diag-piece (piece (aref fields (car diag) (cdr diag)))))
		(when (or (and diag-piece (not (string= (colour diag-piece) colour)))
			  (en-passant-legal diag colour))
		  (push diag moves)))))
	  moves)))))
