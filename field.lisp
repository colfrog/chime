(in-package :chime)

(defclass field ()
  ((col :initarg :col
	:reader col)
   (row :initarg :row
	:reader row)
   (highlighted :initform nil
		:accessor highlighted)
   (selected :initform nil
	     :accessor selected)
   (piece :initarg :piece
	  :accessor piece)))

(defmethod get-position ((field field))
  (with-slots (row col) field
    (cons row col)))

(defmethod positions-around (field)
  (with-slots (row col) field
    (let ((moves '()))
      (dotimes (i 3)
	(dotimes (j 3)
	  (let ((other-row (+ row (1- i)))
		(other-col (+ col (1- j))))
	    (push (cons other-row other-col) moves))))
      moves)))

(defmethod is-checked ((field field) (board board) (colour string))
  (with-slots (fields) board
    (let ((attacks '()))
      (dotimes (i 8)
	(dotimes (j 8)
	  (let* ((other-field (aref fields i j))
		 (piece (piece other-field)))
	    (when (and piece (string/= (colour piece) colour))
	      (let ((piece-attacks
		      (if (string= (kind piece) "king")
			  (positions-around other-field)
			  (if (string= (kind piece) "pawn")
			      (let ((row (row other-field))
				    (col (col other-field)))
				(if (string= (colour piece) "white")
				    (list (cons (1- row) (1+ col))
					  (cons (1- row) (1- col)))
				    (list (cons (1+ row) (1+ col))
					  (cons (1+ row) (1- col)))))
			      (possible-moves piece board)))))
		(setf attacks (concatenate 'list attacks piece-attacks)))))))
      (when (member (cons (row field) (col field)) attacks :test #'equal)
	t))))

(define-presentation-method present ((field field) (type field) stream (view board-view) &key)
  (with-slots (col row piece highlighted selected) field
    (let ((colour (if (evenp (+ row col)) +pale-goldenrod+ +dark-khaki+)))
      (draw-rectangle* stream 0 0 50 50 :ink colour)
      (when highlighted
	(draw-rectangle* stream 1 1 49 49 :ink +red+ :filled nil :line-thickness 2))
      (when selected
	(draw-rectangle* stream 1 1 49 49 :ink +black+ :filled nil :line-thickness 2))
      (when piece
	(present piece 'piece :stream stream :view view)))))
