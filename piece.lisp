(in-package :chime)

(defclass piece ()
  ((colour :initarg :colour
	   :reader colour)
   (kind :initarg :kind
	 :accessor kind)
   (field :initarg :field
	  :accessor field)
   (image-path :initform nil
	       :accessor image-path)))

(defmethod initialize-instance :after ((piece piece) &key)
  (with-slots (image-path kind colour) piece
    (setf image-path
	  (pathname
	  (concatenate
	   'string
	   "sprites/"
	   kind
	   "_"
	   colour
	   ".png")))))

(defgeneric possible-moves (piece board)
  (:documentation "Returns a list of possible moves"))

(defun to-opposite-piece (piece board mul)
  (with-slots (fields) board
    (with-slots (field colour) piece
      (let ((moves '()))
	(dotimes (i 7)
	  (let ((row (+ (row field) (* (car mul) (+ 1 i))))
		(col (+ (col field) (* (cdr mul) (+ 1 i)))))
	    (when (or (not (<= 0 row 7))
		      (not (<= 0 col 7)))
	      (return moves))
	    (let ((other-field (aref fields row col)))
	      (when (and (piece other-field)
			 (string= (colour (piece other-field)) colour))
		(return moves))
	      (push (cons (row other-field) (col other-field)) moves)
	      (when (piece other-field)
		(return moves)))))
	moves))))

(defmethod lines ((piece piece) (board board))
  (concatenate
   'list
   (to-opposite-piece piece board (cons 1 0))
   (to-opposite-piece piece board (cons -1 0))
   (to-opposite-piece piece board (cons 0 1))
   (to-opposite-piece piece board (cons 0 -1))))

(defmethod diagonals ((piece piece) (board board))
  (concatenate
   'list
   (to-opposite-piece piece board (cons 1 1))
   (to-opposite-piece piece board (cons -1 -1))
   (to-opposite-piece piece board (cons -1 1))
   (to-opposite-piece piece board (cons 1 -1))))

(define-presentation-method present ((piece piece) (type piece) stream (view board-view) &key)
  (with-slots (image-path) piece
    (let ((pattern (climi::%collapse-pattern
		    (make-pattern-from-bitmap-file image-path :format :png)
		    0 0 46 46)))
      (draw-pattern* stream pattern 2 2))))
