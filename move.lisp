(in-package :chime)

(defclass move ()
  ((piece :initarg :piece
	  :accessor piece)
   (from-field :initarg :from
	       :accessor from-field)
   (to-field :initarg :to
	     :accessor to-field)))

(defparameter *uci-rows* "87654321")
(defparameter *uci-columns* "abcdefgh")

(defmethod to-uci ((move move))
  (with-slots (from-field to-field) move
    (format nil
	    "~c~c~c~c"
	    (aref *uci-columns* (col from-field))
	    (aref *uci-rows* (row from-field))
	    (aref *uci-columns* (col to-field))
	    (aref *uci-rows* (row to-field)))))

(defun from-uci (uci-move board)
  (let* ((from (cons
		(position (aref uci-move 1) *uci-rows*)
		(position (aref uci-move 0) *uci-columns*)))
	 (to (cons
	      (position (aref uci-move 3) *uci-rows*)
	      (position (aref uci-move 2) *uci-columns*)))
	 (piece (piece (aref (fields board) (car from) (cdr from))))
	 (fields (fields board)))
    (make-instance 'move :piece piece
			 :from (aref fields (car from) (cdr from))
			 :to (aref fields (car to) (cdr to)))))
