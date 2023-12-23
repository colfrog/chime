(in-package :chime)

(defclass board-view (gadget-view) ())

(defclass board ()
  ((fields :initform
	   (make-array '(8 8)
		       :element-type 'field
		       :initial-element
		       (make-instance 'field :col 0 :row 0 :piece nil))
	   :accessor fields)))

(defmethod initialize-instance :after ((board board) &key)
  (with-slots (fields) board
    (dotimes (i 8)
      (dotimes (j 8)
	(setf (aref fields i j)
	      (make-instance
	       'field
	       :row i
	       :col j
	       :piece (cond
			((= i 1)
			 (make-instance 'pawn :colour "black" :field nil))
			((= i 6)
			 (make-instance 'pawn :colour "white" :field nil))
			((or (= i 0) (= i 7))
			 (let ((colour (if (= i 0) "black" "white")))
			   (cond
			     ((or (= j 0) (= j 7))
			      (make-instance 'rook :colour colour :field nil))
			     ((or (= j 1) (= j 6))
			      (make-instance 'knight :colour colour :field nil))
			     ((or (= j 2) (= j 5))
			      (make-instance 'bishop :colour colour :field nil))
			     ((= j 3)
			      (make-instance 'queen :colour colour :field nil))
			     ((= j 4)
			      (make-instance 'king :colour colour :field nil))
			     (t nil))))
			(t nil))))
	(let* ((field (aref fields i j))
	       (piece (piece field)))
	  (when piece
	    (setf (slot-value piece 'field) field)))))))

(define-presentation-method present ((board board) (type board) stream (view board-view) &key)
  (with-slots (fields) board
    (dotimes (i 8)
      (dotimes (j 8)
	(with-translation (stream (* 50 j) (* 50 i))
	  (present (aref fields i j)))))))
