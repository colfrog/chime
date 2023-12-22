(in-package :chime)

(defclass board-view (gadget-view) ())

(defclass board ()
  ((fields :initform
	   (make-array '(8 8)
		       :element-type 'field
		       :initial-element
		       (make-instance 'field :col 0 :row 0 :piece nil)))))

(defmethod initialize-instance :after ((board board) &key)
  (with-slots ((f fields)) board
    (dotimes (i 8)
      (dotimes (j 8)
	(setf (aref f i j)
	      (make-instance
	       'field
	       :col i
	       :row j
	       :piece (cond
			((= j 1)
			 (make-instance 'pawn :colour "black" :field nil))
			((= j 6)
			 (make-instance 'pawn :colour "white" :field nil))
			((or (= j 0) (= j 7))
			 (let ((colour (if (= j 0) "black" "white")))
			   (cond
			     ((or (= i 0) (= i 7))
			      (make-instance 'rook :colour colour :field nil))
			     ((or (= i 1) (= i 6))
			      (make-instance 'knight :colour colour :field nil))
			     ((or (= i 2) (= i 5))
			      (make-instance 'bishop :colour colour :field nil))
			     ((= i 3)
			      (make-instance 'queen :colour colour :field nil))
			     ((= i 4)
			      (make-instance 'king :colour colour :field nil))
			     (t nil))))
			(t nil))))
	(let* ((field (aref f i j))
	       (piece (piece field)))
	  (when piece
	    (setf (slot-value piece 'field) field)))))))

(define-presentation-method present ((board board) (type board) stream (view board-view) &key)
  (with-slots ((f fields)) board
    (dotimes (i 8)
      (dotimes (j 8)
	(with-translation (stream (* 50 i) (* 50 j))
	  (present (aref f i j)))))))
