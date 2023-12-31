(in-package :chime)

(defclass board-view (gadget-view) ())

(defclass board ()
  ((fields :initform
	   (make-array '(8 8)
		       :element-type 'field
		       :initial-element
		       (make-instance 'field :col 0 :row 0 :piece nil))
	   :accessor fields)
   (king-white :initform nil
	       :accessor king-white)
   (king-black :initform nil
	       :accessor king-black)))

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
			      (setf (slot-value board (if (string= colour "white")
							  'king-white 'king-black))
				    (make-instance 'king :colour colour :field nil)))
			     (t nil))))
			(t nil))))
	(let* ((field (aref fields i j))
	       (piece (piece field)))
	  (when piece
	    (setf (slot-value piece 'field) field)))))))

(defmethod is-checkmate ((board board) (colour string))
  (with-slots (fields) board
    (dotimes (i 8)
      (dotimes (j 8)
	(let* ((field (aref fields i j))
	       (piece (piece field)))
	  (when (and (string= (kind piece) "king")
		     (string= (colour piece) colour))
	    (return (and (equal (possible-moves piece board) '())
			 (is-checked field board colour)))))))))

(defmethod play-move ((board board) (move move))
  (with-slots (fields) board
    (with-slots (from-field to-field piece) move
      (when piece
	(let ((frame *application-frame*)
	      (eaten-piece (piece to-field)))
	  (setf (slot-value from-field 'piece) nil)
	  (setf (slot-value to-field 'piece) piece)
	  (setf (slot-value piece 'field) to-field)

	  ;; promotion
	  (when (and (string= (kind piece) "pawn")
		     (= (row to-field) (if (string= (colour piece) "white") 0 7)))
	    (setf (slot-value to-field 'piece) (make-instance 'queen :colour (colour piece) :field to-field)))

	  ;; en passant
	  (when (string= (kind piece) "pawn")
	    (let ((row-delta (- (row to-field) (row from-field)))
		  (col-delta (- (col to-field) (col from-field))))
	      (when (and (= (abs row-delta) 1) (= (abs col-delta) 1)
			 (not eaten-piece))
		(let ((en-passant-field (aref (fields (board frame))
					      (row from-field)
					      (col to-field))))
		  (setf (slot-value en-passant-field 'piece) nil)))))

	  ;; castle
	  (when (and (string= (kind piece) "king")
		     (= (abs (- (col to-field) (col from-field))) 2))
	    (let* ((fields (fields (board frame)))
		   (position (get-position to-field))
		   (rook-position (cond
				    ((equal position '(0 . 2)) '(0 . 0))
				    ((equal position '(0 . 6)) '(0 . 7))
				    ((equal position '(7 . 2)) '(7 . 0))
				    ((equal position '(7 . 6)) '(7 . 7))))
		   (rook-field (aref fields (car rook-position) (cdr rook-position)))
		   (rook (piece rook-field))
		   (rook-target-field (aref fields (row rook-field)
					    (if (= (col rook-field) 0) 3 5))))
	      (setf (slot-value rook-target-field 'piece) rook)
	      (setf (slot-value rook 'field) rook-target-field)
	      (setf (slot-value rook-field 'piece) nil))))))))

(define-presentation-method present ((board board) (type board) stream (view board-view) &key)
  (with-slots (fields) board
    (let ((player-colour (player-colour *application-frame*)))
      (dotimes (i 8)
	(dotimes (j 8)
	  (with-translation (stream (* 50 j) (* 50 i))
	    (present
	     (aref fields (if (string= player-colour "white")
				i
				(- 7 i))
		   j))))))))
