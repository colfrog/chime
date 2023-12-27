(in-package :chime)

(defun draw (frame stream)
  (present (board frame) 'board :stream stream))

(define-application-frame chime ()
  ((current-player :initform "white"
		   :accessor current-player)
   (board :initform (make-instance 'board)
	  :accessor board)
   (highlighted-fields :initform '()
		       :accessor highlighted-fields)
   (selected-field :initform nil
		   :accessor selected-field)
   (history :initform '()
	    :accessor history))
  (:panes
   (app :application
	:scroll-bars nil
	:width (* 8 50)
	:height (* 8 50)
	:max-width (* 8 50)
	:max-height (* 8 50)
	:display-function 'draw
	:default-view (make-instance 'board-view))))

(defun test-field-selection (field &rest args)
  (declare (ignore args))
  (let* ((piece (piece field))
	 (frame *application-frame*)
	 (board (board frame))
	 (colour (current-player frame))
	 (king (if (string= colour "white")
		   (king-white board) (king-black board))))
    (and piece (string= (colour piece) colour)
	 (or (and (is-checked (field king) board colour)
		  (string= (kind piece) "king"))
	     (not (is-checked (field king) board colour)))
	 (/= (length (possible-moves piece (board frame))) 0))))

(define-command (com-select-field :name t :command-table chime)
    ((field 'field :gesture (:select :tester test-field-selection)))
  (let* ((frame *application-frame*)
	 (board (board frame)))
    (with-slots (selected-field highlighted-fields) frame

      (when selected-field
	(setf (slot-value selected-field 'selected) nil)
	(dolist (highlighted-field highlighted-fields)
	  (setf (slot-value highlighted-field 'highlighted) nil)))

      (let ((moves (possible-moves (piece field) (board frame))))
	(flet ((move-results-in-check (move)
		 (if (string= (kind (piece field)) "king")
		     nil
		     (with-slots (fields) board
		       (let* ((target-field (aref fields (car move) (cdr move)))
			      (piece (piece field))
			      (backup-piece (piece target-field))
			      (colour (current-player frame)))
			 (setf (slot-value target-field 'piece) piece)
			 (setf (slot-value field 'piece) nil)
			 (let ((result (is-checked
					(field (if (string= colour "white")
						   (king-white board)
						   (king-black board)))
					board colour)))
			   (setf (slot-value target-field 'piece) backup-piece)
			   (setf (slot-value field 'piece) piece)
			   result))))))
	  (let ((legal-moves (remove-if #'move-results-in-check moves)))
	    (when legal-moves
	      (setf selected-field field)
	      (setf (slot-value field 'selected) t)
	      (setf highlighted-fields '())
	      (dolist (position-to-highlight legal-moves)
		(let ((field-to-highlight (aref (fields (board frame))
						(car position-to-highlight)
						(cdr position-to-highlight))))
		  (setf (slot-value field-to-highlight 'highlighted) t)
		  (push field-to-highlight highlighted-fields))))))))))

(defun test-target-field (field &rest args)
  (declare (ignore args))
  (let* ((frame *application-frame*))
    (and
     (selected-field frame)
     (member field (highlighted-fields frame)))))

(define-command (com-move-piece :name t :command-table chime)
    ((field 'field :gesture (:select :tester test-target-field)))
  (let* ((frame *application-frame*)
	 (selected-field (selected-field frame))
	 (piece (piece selected-field))
	 (eaten-piece (piece field)))
    (setf (slot-value field 'piece) piece)
    (setf (slot-value piece 'field) field)
    (setf (slot-value selected-field 'piece) nil)
    (setf (slot-value selected-field 'selected) nil)

    ;; promotion
    (when (and (string= (kind piece) "pawn")
	       (= (row field) (if (string= (colour piece) "white") 0 7)))
      (setf (slot-value field 'piece) (make-instance 'queen :colour (colour piece) :field field)))

    ;; en passant
    (when (string= (kind piece) "pawn")
      (let ((row-delta (- (row field) (row selected-field)))
	    (col-delta (- (col field) (col selected-field))))
	(when (and (= (abs row-delta) 1) (= (abs col-delta) 1)
		   (not eaten-piece))
	  (let ((en-passant-field (aref (fields (board frame))
					(row selected-field)
					(col field))))
	    (setf (slot-value en-passant-field 'piece) nil)))))

    (push (make-instance 'move :piece piece :from selected-field :to field) (history frame))

    ;; castle
    (when (and (string= (kind piece) "king")
	       (= (abs (- (col field) (col selected-field))) 2))
      (let* ((fields (fields (board frame)))
	     (position (get-position field))
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
	(setf (slot-value rook-field 'piece) nil)))

    (setf selected-field nil)
    (dolist (highlighted-field (highlighted-fields frame))
      (setf (slot-value highlighted-field 'highlighted) nil))
    (setf (slot-value frame 'highlighted-fields) '())
    (setf (slot-value frame 'current-player)
	  (if (string= (current-player frame) "white")
	      "black"
	      "white"))))

(defun main ()
  (run-frame-top-level
   (make-instance 'chime)))
