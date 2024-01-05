(in-package :chime)

(defun draw (frame stream)
  (present (board frame) 'board :stream stream))

(define-application-frame chime ()
  ((player-colour :initform (if (= (random 2) 1) "white" "black")
		  :accessor player-colour)
   (board :initform (make-instance 'board)
	  :accessor board)
   (highlighted-fields :initform '()
		       :accessor highlighted-fields)
   (selected-field :initform nil
		   :accessor selected-field)
   (history :initform '()
	    :accessor history)
   (uci :initarg :uci))
  (:panes
   (app :application
	:scroll-bars nil
	:width (* 8 50)
	:height (* 8 50)
	:max-width (* 8 50)
	:max-height (* 8 50)
	:display-function 'draw
	:default-view (make-instance 'board-view))))

(defmethod initialize-instance :after ((frame chime) &key)
  (with-slots (uci player-colour board) frame
    (if uci
	(progn
	  (setup uci)
	  (wait-ready uci)
	  (when (string= player-colour "black")
	    (let ((best-move (get-best-move uci board)))
	      (play-move board best-move)
	      (play-move uci best-move))))
	(setf player-colour "white"))))

(defun test-field-selection (field &rest args)
  (declare (ignore args))
  (let* ((piece (piece field))
	 (frame *application-frame*)
	 (board (board frame))
	 (colour (player-colour frame))
	 (king (if (string= colour "white")
		   (king-white board) (king-black board)))
	 (checked (is-checked (field king) board colour)))
    (and piece (string= (colour piece) colour)
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
			      (colour (player-colour frame)))
			 (setf (slot-value target-field 'piece) piece)
			 (setf (slot-value field 'piece) nil)
			 (setf (slot-value piece 'field) target-field)
			 (let ((result (is-checked
					(field (if (string= colour "white")
						   (king-white board)
						   (king-black board)))
					board colour)))
			   (setf (slot-value target-field 'piece) backup-piece)
			   (setf (slot-value field 'piece) piece)
			   (setf (slot-value piece 'field) field)
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
	 (piece (piece selected-field)))
    (push (make-instance 'move :piece piece :from selected-field :to field) (history frame))

    (setf (slot-value selected-field 'selected) nil)
    (setf selected-field nil)
    (dolist (highlighted-field (highlighted-fields frame))
      (setf (slot-value highlighted-field 'highlighted) nil))
    (setf (slot-value frame 'highlighted-fields) '())

    (with-slots (uci board history player-colour) frame
      (play-move board (car history))
      (if uci
	  (progn
	    (play-move uci (car history))
	    (let ((best-move (get-best-move uci board)))
	      (play-move board best-move)
	      (play-move uci best-move)
	      (push best-move (history frame))))
	  (setf player-colour
		(if (string= player-colour "white")
		    "black" "white"))))))

(defun main ()
  (run-frame-top-level
   (make-instance 'chime
		  :uci (make-instance
			'uci
			:executable-path "stockfish"
			:options '(("Skill Level" . "3"))))))
