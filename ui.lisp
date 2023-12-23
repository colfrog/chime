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
		   :accessor selected-field))
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
  (let ((piece (piece field))
	(frame *application-frame*))
    (and piece (string= (colour piece) (current-player frame))
	 (/= (length (possible-moves piece (board frame))) 0))))

(define-command (com-select-field :name t :command-table chime)
    ((field 'field :gesture (:select :tester test-field-selection)))
  (let ((frame *application-frame*))
    (with-slots (selected-field highlighted-fields) frame

      (when selected-field
	(setf (slot-value selected-field 'selected) nil)
	(dolist (highlighted-field highlighted-fields)
	  (setf (slot-value highlighted-field 'highlighted) nil)))

      (setf selected-field field)
      (setf (slot-value field 'selected) t)
      (setf highlighted-fields '())
      (dolist (position-to-highlight (possible-moves (piece field)
						     (board frame)))
	(let ((field-to-highlight (aref (fields (board frame))
					(car position-to-highlight)
					(cdr position-to-highlight))))
	  (setf (slot-value field-to-highlight 'highlighted) t)
	  (push field-to-highlight highlighted-fields))))))

(defun test-target-field (field &rest args)
  (declare (ignore args))
  (let ((frame *application-frame*))
    (and
     (selected-field frame)
     (member field (highlighted-fields frame)))))

(define-command (com-move-piece :name t :command-table chime)
    ((field 'field :gesture (:select :tester test-target-field)))
  (let* ((frame *application-frame*)
	 (selected-field (selected-field frame))
	 (piece (piece selected-field)))
    (setf (slot-value field 'piece) piece)
    (setf (slot-value piece 'field) field)
    (setf (slot-value selected-field 'piece) nil)
    (setf (slot-value selected-field 'selected) nil)
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
