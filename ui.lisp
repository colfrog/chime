(in-package :chime)

(defun draw (frame stream)
  (present (board frame) 'board :stream stream))

(define-application-frame chime ()
  ((current-player :initform "white"
		   :accessor current-player)
   (board :initform (make-instance 'board)
	  :accessor board))
  (:panes
   (app :application
	:scroll-bars nil
	:width (* 8 50)
	:height (* 8 50)
	:max-width (* 8 50)
	:max-height (* 8 50)
	:display-function 'draw
	:default-view (make-instance 'board-view))))

(defun main ()
  (run-frame-top-level
   (make-instance 'chime)))
