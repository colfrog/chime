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

(defgeneric possible-moves (piece fields)
  (:documentation "Returns a list of possible moves"))

(defgeneric is-possible-move (piece fields move)
  (:documentation "Checks if the move is a possible move"))

(define-presentation-method present ((piece piece) (type piece) stream (view board-view) &key)
  (with-slots (image-path) piece
    (let ((pattern (make-pattern-from-bitmap-file image-path :format :png)))
      (draw-pattern* stream pattern 2 2))))
