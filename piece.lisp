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

(define-presentation-method present ((piece piece) (type piece) stream (view board-view) &key)
  (with-slots (image-path) piece
    (let ((pattern (climi::%collapse-pattern
		    (make-pattern-from-bitmap-file image-path :format :png)
		    0 0 46 46)))
      (draw-pattern* stream pattern 2 2))))
