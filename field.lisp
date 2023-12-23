(in-package :chime)

(defclass field ()
  ((col :initarg :col
	:reader col)
   (row :initarg :row
	:reader row)
   (highlighted :initform nil
		:accessor highlighted)
   (selected :initform nil
	     :accessor selected)
   (piece :initarg :piece
	  :accessor piece)))

(defmethod get-position ((field field))
  (with-slots (col row) field
    (cons col row)))

(define-presentation-method present ((field field) (type field) stream (view board-view) &key)
  (with-slots (col row piece highlighted selected) field
    (let ((colour (if (evenp (+ row col)) +pale-goldenrod+ +dark-khaki+)))
      (draw-rectangle* stream 0 0 49 49 :ink colour)
      (when highlighted
	(draw-rectangle* stream 0 0 49 49 :ink +red+ :filled nil :line-thickness 2))
      (when selected
	(draw-rectangle* stream 0 0 49 49 :ink +black+ :filled nil :line-thickness 2))
      (when piece
	(present piece 'piece :stream stream :view view)))))
