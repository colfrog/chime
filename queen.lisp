(in-package :chime)

(defclass queen (piece)
  ((kind :initform "queen")))

(defmethod possible-moves ((queen queen) (board board))
  '())
