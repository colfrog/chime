(in-package :chime)

(defclass queen (piece)
  ((kind :initform "queen")))

(defmethod possible-moves ((queen queen) (board board))
  (concatenate 'list (lines queen board) (diagonals queen board)))
