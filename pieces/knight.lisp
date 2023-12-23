(in-package :chime)

(defclass knight (piece)
  ((kind :initform "knight")))

(defmethod possible-moves ((knight knight) (board board))
  '())
