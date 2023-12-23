(in-package :chime)

(defclass rook (piece)
  ((kind :initform "rook")))

(defmethod possible-moves ((rook rook) (board board))
  '())
