(in-package :chime)

(defclass king (piece)
  ((kind :initform "king")))

(defmethod possible-moves ((king king) (board board))
  '())
