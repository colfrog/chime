(in-package :chime)

(defclass bishop (piece)
  ((kind :initform "bishop")))

(defmethod possible-moves ((bishop bishop) (board board))
  (diagonals bishop board))
