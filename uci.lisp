(in-package :chime)

(defclass uci ()
  ((executable-path :initarg :executable-path
		    :reader executable-path)
   subprocess
   input-stream
   output-stream
   (options :initarg :options
	    :reader uci-options)))

(defmethod initialize-instance :after ((uci uci) &rest initargs)
  (with-slots (executable-path subprocess input-stream output-stream) uci
    (setf subprocess (uiop:launch-program executable-path
					  :input :stream
					  :output :stream))
    (setf input-stream (uiop:process-info-input subprocess))
    (setf output-stream (uiop:process-info-output subprocess))
    (set-options uci)))

(defmethod set-option ((uci uci) option)
  (with-slots (input-stream) uci
    (format input-stream
	    "setoption name ~A value ~A~%"
	    (car option) (cdr option))
    (force-output input-stream)))

(defmethod set-options ((uci uci))
  (with-slots (options) uci
    (dolist (option options)
      (set-option uci option))))

(defmethod wait-ready ((uci uci))
  (with-slots (input-stream output-stream) uci
    (format input-stream "isready~%")
    (force-output input-stream)
    (loop for line = (read-line output-stream)
	  until (and (>= (length line) 7)
		     (string= (subseq line 0 7) "readyok")))))

(defmethod setup ((uci uci))
  (with-slots (input-stream) uci
    (format input-stream "uci~%")
    (format input-stream "ucinewgame~%")
    (force-output input-stream)
    (set-options uci)))

(defmethod play-move ((uci uci) (move move))
  (with-slots (input-stream) uci
    (let ((fen (get-fen uci))
	  (uci-move (to-uci move)))
      (format input-stream
	      "position fen ~A moves ~A~%"
	      fen uci-move)
      (force-output input-stream))))

(defmethod get-fen ((uci uci))
  (with-slots (input-stream output-stream) uci
    (format input-stream "d~%")
    (force-output input-stream)
    (loop for line = (read-line output-stream)
	  while line
	  when (and (>= (length line) 3)
			(string= (subseq line 0 3) "Fen"))
	    return (subseq line 5))))

(defmethod get-best-move ((uci uci) (board board) &key (depth 5))
  (with-slots (input-stream output-stream) uci
    (format input-stream "go depth ~d~%" depth)
    (force-output input-stream)
    (let ((bestmove-line
	    (loop for line = (read-line output-stream)
		  while line
		  when (string= (subseq line 0 8) "bestmove")
		    return line)))
      (from-uci (cadr (uiop:split-string bestmove-line :separator " "))
		board))))
