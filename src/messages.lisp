(in-package :vintage)

;; todo make this a ring buffer

(defvar *messages* nil)
(defvar *messages-limit* 10)

(defstruct message
  (string (error "Required") :type string)
  (prev nil :type (or null message))
  (next nil :type (or null message)))

(defstruct message-log
  (size 0 :type (integer 0))
  (head nil :type (or null message))
  (tail nil :type (or null message)))

(define-with-macro message-log size head tail)

(defun init-messages ()
  (setf *messages* (make-message-log)))

(defun append-message (message-log string)
  (with-message-log (message-log)
    (let ((m (make-message :string string :prev tail)))
      (if (zerop size)
        (setf head m tail m)
        (setf (message-next tail) m
              tail m)))
    (incf size)))

(defun pop-message (message-log)
  (with-message-log (message-log)
    (case size
      (0 (error "Cannoy pop from empty message log."))
      (1 (setf head nil tail nil))
      (t (setf head (message-next head)
               (message-prev head) nil)))
    (decf size)))

(defun message (string)
  (when (> (append-message *messages* string) *messages-limit*)
    (pop-message *messages*)))

(defmacro-driver (FOR var IN-MESSAGE-LOG message-log)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (message)
      `(progn
         (generate ,message :next
                   (if (null ,message)
                     (message-log-tail ,message-log)
                     (message-prev ,message)))
         (,kwd ,var = (progn (next ,message)
                             (if ,message
                               (message-string ,message)
                               (terminate))))))))
