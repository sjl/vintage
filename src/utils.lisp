(in-package :vintage)

(defun read-lines (file &key omit-empty)
  (with-open-file (s file)
    (iterate (for line :in-stream s :using #'read-line)
             (unless (and omit-empty (zerop (length line)))
               (collect line)))))

(defun draw-lines (canvas lines &optional (row 0) (col 0))
  (iterate (for r :from row)
           (for line :in-whatever lines)
           (boots:draw canvas r col line)))

(defmacro wait-for-event (goal)
  (once-only (goal)
    `(iterate
       (thereis (eql ,goal (boots:read-event))))))


;;;; State Machines -----------------------------------------------------------
(defmacro define-state-machine-macros ()
  (with-gensyms (next-state transition recur main)
    `(progn
       (defmacro define-state (state-name &body body)
         `(defun ,state-name ()
            (let (,',next-state)
              (tagbody
                (go ,',main)
                ,',recur (return-from ,state-name (funcall ',state-name))
                ,',transition (return-from ,state-name (funcall ,',next-state))
                ,',main ,@body))))

       (defmacro transition (next-state)
         `(progn (setf ,',next-state ',next-state) (go ,',transition)))

       (defmacro reenter ()
         `(go ,',recur)))))

(define-state-machine-macros)

