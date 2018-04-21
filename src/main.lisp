(in-package :vintage)


;;;; Utils --------------------------------------------------------------------
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


;;;; Logging ------------------------------------------------------------------
(defvar *log* nil)

(defun l (&rest forms)
  (vector-push-extend forms *log*)
  (first forms))

(defmacro with-logging (&body body)
  `(progn (setf *log* (make-array 16 :fill-pointer 0))
          ,@body))


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


;;;; Title --------------------------------------------------------------------
(defparameter *title*
  (read-lines "assets/title" :omit-empty t))

(define-state title
  (boots:with-layer (:height (length *title*)
                     :width (length (first *title*)))
      (boots:canvas () (rcurry #'draw-lines *title*))
    (boots:blit)
    (boots:read-event)
    (transition intro)))


;;;; Intro --------------------------------------------------------------------
(defparameter *intro*
  (read-lines "assets/intro" :omit-empty nil))

(define-state intro
  (boots:with-layer (:width 60 :height (+ 2 (length (word-wrap *intro* 58))))
      (boots:canvas ()
                    (lambda (c)
                      (boots:clear c)
                      (boots:border c)
                      (draw-lines c (word-wrap *intro* (- (boots:width c) 2))
                                  1 1)))
    (boots:blit)
    (wait-for-event #\Space)))


;;;; Main ---------------------------------------------------------------------
(defun global-input-hook (event)
  (l event)
  (case event
    (#\~ (boots:blit) nil)
    (t t)))

(defun run ()
  (with-logging
    (boots:with-boots
      (let ((boots:*global-input-hook* 'global-input-hook))
        (title)))))

(defun toplevel ()
  (run))
