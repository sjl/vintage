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


;;;; Terrain ------------------------------------------------------------------
(defparameter *map*
  (read-lines "assets/map" :omit-empty t))

(defvar *map-height* nil)
(defvar *map-width* nil)

(defparameter *terrain* nil)

(defun load-terrain ()
  (setf *map-height* (length *map*)
        *map-width* (length (first *map*))
        *terrain* (make-array (list *map-height* *map-width*)
                              :initial-contents *map*)))


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
    (wait-for-event #\Space)
    (transition world-generation)))


;;;; World Generation ---------------------------------------------------------
(defun draw-loading-screen (canvas)
  (boots:clear canvas)
  (boots:draw canvas 0 0 "Loading, please wait..."))

(define-state world-generation
  (boots:with-layer ()
      (boots:canvas () 'draw-loading-screen)
    (boots:blit)
    (load-terrain)
    (sleep 1/5)
    (transition game-loop)))


;;;; Game Loop ----------------------------------------------------------------
(defun draw-status (canvas)
  (boots:clear canvas)
  (let ((time-string (format nil "TIME: 00:00"))
        (cash-string (format nil "$2,300"))
        (w (boots:width canvas)))
    (boots:draw canvas 0 0 time-string)
    (boots:draw canvas 0 (- w (length cash-string)) cash-string)))

(defun draw-messages (canvas)
  (l "hi")
  (boots:clear canvas)
  (iterate (for message :in-whatever *log*)
           (for r :from 1 :below (boots:height canvas))
           (boots:draw canvas r 2 "~S" message)))

(defun draw-terrain (canvas)
  (draw-lines canvas *map* 0 0))

(defun draw-map (canvas)
  (boots:clear canvas)
  (draw-terrain canvas))

(define-state game-loop ()
  (l *map-width*)
  (boots:with-layer ()
      (boots:shelf ()
        (boots:stack ()
          (boots:canvas () #'boots:clear)
          (boots:shelf (:height *map-height*)
            (boots:canvas () #'boots:clear)
            (boots:canvas (:width *map-width*) 'draw-map)
            (boots:canvas () #'boots:clear))
          (boots:canvas () #'boots:clear))
        (boots:stack (:width 0.3)
          (boots:shelf ()
            (boots:canvas (:width 1)
                          (curry #'boots:fill #\box_drawings_light_vertical))
            (boots:stack ()
              (boots:canvas (:height 1) 'draw-status)
              (boots:canvas () 'draw-messages)))))
    (boots:blit)
    (boots:read-event)))


;;;; Main ---------------------------------------------------------------------
(defun global-input-hook (event)
  (case event
    (#\~ (boots:blit) nil)
    (:resize nil)
    (t t)))

(defun run ()
  (with-logging
    (boots:with-boots
      (let ((boots:*global-input-hook* 'global-input-hook))
        (title)))))

(defun toplevel ()
  (run))
