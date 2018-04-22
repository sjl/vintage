(in-package :vintage)


;;;; State --------------------------------------------------------------------
(defvar *player* nil)


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

(defun generate-player ()
  (setf *player* (make-player *initial-player-row* *initial-player-col*)))

(define-state world-generation
  (boots:with-layer ()
      (boots:canvas () 'draw-loading-screen)
    (boots:blit)
    (progn
      (init-messages)
      (clear-entities)
      (load-terrain)
      (generate-player))
    (sleep 1/5)
    (transition game-loop)))


;;;; Player Control -----------------------------------------------------------
(defun event-to-direction (move)
  (ecase move
    ((#\h :left)  (values  0 -1))
    ((#\j :down)  (values  1  0))
    ((#\k :up)    (values -1  0))
    ((#\l :right) (values  0  1))
    (#\y          (values -1 -1))
    (#\u          (values -1  1))
    (#\b          (values  1 -1))
    (#\n          (values  1  1))))

(defun move-player (move)
  (nest
    (multiple-value-bind (dr dc) (event-to-direction move))
    (with-loc (*player*))
    (let ((r (+ row dr))
          (c (+ col dc))))
    (cond ((not (in-bounds-p r c))
           (message "You can't leave the shop unattended."))
          ((not (passablep r c))
           (message "There's something in the way."))
          (t (setf row r col c)))))


;;;; Game Loop ----------------------------------------------------------------
(defun draw-status (canvas)
  (boots:clear canvas)
  (let ((time-string (format nil "TIME: 00:00"))
        (cash-string (format nil "$2,300"))
        (w (boots:width canvas)))
    (boots:draw canvas 0 0 time-string)
    (boots:draw canvas 0 (- w (length cash-string)) cash-string)))

(defun draw-messages (canvas)
  (boots:clear canvas)
  ;; this is shitty because we want to iterate backwards and draw the latest
  ;; lines at the end of the log, and also want to word wrap them.  im sorry.
  (iterate
    (with w = (boots:width canvas))
    (with h = (boots:height canvas))
    (with r = (1- h))
    (while (plusp r))
    (for message :in-message-log *messages*)
    (for message-lines = (nreverse (word-wrap (list message) (- w 2))))
    (dolist (line message-lines)
      (if (< r 1)
        (return-from draw-messages)
        (progn (boots:draw canvas r 1 line)
               (decf r))))))

(defun draw-entities (canvas)
  (let ((*render-canvas* canvas))
    (run-render)))

(defun draw-map (canvas)
  (boots:clear canvas)
  (draw-terrain canvas)
  (draw-entities canvas))

(defun game-ui ()
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
        (boots:canvas (:width 1) (curry #'boots:fill #\│))
        (boots:stack ()
          (boots:canvas (:height 1) 'draw-status)
          (boots:canvas () 'draw-messages))))))


(define-state game-loop ()
  (boots:with-layer () (game-ui)
    (iterate
      (boots:blit)
      (iterate
        (for e = (boots:read-event))
        (until (case e
                 (#\q (return-from game-loop))
                 (#\x (message "ping") t)
                 (#\r (message "hello my baby hello my honey hello my ragtime gal") t)
                 ((#\h #\j #\k #\l #\y #\u #\b #\n :up :down :left :right)
                  (move-player e)
                  t)))))))


;;;; Main ---------------------------------------------------------------------
(defun global-input-hook (event)
  (case event
    (#\~ (boots:blit) nil)
    (#\= (/ 1 0))
    (:resize nil)
    (t t)))

(defun run ()
  (with-logging
    (boots:with-boots
      (init-colors)
      (let ((boots:*global-input-hook* 'global-input-hook))
        (title)))))


;;;; Toplevel -----------------------------------------------------------------
(defun toplevel ()
  ;; #+sbcl (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (handler-case (run)
    (t (c) (format t "Error: ~A" c))))
