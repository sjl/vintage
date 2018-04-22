(in-package :vintage)


;;;; State --------------------------------------------------------------------
(defvar *player* nil)


;;;; UI -----------------------------------------------------------------------
(defun center (canvas line)
  (max 0 (- (truncate (boots:width canvas) 2)
            (truncate (length line) 2))))

(defun popup (canvas text &optional header footer)
  (boots:clear canvas)
  (boots:border canvas)
  (draw-lines canvas
              (word-wrap (ensure-list text)
                         (- (boots:width canvas) 2))
              1 1)
  (when header
    (boots:draw canvas 0 (center canvas header) header))
  (when footer
    (boots:draw canvas (1- (boots:height canvas)) (center canvas footer) footer)))


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
(defun direction-to-offsets (move)
  (ecase move
    (:w  (values  0 -1))
    (:s  (values  1  0))
    (:n  (values -1  0))
    (:e  (values  0  1))
    (:nw (values -1 -1))
    (:ne (values -1  1))
    (:sw (values  1 -1))
    (:se (values  1  1))))

(defun move-player (direction)
  (nest
    (multiple-value-bind (dr dc) (direction-to-offsets direction))
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
    (run-render))
  (boots:move-cursor canvas (loc/row *player*) (loc/col *player*)))

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


(defun event-to-direction (event)
  (ecase event
    ((#\h :left)  :w)
    ((#\j :down)  :s)
    ((#\k :up)    :n)
    ((#\l :right) :e)
    (#\y          :nw)
    (#\u          :ne)
    (#\b          :sw)
    (#\n          :se)))

(defun parse-input (event)
  (case event
    (#\q '(:quit))
    ((#\h #\j #\k #\l #\y #\u #\b #\n :up :down :left :right)
     (list :move (event-to-direction event)))
    (#\? '(:help))))


(defun draw-help (canvas)
  (popup canvas
         '("Bump into things to interact with them."
           ""
           "CONTROLS"
           ""
           "[hjklyubn/arrows] move"
           "[?] help"
           "[q] quit")
         "HELP" "Press any key"))

(defun show-help ()
  (boots:with-layer (:width 40 :height 12)
      (boots:canvas () 'draw-help)
    (boots:blit)
    (boots:read-event)))

(define-state game-loop ()
  (boots:with-layer () (game-ui)
    (iterate
      (boots:blit)
      (for event = (parse-input (boots:read-event)))
      (case (first event)
        (:quit (return-from game-loop))
        (:move (move-player (second event)))
        (:help (show-help))))))


;;;; Main ---------------------------------------------------------------------
(defun global-input-hook (event)
  (case event
    (#\~ (boots:blit) nil)
    (#\= (error "Bork"))
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
