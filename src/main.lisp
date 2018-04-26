(in-package :vintage)


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
(define-state title
  (boots:with-layer (:height (length *asset-title*)
                     :width (length (first *asset-title*)))
      (boots:canvas () (rcurry #'draw-lines *asset-title*))
    (boots:blit)
    (boots:read-event)
    (transition intro)))


;;;; Intro --------------------------------------------------------------------
(define-state intro
  (boots:with-layer (:width 60 :height (+ 2 (length (word-wrap *asset-intro* 58))))
      (boots:canvas ()
                    (lambda (c)
                      (boots:clear c)
                      (boots:border c)
                      (draw-lines c (word-wrap *asset-intro* (- (boots:width c) 2))
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
    (progn
      (init-messages)
      (clear-entities)
      (load-terrain))
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
    (let ((r (+ (loc/row *player*) dr))
          (c (+ (loc/col *player*) dc))))
    (cond ((not (in-bounds-p r c))
           (message "You can't leave the shop unattended."))
          ((not (passablep r c))
           (message "There's something in the way."))
          (t (move *player* r c)))))

(defun move-look (direction)
  (nest
    (multiple-value-bind (dr dc) (direction-to-offsets direction))
    (let ((r (+ *look-row* dr))
          (c (+ *look-col* dc))))
    (when (in-bounds-p r c)
      (setf *look-row* r *look-col* c))))


;;;; Game Loop ----------------------------------------------------------------
(defun draw-hud (canvas)
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
  (if *looking*
    (boots:move-cursor canvas *look-row* *look-col*)
    (boots:move-cursor canvas (loc/row *player*) (loc/col *player*))))

(defun draw-map (canvas)
  (boots:clear canvas)
  (draw-terrain canvas)
  (draw-entities canvas))

(defun draw-status (canvas)
  (boots:clear canvas)
  (when *looking*
    (when-let ((visible (-<> (entities-at *look-row* *look-col*)
                          (remove-if-not #'flavor? <>)
                          (mapcar #'flavor/name <>))))
      (draw-lines canvas
                  (word-wrap (list (format nil "You see ~A"
                                           (english-list visible)))
                             (1- (boots:width canvas)))))))


(defun game-ui ()
  (boots:shelf ()
    (boots:stack ()
      (boots:canvas () 'draw-status)
      (boots:shelf (:height *map-height*)
        (boots:canvas () #'boots:clear)
        (boots:canvas (:width *map-width*) 'draw-map)
        (boots:canvas () #'boots:clear))
      (boots:canvas () #'boots:clear))
    (boots:stack (:width 0.3)
      (boots:shelf ()
        (boots:canvas (:width 1) (curry #'boots:fill #\│))
        (boots:stack ()
          (boots:canvas (:height 1) 'draw-hud)
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

(defun parse-input-main (event)
  (case event
    (#\q '(:quit))
    (#\L '(:look))
    (#\R '(:reload))
    ((#\h #\j #\k #\l #\y #\u #\b #\n :up :down :left :right)
     (list :move (event-to-direction event)))
    (#\? '(:help))
    (t (message (structural-string event)) nil)))

(defun parse-input-look (event)
  (case event
    ((#\h #\j #\k #\l #\y #\u #\b #\n :up :down :left :right)
     (list :move (event-to-direction event)))))


(defparameter help-text
  '("Bump into things to interact with them."
    ""
    "CONTROLS"
    ""
    "[hjklyubn/arrows] move"
    "[L] look"
    "[?] help"
    "[q] quit"))

(defun draw-help (canvas)
  (popup canvas help-text "HELP" "Press any key"))

(defun show-help ()
  (boots:with-layer
      (:width 40 :height (+ 2 (length (word-wrap help-text 38)) 1))
      (boots:canvas () 'draw-help)
    (boots:blit)
    (boots:read-event)))


(defun look ()
  (let ((*looking* t)
        (*look-row* (loc/row *player*))
        (*look-col* (loc/col *player*)))
    (iterate
      (boots:blit)
      (for event = (parse-input-look (boots:read-event)))
      (case (first event)
        (:move (move-look (second event)))
        (t (return-from look))))))

(define-state game-loop ()
  (boots:with-layer () (game-ui)
    (iterate
      (boots:blit)
      (for event = (parse-input-main (boots:read-event)))
      (case (first event)
        (:quit (return-from game-loop))
        (:reload (ql:quickload :vintage :silent t))
        (:look (look))
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
  (setf *random-state* (make-random-state t))
  (boots:with-boots
    (init-colors)
    (let ((boots:*global-input-hook* 'global-input-hook))
      (title))))


;;;; Toplevel -----------------------------------------------------------------
(defun toplevel ()
  ;; #+sbcl (sb-ext:disable-debugger)
  (handler-case (run)
    (t (c) (format t "Error: ~A" c))))
