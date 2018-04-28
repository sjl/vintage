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

(defun move-cursor (row col)
  (setf *cursor-row* row *cursor-col* col))


;;;; Time ---------------------------------------------------------------------
(defun to-nsec (internal-time-units)
  (-<> internal-time-units
    (/ <> internal-time-units-per-second) ; seconds
    (* <> 1000000000) ; nanoseconds 
    truncate))

(defun increment-clock (internal-time-units)
  (timestamp-incf *current-time* (* *time-speed* (to-nsec internal-time-units))
                  :nsec))

(defmacro clocking (&body body)
  (with-gensyms (start result end)
    `(let ((,start (get-internal-real-time))
           (,result (progn ,@body))
           (,end (get-internal-real-time)))
       (increment-clock (- ,end ,start))
       ,result)))


(defun clocking-read-event ()
  (iterate (thereis (clocking (boots:read-event 1/2)))
           (boots:blit)))


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
      (setf *current-time* (local-time:now))
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

(defun resolve-direction (direction &optional
                          (row (loc/row *player*)) (col (loc/col *player*)))
  (multiple-value-bind (dr dc)
      (direction-to-offsets direction)
    (values (+ row dr)
            (+ col dc))))

(defun move-player (direction)
  (multiple-value-bind (r c) (resolve-direction direction)
    (cond ((not (in-bounds-p r c))
           (message "You can't leave the shop unattended."))
          ((not (passablep r c))
           (message "There's something in the way."))
          (t (move *player* r c)))))

(defun move-look (direction)
  (multiple-value-bind (r c)
      (resolve-direction direction *look-row* *look-col*)
    (when (in-bounds-p r c)
      (setf *look-row* r *look-col* c))))


;;;; Game Loop ----------------------------------------------------------------
(defparameter *clock-format* '(:hour12 ":" (:min 2 #\0) " " :ampm))
(defparameter *time-speed* 20)

(defun clock-string ()
  (string-upcase (local-time:format-timestring
                   nil *current-time* :format *clock-format*)))

(defun draw-hud (canvas)
  (boots:clear canvas)
  (let ((time-string (clock-string))
        (cash-string (format nil "$2,300"))
        (carrying-string (when-let ((object (carrier/holding *player*)))
                           (format nil "CARRYING: ~A" (flavor/name object))))
        (w (boots:width canvas)))
    (boots:draw canvas 0 0 time-string)
    (boots:draw canvas 0 (- w (length cash-string)) cash-string)
    (when carrying-string
      (boots:draw canvas 1 0 carrying-string))))

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
    (map-entities #'render 'renderable)
    (render *player*)
    (boots:move-cursor canvas *cursor-row* *cursor-col*)))

(defun draw-map (canvas)
  (boots:clear canvas)
  (draw-terrain canvas)
  (draw-entities canvas))

(defun draw-status (canvas)
  (boots:clear canvas)
  (when *looking*
    (when-let ((visible (-<> (entities-at *look-row* *look-col*)
                          (remove-if-not #'flavor? <>)
                          (mapcar #'name-with-article <>))))
      (draw-lines canvas
                  (word-wrap (list (format nil "You see ~A."
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
          (boots:canvas (:height 2) 'draw-hud)
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
    ((#\h #\j #\k #\l #\y #\u #\b #\n :up :down :left :right)
     (list :move (event-to-direction event)))
    (#\q '(:quit))
    (#\L '(:look))
    (#\R '(:reload))
    (#\g '(:get))
    (#\d '(:drop))
    (#\p '(:place))
    (#\t '(:take))
    (#\? '(:help))
    (t (message (structural-string event)) nil)))

(defun parse-input-select-direction (event)
  (case event
    ((#\h #\j #\k #\l #\y #\u #\b #\n :up :down :left :right)
     (event-to-direction event))
    (t nil)))

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
    "[g] get"
    "[d] drop"
    "[p] place on/in"
    "[t] take from"
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


(defun look! ()
  (let ((*looking* t)
        (*look-row* (loc/row *player*))
        (*look-col* (loc/col *player*)))
    (iterate
      (move-cursor *look-row* *look-col*)
      (boots:blit)
      (for event = (parse-input-look (boots:read-event)))
      (case (first event)
        (:move (move-look (second event)))
        (t (return-from look!))))))

(defun get! ()
  (when (carrier/holding *player*)
    (return-from get! (message "You are already carrying something.")))

  (message "Which direction?")
  (boots:blit)

  (nest
    (let ((direction (parse-input-select-direction (clocking-read-event)))))
    (if (null direction)
      (message "Nevermind."))

    (multiple-value-bind (r c) (resolve-direction direction))
    (if (not (in-bounds-p r c))
      (message "You can't get anything from there."))

    (let ((objects (remove-if-not #'carryable? (entities-at r c)))))
    (if (null objects)
      (message "There's nothing to get there."))

    (let ((target (first objects))))
    (progn (pick-up *player* target)
           (message "You pick up the ~A" (flavor/name target)))))

(defun drop! ()
  (unless (carrier/holding *player*)
    (return-from drop! (message "You are not carrying anything.")))

  (message "Which direction?")
  (boots:blit)

  (nest
    (let ((direction (parse-input-select-direction (clocking-read-event)))))
    (if (null direction)
      (message "Nevermind."))

    (multiple-value-bind (r c) (resolve-direction direction))
    (if (not (in-bounds-p r c))
      (message "You can't drop something there."))

    (if (not (passablep r c))
      (message "There's something in the way."))

    (-<> (put-down *player* r c)
      flavor/name
      (message "You drop the ~A." <>))))

(defun place! ()
  (let ((object (carrier/holding *player*)))
    (cond
      ((null object)
       (return-from place! (message "You are not carrying anything.")))
      ((not (containable? object))
       (return-from place! (message "You can't put that on/in something else."))))

    (message "Which direction?")
    (boots:blit)

    (nest
      (let ((direction (parse-input-select-direction (clocking-read-event)))))
      (if (null direction)
        (message "Nevermind."))

      (multiple-value-bind (r c) (resolve-direction direction))
      (if (not (in-bounds-p r c))
        (message "You can't put something there."))

      (let ((container (first (remove-if-not #'container? (entities-at r c))))))
      (if (not container)
        (message "There's nothing to put it on (did you mean [d]rop)?"))

      (if (fullp container)
        (message "The ~A is full." (flavor/name container)))

      (progn
        (setf (carrier/holding *player*) nil) ; todo this sucks
        (place-in container object)
        (message "You put the ~A on the ~A."
                 (flavor/name object)
                 (flavor/name container))))))
(defun take! ()
  (when (carrier/holding *player*)
    (return-from take! (message "You are already carrying something.")))

  (message "Which direction?")
  (boots:blit)

  (nest
    (let ((direction (parse-input-select-direction (clocking-read-event)))))
    (if (null direction)
      (message "Nevermind."))

    (multiple-value-bind (r c) (resolve-direction direction))
    (if (not (in-bounds-p r c))
      (message "You can't take anything from there."))

    (let ((container (first (remove-if-not #'container? (entities-at r c))))))
    (if (not container)
      (message "There's nothing to take from (did you mean [g]et)?"))

    (if (emptyp container)
      (message "The ~A is empty." (flavor/name container)))

    (let ((object (first (container/contents container)))))
    (progn
      (pick-up *player* object)
      (remove-from container object)
      (message "You take the ~A from the ~A."
               (flavor/name object)
               (flavor/name container)))))


(define-state game-loop ()
  (boots:with-layer () (game-ui)
    (iterate
      (move-cursor nil nil)
      (boots:blit)
      (for command = (parse-input-main (clocking-read-event)))
      (case (first command)
        (:quit (return-from game-loop))
        (:reload (ql:quickload :vintage :silent t))
        (:look (look!))
        (:get (get!))
        (:drop (drop!))
        (:place (place!))
        (:take (take!))
        (:move (move-player (second command)))
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
