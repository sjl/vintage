(in-package :vintage)

;;;; State --------------------------------------------------------------------
(defparameter *map*
  (read-lines "assets/map" :omit-empty t))

(defvar *map-height* nil)
(defvar *map-width* nil)

(defvar *initial-player-row* 0)
(defvar *initial-player-col* 0)

(defvar *terrain* nil) ; array of strings
(defvar *terrain-colors* nil)


;;;; Code ---------------------------------------------------------------------
(defun terrain (row col)
  (aref (aref *terrain* row) col))

(defun set-terrain (row col new-value)
  (setf (aref (aref *terrain* row) col) new-value))

(defsetf terrain set-terrain)


(defmacro do-terrain ((cell &optional row col) &body body)
  (let ((row (or row (gensym "ROW")))
        (col (or col (gensym "COL"))))
    `(dotimes (,row *map-height*)
       (dotimes (,col *map-width*)
         (symbol-macrolet ((,cell (terrain ,row ,col)))
           ,@body)))))


(defun load-initial-player-location ()
  (do-terrain (cell row col)
    (when (eql #\@ cell)
      (setf *initial-player-row* row
            *initial-player-col* col
            cell #\Space)
      (return))))

(defun cell-color (char)
  (case char
    (#\= +yellow-black+)
    (#\$ +yellow-black+)
    (#\~ +blue-black+)))

(defun cell-attrs (char)
  (case char
    (#\$ charms/ll:A_BOLD)
    (t 0)))

(defun load-terrain-colors ()
  (setf *terrain-colors* nil)
  (do-terrain (cell row col)
    (when-let ((color (cell-color cell)))
      (push (list row col color (cell-attrs cell))
            *terrain-colors*))))

(defun color-terrain (canvas)
  ;; this sucks
  (iterate (for (row col color attrs) :in *terrain-colors*)
           (charms/ll:mvwchgat (charms::window-pointer (boots::window canvas))
                               row col 1 attrs color (cffi:null-pointer))))

(defun draw-terrain (canvas)
  (draw-lines canvas *terrain* 0 0)
  (color-terrain canvas))


(defun load-terrain ()
  (setf *map-height* (length *map*)
        *map-width* (length (first *map*))
        *terrain* (make-array *map-height* :initial-contents *map*))
  (load-initial-player-location)
  (load-terrain-colors)
  t)


(defun in-bounds-p (row col)
  (and (in-range-p 0 row *map-height*)
       (in-range-p 0 col *map-width*)))

(defun passablep (row col)
  (ensure-boolean
    (position (terrain row col) " =Lu<")))

