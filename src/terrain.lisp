(in-package :vintage)

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


(defun load-terrain-entities ()
  (do-terrain (cell row col)
    (when (case cell
            (#\@ (setf *player* (make-player row col)))
            (#\$ (make-cash-register row col))
            (#\t (make-table row col))
            (#\C (make-computer row col))
            (#\u (make-toilet row col))
            (#\= (make-door row col))
            (#\~ (make-window row col))
            (#\O (make-sink row col))
            (#\L (make-chair row col))
            (#\< (make-stairs row col))
            (#\A (generate-antique row col)))
      (setf cell #\Space))))

(defun draw-terrain (canvas)
  (draw-lines canvas *terrain* 0 0))

(defun load-terrain ()
  (setf *map-height* (length *asset-map*)
        *map-width* (length (first *asset-map*))
        *terrain* (make-array *map-height* :initial-contents *asset-map*))
  (initialize-locations) ; shitty
  (load-terrain-entities)
  t)


(defun in-bounds-p (row col)
  (and (in-range-p 0 row *map-height*)
       (in-range-p 0 col *map-width*)))


(defun terrain-passable-p (row col)
  (eql #\space (terrain row col)))

(defun solid-at-p (row col)
  (ensure-boolean (find-if #'solid? (entities-at row col))))

(defun passablep (row col)
  (and (terrain-passable-p row col)
       (not (solid-at-p row col))))

