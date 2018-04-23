(in-package :vintage)

;;;; Location -----------------------------------------------------------------
(define-aspect loc
  (row :type (or null (integer 0)))
  (col :type (or null (integer 0))))

(defmacro with-loc (entity-and-symbols &body body)
  (destructuring-bind (entity &optional (row-symbol 'row) (col-symbol 'col))
      (ensure-list entity-and-symbols)
    `(with-accessors ((,row-symbol loc/row)
                      (,col-symbol loc/col))
       ,entity
       ,@body)))

(defun in-loc-bounds-p (row col)
  (and (in-range-p 0 col *map-width*)
       (in-range-p 0 row *map-height*)))

(defun initialize-locations ()
  (setf *locations* (make-array (list *map-height* *map-width*)
                     :initial-element nil)))

(defun loc-insert (entity)
  (with-loc entity
    (push entity (aref *locations* row col))))

(defun loc-remove (entity)
  (with-loc entity
    (deletef (aref *locations* row col) entity)))

(defun move (entity new-row new-col)
  (with-loc entity
    (when row (loc-remove entity))
    (setf row new-row
          col new-col)
    (when new-row (loc-insert entity))))

(defun entities-at (row col)
  (when (in-loc-bounds-p row col)
    (aref *locations* row col)))

(defmethod entity-created :after ((entity loc))
  (with-loc entity
    (when row (loc-insert entity))))

(defmethod entity-destroyed :after ((entity loc))
  (with-loc entity
    (when row (loc-remove entity))))


;;;; Renderable ---------------------------------------------------------------
(define-aspect renderable
  (glyph :type character)
  (color :type fixnum :initform +white-black+)
  (attrs :type (integer 0) :initform 0))

(define-with-macro (renderable :conc-name renderable/) glyph color attrs)

(define-system render ((entity renderable loc))
  (nest
    (with-loc entity)
    (when row)
    (with-renderable (entity))
    (with-color (*render-canvas* color attrs))
    (boots:draw *render-canvas* row col
                (string glyph))))


;;;; Solid --------------------------------------------------------------------
(define-aspect solid)


;;;; Flavor -------------------------------------------------------------------
(define-aspect flavor
  (name :type string))

