(in-package :vintage)

;;;; Location -----------------------------------------------------------------
(define-aspect loc
  (row :type (or null (integer 0)))
  (col :type (or null (integer 0))))

(defmacro with-loc ((entity &optional (row-symbol 'row) (col-symbol 'col))
                    &body body)
  `(with-accessors ((,row-symbol loc/row)
                    (,col-symbol loc/col))
       ,entity
     ,@body))

;;;; Renderable ---------------------------------------------------------------
(define-aspect renderable
  (glyph :type character))

(defvar *render-canvas* nil)

(define-system render ((entity renderable loc))
  (when (loc/row entity)
    (boots:draw *render-canvas* (loc/row entity) (loc/col entity)
                (string (renderable/glyph entity)))))
