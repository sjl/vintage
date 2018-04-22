(in-package :vintage)


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable solid))

(defun make-player (row col)
  (create-entity 'player
    :renderable/glyph #\@
    :loc/row row
    :loc/col col))


;;;; Cash Register ------------------------------------------------------------
(define-entity cash-register (loc renderable solid))

(defun make-cash-register (row col)
  (create-entity 'cash-register
    :renderable/glyph #\$
    :renderable/color +yellow-black+
    :renderable/attrs +bold+
    :loc/row row
    :loc/col col))
