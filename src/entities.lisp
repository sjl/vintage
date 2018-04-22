(in-package :vintage)


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable))

(defun make-player (row col)
  (create-entity 'player
    :renderable/glyph #\@
    :loc/row row
    :loc/col col))
