(in-package :vintage)


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable solid flavor))

(defun make-player (row col)
  (create-entity 'player
    :flavor/name "yourself"
    :renderable/glyph #\@
    :loc/row row
    :loc/col col))


;;;; Cash Register ------------------------------------------------------------
(define-entity cash-register (loc renderable solid flavor))

(defun make-cash-register (row col)
  (create-entity 'cash-register
    :flavor/name "an old cash register"
    :renderable/glyph #\$
    :renderable/color +yellow-black+
    :renderable/attrs +bold+
    :loc/row row
    :loc/col col))


;;;; Tables -------------------------------------------------------------------
(define-entity table (loc renderable solid flavor))

(defun make-table (row col)
  (create-entity 'table
    :flavor/name "a wooden table"
    :renderable/glyph #\space
    :renderable/color +black-yellow+
    :loc/row row
    :loc/col col))


;;;; Computer -----------------------------------------------------------------
(define-entity computer (loc renderable solid))

(defun make-computer (row col)
  (create-entity 'computer
    :renderable/glyph #\#
    :loc/row row
    :loc/col col))

;;;; Toilet -------------------------------------------------------------------
(define-entity toilet (loc renderable flavor))

(defun make-toilet (row col)
  (create-entity 'toilet
    :flavor/name "a toilet"
    :renderable/glyph #\u
    :loc/row row
    :loc/col col))

;;;; Sink ---------------------------------------------------------------------
(define-entity sink (loc renderable solid flavor))

(defun make-sink (row col)
  (create-entity 'sink
    :flavor/name "a sink"
    :renderable/glyph #\O
    :loc/row row
    :loc/col col))

;;;; Stairs -------------------------------------------------------------------
(define-entity stairs (loc renderable flavor))

(defun make-stairs (row col)
  (create-entity 'stairs
    :flavor/name "the stairs to your loft"
    :renderable/glyph #\<
    :loc/row row
    :loc/col col))

