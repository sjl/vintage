(in-package :vintage)


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable solid flavor carrier purse))

(defun make-player (row col)
  (create-entity 'player
    :flavor/name "yourself"
    :flavor/article :none
    :renderable/glyph #\@
    :renderable/fg-color +black+
    :renderable/bg-color +white+
    :renderable/attrs +bold+
    :purse/dollars 2000
    :loc/row row
    :loc/col col))

;;;; Cash Register ------------------------------------------------------------
(define-entity cash-register (loc renderable solid flavor))

(defun make-cash-register (row col)
  (create-entity 'cash-register
    :flavor/name "old cash register"
    :renderable/glyph #\$
    :renderable/fg-color +yellow+
    :renderable/attrs +bold+
    :loc/row row
    :loc/col col))


;;;; Tables -------------------------------------------------------------------
(define-entity table (loc renderable solid flavor container carryable))

(defun make-table (row col)
  (create-entity 'table
    :flavor/name "wooden table"
    :renderable/glyph #\space
    :renderable/fg-color +black+
    :renderable/bg-color +yellow+
    :container/capacity 1
    :container/on-or-in "on"
    :loc/row row
    :loc/col col))

(defmethod rendering-data ((table table))
  (if-let ((object (first (container/contents table))))
    (values
      (renderable/glyph object)
      (renderable/fg-color object)
      (renderable/bg-color table)
      (renderable/attrs object))
    (call-next-method)))


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
    :flavor/name "toilet"
    :renderable/glyph #\u
    :loc/row row
    :loc/col col))


;;;; Sink ---------------------------------------------------------------------
(define-entity sink (loc renderable solid flavor))

(defun make-sink (row col)
  (create-entity 'sink
    :flavor/name "sink"
    :renderable/glyph #\O
    :loc/row row
    :loc/col col))


;;;; Stairs -------------------------------------------------------------------
(define-entity stairs (loc renderable flavor))

(defun make-stairs (row col)
  (create-entity 'stairs
    :flavor/name "stairs to your loft"
    :flavor/article :definite
    :renderable/glyph #\<
    :loc/row row
    :loc/col col))


;;;; Door ---------------------------------------------------------------------
(define-entity door (loc renderable flavor))

(defun make-door (row col)
  (create-entity 'door
    :flavor/name "door"
    :renderable/glyph #\=
    :renderable/fg-color +yellow+
    :loc/row row
    :loc/col col))


(defmethod renderp ((entity door))
  (with-loc entity
    (let ((other (find entity (entities-at row col) :test-not #'eql)))
      (not other))))


;;;; Window -------------------------------------------------------------------
(define-entity window (loc renderable flavor solid))

(defun make-window (row col)
  (create-entity 'window
    :flavor/name "window"
    :renderable/glyph #\~
    :renderable/fg-color +blue+
    :loc/row row
    :loc/col col))


;;;; Window -------------------------------------------------------------------
(define-entity chair (loc renderable flavor))

(defun make-chair (row col)
  (create-entity 'chair
    :flavor/name "office chair"
    :renderable/glyph #\L
    :loc/row row
    :loc/col col))

