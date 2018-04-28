(in-package :vintage)

(define-entity mob (loc renderable solid flavor carrier purse ticking brain))

(defun make-mob ()
  (create-entity 'mob
    :flavor/name "antique collector"
    :renderable/glyph #\@
    :purse/dollars 600))

(defun generate-mobs ()
  (do-repeat 5 (make-mob)))

(defmethod tick ((mob mob))
  (act mob))


(defmethod brain-state! ((mob mob) (state (eql :start)))
  (transition-brain mob :home t))

(defmethod brain-state! ((mob mob) (state (eql :home)))
  (if (randomp 0.05)
    (transition-brain mob :entering t)
    (incf (purse/dollars mob) 1)))

(defmethod brain-state! ((mob mob) (state (eql :entering)))
  (if-let ((entrance (random-elt (free-entrances))))
    (progn (move mob (car entrance) (cdr entrance))
           (transition-brain mob :browsing))
    (transition-brain mob :home)))

(defmethod brain-state! ((mob mob) (state (eql :browsing)))
  (if (randomp 0.05)
    (progn
      (move mob nil nil)
      (transition-brain mob :home))
    nil))



