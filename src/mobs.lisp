(in-package :vintage)

(define-entity mob (loc renderable solid flavor carrier purse ticking brain))

(defun make-mob ()
  (create-entity 'mob
    :flavor/name "antique collector"
    :renderable/glyph #\@
    :renderable/fg-color (random-elt (list +green+ +red+ +blue+ +yellow+))
    :purse/dollars 600))

(defun generate-mobs ()
  (do-repeat 5 (make-mob)))

(defmethod tick ((mob mob))
  (act mob))


(defmethod brain-state! ((mob mob) (state (eql :start)))
  (transition-brain mob :home t))

(defmethod brain-state! ((mob mob) (state (eql :home)))
  (if (randomp 0.01)
    (transition-brain mob :entering t)
    (incf (purse/dollars mob) 1)))

(defmethod brain-state! ((mob mob) (state (eql :entering)))
  (if-let ((entrance (random-elt (free-entrances))))
    (progn (move mob (car entrance) (cdr entrance))
           (transition-brain mob :walk-to-register))
    (transition-brain mob :home)))

(defmethod brain-state! ((mob mob) (state (eql :walk-to-register)))
  (destructuring-bind (row . col) *in-front-of-register*
    (if (entity-at-p mob row col)
      (transition-brain mob :leaving)
      (walk-toward mob *in-front-of-register*))))

(defmethod brain-state! ((mob mob) (state (eql :leaving)))
  (if (some (lambda (entrance)
              (entity-at-p mob (car entrance) (cdr entrance)))
            *entrances*)
    (transition-brain mob :exit t)
    (walk-toward mob (random-elt *entrances*))))

(defmethod brain-state! ((mob mob) (state (eql :exit)))
  (move mob nil nil)
  (transition-brain mob :home))

(defun walk-toward (mob destination)
  (destructuring-bind (row . col) destination
    (let ((path (astar :start (cons (loc/row mob) (loc/col mob))
                       :neighbors (lambda (l)
                                    (neighbors (car l) (cdr l)
                                               :passable t))
                       :goalp (curry #'equal (cons row col))
                       :cost (constantly 1)
                       :heuristic (lambda (l)
                                    (destructuring-bind (r . c) l
                                      (chebyshev-distance r c row col)))
                       :test #'equal)))
      (if path
        (destructuring-bind (r . c) (first (rest path))
          (if (passablep r c)
            (progn (move mob r c)
                   t)
            nil))
        nil))))

