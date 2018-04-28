(in-package :vintage.astar)

(defstruct path
  state
  (estimate 0)
  (cost 0)
  (previous nil))

(defun path-to-list (path &aux result)
  (recursively ((path path))
    (unless (null path)
      (push (path-state path) result)
      (recur (path-previous path))))
  result)

(defun astar (&key start neighbors goalp cost heuristic test)
  "Search for a path from `start` to a goal using A★.

  The following parameters are all required:

  * `start`: the starting state.

  * `neighbors`: a function that takes a state and returns all states reachable
    from it.

  * `goalp`: a predicate that takes a state and returns whether it is a goal.

  * `cost`: a function that takes two states `a` and `b` and returns the cost
    to move from `a` to `b`.

  * `heuristic`: a function that takes a state and estimates the distance
    remaining to the goal.

  * `test`: an equality predicate for comparing nodes.  It must be suitable for
    passing to `make-hash-table`.

  If the heuristic function is admissable (i.e. it never overestimates the
  remaining distance) the algorithm will find the shortest path.

  Note that `test` is required.  The only sensible default would be `eql`, but
  if you were using states that need a different predicate and forgot to pass it
  the algorithm would end up blowing the heap, which is unpleasant.

  "
  (let ((seen (make-hash-table :test test))
        (frontier (pileup:make-heap #'< :key #'path-estimate)))
    (labels ((mark-seen (path)
               (setf (gethash (path-state path) seen) (path-cost path)))
             (push-path (path)
               (mark-seen path)
               (pileup:heap-insert path frontier)))
      (iterate
        (initially (push-path (make-path :state start)))

        (for (values current found) = (pileup:heap-pop frontier))
        (unless found
          (return (values nil nil)))

        (for current-state = (path-state current))

        (when (funcall goalp current-state)
          (return (values (path-to-list current) t)))

        (for current-cost = (path-cost current))

        (iterate
          (for next-state :in (funcall neighbors current-state))
          (for next-cost = (+ current-cost (funcall cost current-state next-state)))
          (for (values seen-cost previously-seen) = (gethash next-state seen))
          (when (or (not previously-seen)
                    (< next-cost seen-cost))
            (for next-estimate = (+ next-cost (funcall heuristic next-state)))
            (push-path (make-path :state next-state
                                  :cost next-cost
                                  :estimate next-estimate
                                  :previous current))))))))
