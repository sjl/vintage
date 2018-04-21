(ql:quickload :vintage)

#+sbcl
(progn
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
    "vintage"
    :executable t
    :compression nil
    :toplevel #'vintage:toplevel
    :save-runtime-options t))

#+ccl
(progn
  (ccl:gc)
  (ccl:save-application
    "vintage"
    :toplevel-function #'vintage:toplevel
    :purify t
    :prepend-kernel t))
