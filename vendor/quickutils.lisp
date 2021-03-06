;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:COMPOSE :CURRY :DELETEF :ENSURE-BOOLEAN :ENSURE-LIST :ONCE-ONLY :RCURRY :REMOVEF :SYMB :WITH-GENSYMS) :ensure-package T :package "VINTAGE.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "VINTAGE.QUICKUTILS")
    (defpackage "VINTAGE.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "VINTAGE.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:MAKE-GENSYM-LIST :ENSURE-FUNCTION
                                         :COMPOSE :CURRY :DELETEF
                                         :ENSURE-BOOLEAN :ENSURE-LIST
                                         :ONCE-ONLY :RCURRY :REMOVEF :MKSTR
                                         :SYMB :STRING-DESIGNATOR :WITH-GENSYMS))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  )                                        ; eval-when

  (defun compose (function &rest more-functions)
    "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  

  (defun curry (function &rest arguments)
    "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  

  (declaim (inline delete/swapped-arguments))
  (defun delete/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'delete item sequence keyword-arguments))

  (define-modify-macro deletef (item &rest remove-keywords)
    delete/swapped-arguments
    "Modify-macro for `delete`. Sets place designated by the first argument to
the result of calling `delete` with `item`, place, and the `keyword-arguments`.")
  

  (defun ensure-boolean (x)
    "Convert `x` into a Boolean value."
    (and x t))
  

  (defun ensure-list (list)
    "If `list` is a list, it is returned. Otherwise returns the list designated by `list`."
    (if (listp list)
        list
        (list list)))
  

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (declaim (inline remove/swapped-arguments))
  (defun remove/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'remove item sequence keyword-arguments))

  (define-modify-macro removef (item &rest remove-keywords)
    remove/swapped-arguments
    "Modify-macro for `remove`. Sets place designated by the first argument to
the result of calling `remove` with `item`, place, and the `keyword-arguments`.")
  

  (defun mkstr (&rest args)
    "Receives any number of objects (string, symbol, keyword, char, number), extracts all printed representations, and concatenates them all into one string.

Extracted from _On Lisp_, chapter 4."
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  

  (defun symb (&rest args)
    "Receives any number of objects, concatenates all into one string with `#'mkstr` and converts them to symbol.

Extracted from _On Lisp_, chapter 4.

See also: `symbolicate`"
    (values (intern (apply #'mkstr args))))
  

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(compose curry deletef ensure-boolean ensure-list once-only rcurry
            removef symb with-gensyms with-unique-names)))

;;;; END OF quickutils.lisp ;;;;
