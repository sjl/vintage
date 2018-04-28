(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :deletef
               :ensure-boolean
               :ensure-list
               :once-only
               :rcurry
               :removef
               :symb
               :with-gensyms

               )
  :package "VINTAGE.QUICKUTILS")
