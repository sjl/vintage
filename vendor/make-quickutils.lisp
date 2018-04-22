(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :once-only
               :rcurry
               :symb
               :with-gensyms
               :ensure-boolean
               :ensure-list
               :deletef

               )
  :package "VINTAGE.QUICKUTILS")
