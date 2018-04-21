(asdf:defsystem :vintage
  :description "My entry in the Lisp Game Jam 2018"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :cl-charms
               :iterate
               :losh
               :split-sequence

               )

  :serial t
  :components
  ((:module "vendor" :serial t
    :components ((:file "quickutils-package")
                 (:file "quickutils")))
   (:file "package")
   (:module "src" :serial t
    :components
    ((:file "boots")
     (:file "wrap")
     (:file "main")))))
