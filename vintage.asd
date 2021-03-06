(asdf:defsystem :vintage
  :description "My entry in the Lisp Game Jam 2018"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :beast
               :chancery
               :cl-charms
               :iterate
               :local-time
               :losh
               :pileup
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
     (:file "utils")
     (:file "wrap")
     (:file "astar")
     (:file "color")
     (:file "state")
     (:file "aspects")
     (:file "entities")
     (:file "antiques")
     (:file "mobs")
     (:file "messages")
     (:file "terrain")
     (:file "main")))))
