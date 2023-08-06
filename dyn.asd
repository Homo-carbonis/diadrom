(defsystem dyn
  :author "Hugh Coleman"
  :version "0.1"
  :description "Dynamical"
  :depends-on ("misc" "let-over-lambda" "trivia" "srfi-1")
  :components ((:file "packages")
               (:file "dyn" :depends-on ("packages"))
               (:file "n-tree" :depends-on ("packages"))
               (:file "solver" :depends-on ("packages" "n-tree"))))
