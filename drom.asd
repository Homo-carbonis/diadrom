(defsystem drom
  :author "Hugh Coleman"
  :version "0.1"
  :description "Dynamical"
  :depends-on ("drom/solver")
  :class :package-inferred-system
  :in-order-to ((test-op (load-op "drom/test")))
  :perform (test-op (op c) (symbol-call :rove :run-suite :hypergraph/test)))

(register-system-packages
 "cl-plus-c" '(:plus-c))
