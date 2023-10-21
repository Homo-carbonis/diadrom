(defsystem dyn
  :author "Hugh Coleman"
  :version "0.1"
  :description "Dynamical"
  :depends-on ("dyn/solver")
  :class :package-inferred-system
  :in-order-to ((test-op (load-op "dyn/test")))
  :perform (test-op (op c) (symbol-call :rove :run-suite :hypergraph/test)))
