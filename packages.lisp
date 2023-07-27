(defpackage #:n-tree 
  (:use #:cl #:alexandria #:misc)
  (:export #:n-tree-search))

(defpackage #:dyn
  (:shadowing-import-from #:trivia #:if-match #:when-match)
  (:shadowing-import-from #:alexandria #:flatten)
  (:import-from #:srfi-1 #:fold #:zip #:unzip2)
  (:use #:cl #:misc #:alexandria #:trivia #:lol #:n-tree))
