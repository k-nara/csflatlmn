#!/usr/local/bin/gosh

(add-load-path ".")

(use lmn.error)
(use lmn.atom)
(use lmn.atomset)
(use lmn.process)
(use lmn.stack)

(load "./lmn/test/error.test.scm")
(load "./lmn/test/atom.test.scm")
(load "./lmn/test/atomset.test.scm")
(load "./lmn/test/process.test.scm")
(load "./lmn/test/stack.test.scm")
