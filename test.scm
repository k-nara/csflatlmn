#!/usr/local/bin/gosh

(add-load-path ".")

(use lmn.error)
(use lmn.atom)
(use lmn.atomset)
(use lmn.process)
(use lmn.stack)
(use lmn.control)

(load "./test/error.test.scm")
(load "./test/atom.test.scm")
(load "./test/atomset.test.scm")
(load "./test/process.test.scm")
(load "./test/stack.test.scm")
(load "./test/control.test.scm")
