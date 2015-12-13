#!/usr/local/bin/gosh

(add-load-path ".")

(use lmn.util)
(set! *debug* #t)

(use lmn.object.atom)
(use lmn.object.atomset)
(use lmn.object.process)
(use lmn.parser.peg)
(use lmn.evaluator.control.stack)
(use lmn.evaluator.control.pp)
(use lmn.evaluator.operations)

(load "./test/object/atom.test.scm")
(load "./test/object/atomset.test.scm")
(load "./test/object/process.test.scm")
(load "./test/parser/peg.test.scm")
(load "./test/evaluator/control/stack.test.scm")
(load "./test/evaluator/control/pp.test.scm")
(load "./test/evaluator/operations.test.scm")
