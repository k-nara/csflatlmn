#!/usr/local/bin/gosh

(add-load-path ".")

(use lmn.util.debug)
(set! *debug* #t)

(use lmn.util.set)
(use lmn.object.atom)
(use lmn.object.atomset)
(use lmn.object.process)
(use lmn.parser.peg)
(use lmn.util.stack)
(use lmn.util.pp)
(use lmn.evaluator.operations)
(use lmn.evaluator.type)

(load "./test/util/stack.test.scm")
(load "./test/util/pp.test.scm")
(load "./test/util/set.test.scm")
(load "./test/object/atom.test.scm")
(load "./test/object/atomset.test.scm")
(load "./test/object/process.test.scm")
(load "./test/parser/peg.test.scm")
(load "./test/evaluator/operations.test.scm")
(load "./test/evaluator/type.test.scm")
