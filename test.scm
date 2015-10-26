#!/usr/local/bin/gosh

;; For Emacsen (evaluate with C-x C-e) :
;; (outline-minor-mode 1)
;; (setq-local outline-regexp "^[\s\t]*;;[\s]+\\+[+-]*\s")
;; (setq-local outline-level (lambda () (- (outline-level) 4)))

(add-load-path ".")

(use srfi-1)     ;; list utils
(use srfi-13)    ;; string utils
(use util.match) ;; match / match-lambda
(use lmn.peg)
(use lmn.util)

(use lmn.atom)
(use lmn.atomset)
(use lmn.process)
(load "./lmn/test/atom.test.scm")
(load "./lmn/test/atomset.test.scm")
(load "./lmn/test/process.test.scm")
