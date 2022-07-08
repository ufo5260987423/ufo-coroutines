#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 Guy Q. Schemer
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (ufo-coroutines))

(test-begin "Test 1: init-iterator")
(let ((iter
        (init-iterator 
            (lambda (yield)
			    (let loop ((val 0))
				    (when (< val 3)
				        (loop (yield val))))))))
    (test-equal 0 (iter))
    (test-equal 1 (iter 1))
    (test-equal 2 (iter 2))
    (test-equal 'stop-iteration (iter 3))
    (test-equal 'stop-iteration (iter 0)))
(test-end)

(test-begin "Test 2: make-coroutine")
(let ((cor
        (init-coroutine
            (lambda (yield)
			    (let loop ((val 0))
				    (if (< val 3)
				        (loop (yield val)))
                        'end)))))
    (let-values ([(resume arg) (cor)])
        (set! cor resume)
        (test-equal 0 arg))
    (let-values ([(resume arg) (cor 1)])
        (set! cor resume)
        (test-equal 1 arg))
    (let-values ([(resume arg) (cor 2)])
        (set! cor resume)
        (test-equal 2 arg))
    (let-values ([(resume arg) (cor 3)])
        (test-equal #f resume)
        (test-equal 'end arg)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
