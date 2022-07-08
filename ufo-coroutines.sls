;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2022 Guy Q. Schemer
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ufo-coroutines)
  (export init-coroutine init-iterator)
  (import (chezscheme))

(define (init-coroutine proc . args)
  (letrec* (
      [prompt #f]
      [prompt-and-null 
        (lambda()
          (let ([return prompt])
            (set! prompt #f)
            return))]
      [yield 
        (case-lambda
          [() (yield #f)]
          [(return) 
            (when (not prompt) (error "yield" "yield attempted to expired prompt")) 
            (call/cc
              (lambda(current-continuation-0)
                (letrec 
                  ([resume 
                    (case-lambda
                      [() (resume #f)]
                      [(arg) 
                        ((call/1cc
                          (lambda (current-continuation-1)
                            (set! prompt current-continuation-1)
                            (current-continuation-0 arg)))) ])])
                  ((prompt-and-null) (lambda()(values resume return))))))])]
      [coroutine-proc 
        (case-lambda
          [() (coroutine-proc #f)]
          [(ignore) 
            ((call/1cc
              (lambda(current-continuation)
                (set! prompt current-continuation)
                (let ([return 
                    (with-exception-handler
                      (lambda (exception) ((prompt-and-null) (lambda () (raise exception))))
                      (lambda () (apply proc yield args))) ])
                  ((prompt-and-null) (lambda()(values #f return)))))))])] )
    coroutine-proc))

(define (init-iterator proc . args)
  (letrec* (
      [prompt #f]
      [done #f]
      [prompt-and-null 
        (lambda()
          (let ([return prompt])
            (set! prompt #f)
            return))]
      [iterator
        (lambda(ignore)
          (with-exception-handler
            (lambda(exception)
              ((prompt-and-null) (lambda() (raise exception))))
            (lambda() (apply proc yield args)))
          (set! done #t)
          ((prompt-and-null) (lambda() 'stop-iteration))) ]
      [yield 
        (case-lambda
          [() (yield #f)]
          [(return) 
            (when (not prompt) (error "yield" "yield attempted to expired prompt")) 
            (call/cc
              (lambda(current-continuation)
                (set! iterator current-continuation)
                ((prompt-and-null) (lambda() return))))])]
      [iterator-proc 
        (case-lambda
          [() (iterator-proc #f)]
          [(send-back) 
            (if done
              'stop-iteration
              ((call/1cc
                (lambda(current-continuation)
                  (set! prompt current-continuation)
                  (iterator send-back)))))])])
    iterator-proc))
     
(define (sync waitable . args)
  (letrec ((resume 
        (init-iterator 
          (lambda (await)
		        (apply waitable await resume args)))))
    (resume)))
)
