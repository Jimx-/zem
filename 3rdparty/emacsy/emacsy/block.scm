;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;;
;;; This file is part of Emacsy.
;;;
;;; Emacsy is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Emacsy is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; @node Block
;; @section Block

;;; Code:

(define-module (emacsy block)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (emacsy util))

;;.
(define-syntax-public with-blockable
  (syntax-rules ()
    ((with-blockable e ...)
     (call-blockable (lambda () e ...)))))

;; We're going to capture these blocking continuations into a class.
(define-class <blocking-continuation> ()
  (number #:getter number #:init-thunk (let ((count -1))
                                         (lambda () (incr! count))))
  (loop-number #:getter loop-number #:init-keyword #:loop-number)
  (tag #:getter tag #:init-keyword #:tag)
  (continuation #:init-keyword #:continuation)
  (continue-when? #:init-keyword #:continue-when?)
  (continue-now #:init-keyword #:continue-now)
  ;; Has this ran and ready to be deleted?
  (ran? #:accessor ran? #:init-value #f)
  (serial? #:getter serial? #:init-keyword #:serial? #:init-value #t))

(define-method (write (obj <blocking-continuation>) port)
  (write (string-concatenate
          (list "#<bc " (symbol->string (tag obj))
                " " (number->string (number obj))
                " cl " (number->string (loop-number obj)) ">")) port))

;; [[call-blockable]] will handle any aborts to the [['block]] prompt.
;; If the thunk aborts, it adds an instance of the class
;; [[<blocking-continuation>]] to a list of such instances.
(define blocking-continuations '())

;;; If there are no blocking continuations, we run this hook.

(define-public no-blocking-continuations-hook (make-hook))

;;.
(define-public (block-yield)
  ;; I forgot why I'm running this thunk.
  (run-thunk (abort-to-prompt 'block 'block-until
                              (const #t) #t)))

;;.
(define-public (call-blockable thunk)
  (let ((bc #f))
    (call-with-prompt
     'block
     thunk
     (lambda (cc kind . args)
       (case kind
         ((block-until)
          (let ((continue-command-loop? #t)
                (continue-wait? #t))
            (set! bc ;;; <block:Make blocking continuation.>=
                     (make <blocking-continuation>
                       #:tag 'block-until
                       #:continuation cc
                       #:loop-number 0
                       #:continue-when? (car args)
                       #:continue-now
                       (lambda ()
                         (set! continue-command-loop? #f)
                         (if continue-wait?
                             (call-blockable
                              (lambda () (cc (lambda () #t))))))
                       #:serial? (cadr args)))
            ;; Remember this bc.
            (cons! bc blocking-continuations))))))
    bc))

;;; @c \todo{rename continue-now?}

;; To possibly resume these continuations, we're going to call
;; [[block-tick]].  Additionally, continuations come in two flavors:
;; serial and non-serial.  The constraints on resuming are different.  A
;; non-serial block can be resumed whenever the
;; [[continue-when?]]
;; thunk return true.  A
;; serial block, however, will only be resumed after every other serial
;; block that has a greater number, meaning more recent, has been
;; resumed.
(define-public (block-tick)
  (set! blocking-continuations
        ;; Sort the continuations by the most recent ones.
        (sort! blocking-continuations (lambda (a b)
                                        (> (number a) (number b)))))
     (let ((ran-serial? #f))
       (for-each
        (lambda (bc)
          (if (not (serial? bc))
              ;; If it's not serial, we might run it.
              (maybe-continue bc)
              ;; If it's serial, we only run the top one.
              (if (and (not ran-serial?) (serial? bc))
                  (begin
                    (if (maybe-continue bc)
                        (set! ran-serial? #t))))))
        blocking-continuations))
     ;; Keep everything that hasn't been run.
     (set! blocking-continuations
           (filter! (lambda (bc) (not (ran? bc)))
                    blocking-continuations))
     ;(format #t "blocking-continuations #~a of ~a~%" (length blocking-continuations) (map number blocking-continuations))
     (when (or (null? blocking-continuations)
               (null? (filter serial? blocking-continuations)))
       (run-hook no-blocking-continuations-hook))
    #t)

;;; @c\todo[inline]{Maybe get rid of no-blocking-continuations-hook and just have a
;;; @cpredicate to test for whether any blocks exist?}

;;.
(define*-public (blocking?)
  (> (length blocking-continuations) 0))

;;.
(define-method (maybe-continue (obj <blocking-continuation>))
  (if (and (not (ran? obj))
;           (or run-serial? (serial? obj))
           ;; this line crashed.
           (run-thunk (slot-ref obj 'continue-when?)))
      (begin (set! (ran? obj) #t)
             (run-thunk (slot-ref obj 'continue-now))
             #t)
      #f))

;; In addition to simply yielding we can block until a particular
;; condition is met.
(define*-public (block-until condition-thunk #:optional (serial? #f))
  (if (not (run-thunk condition-thunk))
      (run-thunk (abort-to-prompt 'block 'block-until
                                  condition-thunk serial?))))

;; And if we have [[block-until]], it's easy to write
;; [[block-while]].
(define*-public (block-while condition-thunk #:optional (serial? #f))
  (block-until (negate condition-thunk) serial?))

;; Sometimes we may just want to kill a blocking continuation.  One could
;; just forget the reference and let it be garbage collected.  Here,
;; we're going to throw an exception such that whatever the continuation
;; was doing can potentially be cleaned up.
(define-method-public (block-kill (obj <blocking-continuation>))
  (set! (ran? obj) #t)
  (call-blockable
   (lambda () ((slot-ref obj 'continuation)
               (lambda ()
                 (throw 'block-killed obj)
                 #f)))))
