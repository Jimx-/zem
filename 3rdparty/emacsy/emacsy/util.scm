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
(define-module (emacsy util)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (debugging assert)
  #:use-module (system repl error-handling)
  #:declarative? #f
  ;#:export-syntax (define-syntax-public)
)
;;; <util:state>=
;; I want to get rid of this state if I can.
(define-public continue-command-loop? (make-unbound-fluid))
;;; <util:state>=
(define-public debug-on-error? #f)

;;; When emacsy-log? is set to #t all output is logged to
;;; (current-error-port).  If emacsy-log? is set to #f all logging
;;; output is ignored.
(define-public emacsy-log? #t)

;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-
;;; @section Utility Module
;;;
;;; The [[util]] module is a grab bag of all sorts of miscellaneous
;;; functionality.  Rather than defining it here in one place.  I thought
;;; it'd be best to define each piece where it is actually introduced and
;;; used.
;;;
;;;
;;; <util:macro>=
(define-syntax define-syntax-public
  (syntax-rules ()
    ((define-syntax-public name . body)
     (begin
       (define-syntax name . body)
       (export-syntax name)))))
(export-syntax define-syntax-public)
;;; <util:macro>=
(define-syntax-public string-case
  (syntax-rules (else)
    ((_ str (else e1 ...))
     (begin e1 ...))
    ((_ str (e1 e2 ...))
     (when (string=? str e1) e2 ...))
    ((_ str (e1 e2 ...) c1 ...)
     (if (string=? str e1)
         (begin e2 ...)
         (string-case str c1 ...)))))
;;; <util:macro>=
(define-syntax-public define-class-public
  (syntax-rules ()
    ((define-class-public name . body)
     (begin
       (define-class name . body)
       (export name)
       ))))
;;; <util:macro>=
(define-syntax-public define-method-public
  (syntax-rules ()
    ((define-method-public (name . args) . body)
     (begin
       (define-method (name . args) . body)
       (export name)
       ))))
;;; <util:macro>=
(define-syntax define-generic-public
  (syntax-rules ()
    ((define-generic-public name)
     (begin
       (define-generic name)
       (export name)))))
(export define-generic-public)
;;; <util:macro>=
(define-syntax-public repeat
  (syntax-rules ()
    ((repeat c e ...)
     (repeat-func c (lambda () e ...)))))
;;; <util:macro>=
(define-syntax-public in-out
  (syntax-rules ()
    ((in-out in thunk out)
     (in-out-guard (lambda () in)
                   (lambda () thunk)
                   (lambda () out)))
    ((in-out in thunk out pass-keys)
     (in-out-guard (lambda () in)
                   (lambda () thunk)
                   (lambda () out)
                   pass-keys))))
;;; \noindent There's a lot of information being
;;;
;;;
;;;
;;; \subsection*{Utilities}
;;; The [[incr!]] macro is just a little bit of syntactic sugar.
;;;
;;;
;;; <util:macro>=
(define-syntax-public incr!
  (syntax-rules ()
    ((incr! variable inc)
     (begin
       (set! variable (+ variable inc))
       variable))
    ((incr! variable)
     (incr! variable 1))))
;;; <util:macro>=
(define-syntax-public decr!
  (syntax-rules ()
    ((decr! variable inc)
     (incr! variable (- inc)))
    ((decr! variable)
     (decr! variable 1))))
;;; <util:macro>=
(define-syntax-public cons!
  (syntax-rules ()
    ((cons! elm list)
     (begin
       (set! list (cons elm list))
       list))))
;;; <util:procedure>=
(define-public pp pretty-print)
;;; <util:procedure>=
(define-public (repeat-func count func)
  (if (<= count 0)
      #f
      (begin
        (func)
        (repeat-func (1- count) func))))
;;; This defines the class [[<event>]].  It relies on [[emacsy-time]] that
;;; I'm going to place in a utilities module, which will be mostly a
;;; bucket of miscellaneous things that any module might end up making use
;;; of.
;;;
;;;
;;; <util:procedure>=
(define-public (emacsy-time)
  (exact->inexact (/ (tms:clock (times)) internal-time-units-per-second)))
;;; The procedure [[find-first]] is similar to [[find]]; however,
;;; [[(find-first f (x . xs))]] returns the first [[(f x)]] that not false
;;; rather than the first [[x]] for which [[(f x)]] is true.
;;;
;;;
;;; <util:procedure>=
(define-public (find-first f lst)
  "Return the first result f which is not false and #f if no such result is found."
  (if (null? lst)
      #f
      (or (f (car lst))
          (find-first f (cdr lst)))))
;;; <util:procedure>=
(define-public (alist-values alist)
  (map cdr alist))

(define-public (alist-keys alist)
  (map car alist))
;;; <util:procedure>=
(define-public (intersect-order list ordered-list )
  "Returns the intersection of the two lists ordered according to the
second argument."
  (filter (lambda (x) (memq x list))
          ordered-list))
;;; I use some procedures to access the last item of a list, which I call
;;; the [[rcar]], and the tail of the list with respect to its end instead
;;; of its head [[rcdr]].  These aren't efficient and should be replaced
;;; later.
;;;
;;;
;;; <util:procedure>=
(define-public (rcar lst)
  (car (reverse lst)))

(define-public (rcdr lst)
  (reverse (cdr (reverse lst))))
;;; <util:procedure>=
(define-public (emacsy-log-info format-msg . args)
  (when emacsy-log?
    (apply format (current-error-port) format-msg args)
    (newline (current-error-port))))
;;; [[member-ref]] returns the index of the element in the list if there
;;; is one.
;;;
;;;
;;; <util:procedure>=
(define-public (member-ref x list)
  (let ((sublist (member x list)))
    (if sublist
        (- (length list) (length sublist))
        #f)))
;;; This macro requires a procedure [[in-out-guard]] defined in the
;;; [[util]] module.
;;;
;;;
;;; <util:procedure>=
(define*-public (in-out-guard in thunk out #:optional (pass-keys '(quit quit-command)))
  (run-thunk in)
  ;if debug-on-error?
  ;; Don't run this as robustly so that we can debug the errors
  ;; more easily.
  (when #f
    (receive (result . my-values) (run-thunk thunk)
    (run-thunk out)
    (apply values result my-values)))

  (receive (result . my-values)
      (catch #t
        (if debug-on-error?
            (lambda ()
              (call-with-error-handling thunk #:pass-keys pass-keys))
            thunk)
        (lambda (key . args)
          (run-thunk out)
          (apply throw key args)))
    (run-thunk out)
    (apply values result my-values)))

;; Make code a little more obvious.
(define-public (run-thunk t)
  (t))
;;; <util:procedure>=
(define-public (vector= a b)
  (assert (vector? a) (vector? b) report: "vector= ")
  (let ((len (vector-length a)))
   (and (= (vector-length a) (vector-length b))
        (let loop ((i 0))
          (if (>= i len)
              #t
              (if (= (vector-ref a i) (vector-ref b i))
               (loop (1+ i))
               #f))))))
;;; @subsection What is a command?
;;;
;;;
;;; <util:procedure>=
(define*-public (with-backtrace* thunk #:optional (no-backtrace-for-keys '()))
  (with-throw-handler
   #t
   thunk
   (lambda (key . args)
     (when (not (memq key no-backtrace-for-keys))
       (emacsy-log-error
        "ERROR: Throw to key `~a' with args `~a'." key args)
       (backtrace)))))
;;; We don't want to tie the embedders hands, so for any error output it
;;; ought to go through [[emacsy-log-error]].
;;;
;;;
;;; <util:procedure>=
(define-public (emacsy-log-error format-msg . args)
  (when emacsy-log?
    (apply format (current-error-port) format-msg args)
    (newline (current-error-port))))

(define-public (emacsy-log-trace format-msg . args)
  (when emacsy-log?
    (apply format (current-error-port) format-msg args)
    (newline (current-error-port))))
;;; Let's define a convenience procedure to [[pretty-print]] to a string.
;;;
;;;
;;; <util:procedure>=
(define-public (pp-string obj)
  (call-with-output-string (lambda (port) (pp obj port))))
;;; <util:procedure>=
(define-public (emacsy-log-debug format-msg . args)
  (when emacsy-log?
    (apply format (current-error-port) format-msg args)
    (newline (current-error-port))))
;;; <util:procedure>=
(define-public (list-insert! lst k val)
  "Insert val into list such that (list-ref list k) => val."
  (receive (pre post) (split-at! lst k)
    (append! pre (list val) post)))
;;; [[read-from-string]] parses the string into an expression.
;;;
;;;
;;; <util:procedure>=
(define-public (read-from-string string)
  (call-with-input-string string (lambda (port) (read port))))
;;; So let's write it.  (This functionality has been baked into
;;; completing-read with the [[#:to-string]] keyword argument.)
;;;
;;;
;;; <util:procedure>=
;; object-tracker :: (a -> b) -> ((a -> b), (b -> a))
(define-public (object-tracker a->b)
  (define (swap-cons c)
    (cons (cdr c) (car c)))
  (let ((cache (make-hash-table)))
    (values
     (lambda (x)
       (let ((y (a->b x)))
         (if (hash-ref cache y)
             (emacsy-log-warning "object-tracker has a duplicate for pairs ~a ~a" (cons x y) (swap-cons (hash-get-handle cache y))))
         (hash-set! cache y x)
         y))
     (lambda (y)
       (or (hash-ref cache y) y)))))
;;; And if we have warning, we emit then through [[emacsy-log-warning]].
;;;
;;;
;;; <util:procedure>=
(define-public (emacsy-log-warning format-msg . args)
  (when emacsy-log?
    (apply format (current-error-port) format-msg args)
    (newline (current-error-port))))
