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

;; @node Self-doc
;; @section Self-doc

;; Emacs offers a fantastic comprehensive help system.  Emacsy intends to
;; replicate most of this functionality.  One distinction that would be
;; nice to make is to partition Scheme values into procedures, variables,
;; and parameters.  In Scheme, all these kinds of values are the handled
;; the same way.  In Emacs, each are accessible by the help system
;; distinctly.  For instance, [[C-h f]] looks up functions, [[C-h v]]
;; looks up variables.  In addition to defining what kind of value a
;; variable holds, this also allows one to include documentation for
;; values which is not included in Guile Scheme by default. (XXX fact
;; check.)

;;; Code:
(define-module (emacsy self-doc)
  #:use-module (emacsy util)
  #:use-module (srfi srfi-1)
  #:use-module (search basic)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 documentation)
  #:export (emacsy-collect-kind
            emacsy-kind-ref
            emacsy-kind-set!
            documentation
            define-variable
            define-documentation
            define-parameter))

(define (object-documentation-ref object)
  "Return the docstring for OBJECT.
OBJECT can be a procedure, macro or any object that has its
`documentation' property set."
  (object-property object 'documentation))

(define (object-documentation-set! object value)
  "Return the docstring for OBJECT.
OBJECT can be a procedure, macro or any object that has its
`documentation' property set."
  (set-object-property! object 'documentation value))

(define (emacsy-kind-ref object)
  "Return the kind for the OBJECT."
  (object-property object 'emacsy-kind))

(define (emacsy-kind-set! object kind)
  "Return the kind for the OBJECT."
  (set-object-property! object 'emacsy-kind kind))

(define (documentation variable-or-symbol)
  (let ((v (cond
            ((symbol? variable-or-symbol)
             ;(format #t "IN current-module ~a~%" (current-module))
             (module-variable (current-module) variable-or-symbol))
            ((variable? variable-or-symbol)
             (object-documentation-ref variable-or-symbol))
            (else
             (scm-error
              'no-such-variable
              "variable-documentation"
              "Expected a symbol in the current module or a variable; got ~a"
              (list variable-or-symbol)
              #f)))))
    (if v
        (object-documentation-ref v)
        #f)))

;; We also want to be able to collect up all the variables in some given
;; module.
(define* (emacsy-collect-kind module kind #:optional (depth 0))
    "Return the symbols that are of the variable, parameter, or
command kind. Inspects symbols defined locally within the module and
of the interfaces it includes (up to a given depth)."
    (let ((results '()))
      (define (collect module)
        (module-for-each
         (lambda (symbol variable)
           (if (eq? kind (emacsy-kind-ref variable))
               (cons! symbol results)))
         module))
      (define expander
        (match-lambda
         ((mod current-depth)
          (map (lambda (child-mod)
                 (list child-mod (1+ current-depth)))
               (module-uses mod)))))
      (define done?
        (match-lambda
         ((mod current-depth)
          (if (<= current-depth depth)
           (collect mod))
          (> current-depth depth))))
      (breadth-first-search (list module 0) done? expander)
      results))

;;.
(define-syntax define-variable
  (syntax-rules ()
    ((define-variable name value documentation)
     (begin
       (define-once name value)
       ;(define-documentation name documentation)
       (let ((v (module-variable (current-module) 'name)))
         (emacsy-kind-set! v 'variable)
         (object-documentation-set! v documentation)
         (set-object-property! v 'source-properties (current-source-location)))))

    ((define-variable name value)
     (define-variable name value ""))))

;;.
(define-syntax define-documentation
  (syntax-rules ()
    ((define-documentation name documentation)
     (begin
       (let ((v (module-variable (current-module) 'name)))
         (object-documentation-set! v documentation)
         (set-object-property! v 'source-properties (current-source-location)))))))

;; Parameters behave similarly to variables; however, whenever they are
;; defined, their values are set.
(define-syntax define-parameter
  (syntax-rules ()
    ((define-parameter name value documentation)
     (begin
       (define name value)
       (let ((v (module-variable (current-module) 'name)))
         (when v
           (emacsy-kind-set! v 'parameter)
           (object-documentation-set! v documentation)
           (set-object-property! v 'source-properties (current-source-location))))))
    ((define-parameter name value)
     (define-parameter name value ""))))
