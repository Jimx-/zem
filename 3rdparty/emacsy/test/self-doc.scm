;;; <file:self-doc-test.scm>=
;;; @subsection Legal Stuff
;;;
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
(use-modules (check)
             (emacsy self-doc))
(use-private-modules (emacsy self-doc))

;;; <self-doc:test>=
(define (this-module) (current-module))
(define-variable x 1 "This is the variable x.")
(check x => 1)
;;(check (documentation (module-variable (this-module) 'x)) => "This is the variable x.")
(check (documentation 'x) => "This is the variable x.")
(define-variable x 2 "This is the variable x.")
(check x => 1)
(check (documentation 'x) => "This is the variable x.")
(set! x 3)
(check x => 3)
(check (documentation 'x) => "This is the variable x.")

;; When we re-define x, the documentation stays.
(define x 4)
(check (documentation 'x) => "This is the variable x.")
(check x => 4)
(define-variable x 5 "This is the variable x; it is!")
(check x => 4)
(check (documentation 'x) => "This is the variable x; it is!")

(define-variable x 5 "This is the variable x.")
;;; <self-doc:test>=
(check (emacsy-collect-kind (current-module) 'variable) => '(x))
;;; <self-doc:test>=
(define-parameter y 1 "This is the parameter y.")
(check y => 1)
(check (documentation 'y) => "This is the parameter y.")
(define-parameter y 2 "This is the parameter y.")
(check y => 2)
(check (documentation 'y) => "This is the parameter y.")
(set! y 3)
(check y => 3)
(check (documentation 'y) => "This is the parameter y.")
;(check (object-properties (module-variable (current-module) 'y)) => '())
(check (emacsy-collect-kind (current-module) 'parameter) => '(y))
(define emacsy-collect-all-kind emacsy-collect-kind)
(check (emacsy-collect-all-kind (current-module) 'parameter 0) => '(y))

;;; Now let's try to start a new module.  And probe some of the behavior.
;;;
;;;
;;; <self-doc:test-this-last>=
(define-module (test-this)
  #:use-module (check)
  #:use-module (emacsy self-doc))


(check (emacsy-collect-kind (current-module) 'parameter) => '())
(check (emacsy-collect-kind (current-module) 'variable) => '())
(check (module-name (current-module)) => '(test-this))

;; XXX These two tests behave differently on GNU/Linux and Mac OS X.
;(check (module-variable (current-module) 'x) => #f)
;(check (documentation 'x) => #f)
;(check (documentation 'y) => #f)

(use-private-modules (guile-user))

(check x => 4)
(check y => 3)
(check (documentation 'x) => "This is the variable x.")
;;(check (documentation (module-variable (this-module) 'x)) => "This is the variable x.")
(check (documentation 'y) => "This is the parameter y.")
;;(check (documentation (module-variable (this-module) 'y)) => "This is the parameter y.")
(check (emacsy-collect-kind (current-module) 'variable) => '())
(check (emacsy-collect-kind (current-module) 'parameter) => '())

(check (emacsy-collect-all-kind (current-module) 'variable) => '())
(check (emacsy-collect-all-kind (current-module) 'parameter) => '())

(check (emacsy-collect-all-kind (current-module) 'variable 1) => '(x))
(check (emacsy-collect-all-kind (current-module) 'parameter 1) => '(y))

(check (string-suffix? "test/self-doc.scm" (assoc-ref (current-source-location) 'filename)) => #t)
;;(check (current-filename) => #f)
(check (source-properties x) => '())
(check (source-properties 'x) => '())
(check (source-properties (module-variable (current-module) 'x)) => '())

(check-exit)
