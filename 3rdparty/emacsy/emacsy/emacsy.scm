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

(define-module (emacsy emacsy)
  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy event)
  #:use-module (emacsy keymap)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:use-module (emacsy command)
  #:use-module (emacsy mode)
  #:use-module (emacsy buffer)
  #:use-module (emacsy text)
  #:use-module (emacsy block)
  #:use-module (emacsy klecl)
  #:use-module (emacsy kbd-macro)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy core)
  #:use-module (emacsy help))

;;; Commentary:

;; @node Emacsy Facade
;; @section Emacsy Facade

;; So that users of our library don't have to import all of our nicely
;; partitioned modules individually, we'll expose a facade module that
;; re-exports all of the public interfaces for each module.  Just use
;; @example
;;   (use-modules (emacsy emacsy))
;; @end example
;; or
;; @example
;;   #:use-module (emacsy emacsy)
;; @end example

;;; Code:

(define (re-export-modules . modules)
  (define (re-export-module module)
    (module-for-each
     (lambda (sym var)
       ;;(format #t "re-exporting ~a~%" sym)
       (module-re-export! (current-module) (list sym)))
     (resolve-interface module)))
  (for-each re-export-module modules))

(re-export-modules
 '(emacsy util)
 '(emacsy self-doc)
 '(emacsy keymap)
 '(emacsy event)
 '(emacsy mode)
 '(emacsy buffer)
 '(emacsy text)
 '(emacsy coroutine)
 '(emacsy agenda)
 '(emacsy command)
 '(emacsy block)
 '(emacsy klecl)
 '(emacsy kbd-macro)
 '(emacsy minibuffer)
 '(emacsy core)
 '(emacsy help))
