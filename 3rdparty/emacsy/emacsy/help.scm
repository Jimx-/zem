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

;; @node Help
;; @section Help

;;; Code:

(define-module (emacsy help)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy klecl)
  #:use-module (emacsy command)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy core))

;;.
(define-interactive (describe-variable #:optional symbol) #t)

(define-interactive
  (describe-variable
   #:optional
   (symbol (completing-read
              "Describe variable: "
              (emacsy-collect-kind (current-module) 'variable 1)
              #:to-string symbol->string)))
  ;;(message "Describing variable ~a: ~a" symbol (variable-documentation symbol))
  (message "~a" (variable-documentation symbol)))

;;.
(define-interactive (describe-command #:optional symbol) #t)

(define-interactive
  (describe-command
   #:optional
   (symbol (completing-read
              "Describe command: "
              (emacsy-collect-kind (current-module) 'command 1)
              #:to-string symbol->string)))
  ;;(message "Describing variable ~a: ~a" symbol (variable-documentation symbol))
  (message "~a" (procedure-documentation (module-ref (current-module) symbol))))
;;; <help:keymap>=

(define-key global-map "C-h v" 'describe-variable)
(define-key global-map "C-h c" 'describe-command)
