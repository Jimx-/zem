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
(define-module (emacsy mode)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-26)
  #:use-module (string completion)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy event)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (rnrs base)
  #:export (<mode>
            mode-name
            mode-map))

(define-class <mode> ()
  (name #:getter mode-name #:init-keyword #:mode-name)
  (mode-map #:accessor mode-map
            #:init-keyword #:mode-map
            #:init-form (make-keymap)))
