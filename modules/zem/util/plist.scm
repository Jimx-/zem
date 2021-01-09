;;; lists.scm --- Property lists

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 12 Apr 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides procedures for working with property lists.
;;
;; Property list is a list of the form:
;;
;;   (PROP1 VALUE1 PROP2 VALUE2 ...)
;;
;; PROP1, PROP2, ...   - symbols or keywords.
;; VALUE1, VALUE2, ... - any objects.

;;; Examples:

;; (plist-get '(one 1 two 2 three 3) 'two)   =>  2
;; (plist-get '(one 1 two 2 three 3) 'four)  =>  #f
;; (plist-get '(foo bar bar zoo) 'bar)       =>  zoo
;;
;; (plist-delete '(#:foo a #:bar b baz c #:bar d) #:bar)
;; =>  (baz c #:foo a)
;;
;; (plist-put '(foo 1 bar two) 'bar 2)  =>  (bar 2 foo 1)
;; (plist-put '(foo 1 bar 2) 'baz 3)    =>  (baz 3 foo 1 bar 2)
;;
;; (plist-new '(#:foo "one" #:bar "two" baz 3)
;;   'baz "three"
;;   'foo "oof"
;;   #:bar 'bar)
;; =>  (#:bar bar foo "oof" baz "three" #:foo "one")

;;; Code:

(define-module (zem util plist)
  #:use-module (ice-9 match)
  #:export (plist-fold
            plist-get
            plist-add
            plist-delete
            plist-put
            plist-new))

(define (plist-fold proc init plist)
  "Fold over property/value elements of PLIST.
Call (PROC PROPERTY VALUE RESULT) for each property, using INIT as the
initial value of RESULT."
  (let loop ((result init)
             (current plist))
    (match current
      (()
       result)
      ((prop val rest ...)
       (loop (proc prop val result)
             rest)))))

(define (plist-get plist property)
  "Return a value of PROPERTY from PLIST.
Return #f if PROPERTY does not exist."
  (match plist
    ((prop val rest ...)
     (if (eq? prop property)
         val
         (plist-get rest property)))
    (_ #f)))

(define (plist-add plist property value)
  "Add PROPERTY/VALUE pair to PLIST."
  (cons* property value plist))

(define (plist-delete plist property)
  "Remove all PROPERTY elements from PLIST."
  (plist-fold (lambda (prop val res)
                (if (eq? prop property)
                    res
                    (plist-add res prop val)))
              '()
              plist))

(define (plist-put plist property value)
  "Return new plist by changing or adding PROPERTY/VALUE pair in PLIST."
  (plist-add (plist-delete plist property)
             property value))

(define (plist-new old-plist . add-plist)
  "Return new plist by adding property/value pairs from ADD-PLIST to
OLD-PLIST."
  (plist-fold (lambda (prop val res)
                (plist-put res prop val))
              old-plist
              add-plist))

;;; lists.scm ends here
