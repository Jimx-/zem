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

;; We generate the file @file{example/hello-emacsy.c.x} by running the
;; command: @code{guile-snarf example/hello-emacsy.c}.  Emacsy can now
;; access and alter the application's internal state.
;;.

;; Here's where the fun begins.
(use-modules (srfi srfi-1) ;; any
             )

(define-interactive 
  (load-url #:optional 
        (url (read-from-minibuffer "URL: "))) 
  (webkit-load-url url))

;; Load-url is all right, but it requires an actual URL.
;; Let's fix that with a new command: GOTO.
(define-interactive 
  (goto #:optional
        (urlish (read-from-minibuffer "GOTO: ")))
  (cond
   ((string-prefix? "http://" urlish)
    (load-url urlish))
   ((string-contains urlish " ")
    ;; It contains spaces. It's probably a search.
    (load-url
     (format #f "http://www.google.com/search?q=~a"
             (string-map (lambda (c) (if (eq? c #\space) #\+ c)) urlish)))
    )
   (else
    ;; It's just one word.  Let's try adding a .com and http:// if it
    ;; needs it.
    (load-url (format #f "http://~a~a" urlish 
                      (if (any (lambda (suffix) 
                                 (string-suffix? suffix urlish))
                               '(".com" ".org" ".net"))
                          ""
                          
                          ".com"))))))

(define-interactive (go-forward)
  (webkit-forward))

(define-interactive (go-back)
  (webkit-backward))

(define-interactive (reload)
  (webkit-reload))

(define-interactive (reload-script)
  (load ".emacsy-webkit-gtk.scm"))

(define find-text #f)

;; These aren't as good as Emacs' isearch-forward, but they're not
;; a bad start.
(define-interactive 
  (search-forward #:optional
                   (text (or find-text (read-from-minibuffer "Search: "))))
  (set! find-text text)
  (webkit-find-next text))

(define-interactive 
  (search-backward #:optional
                  (text (or find-text (read-from-minibuffer "Search: "))))
  (set! find-text text)
  (webkit-find-previous text))

;; Now let's bind these to some keys.

(define-key global-map (kbd "M-g") 'goto)
(define-key global-map (kbd "s-g") 'goto)
;; Let's use the super key to go forward and backward.
(define-key global-map (kbd "s-f") 'go-forward)
(define-key global-map (kbd "s-b") 'go-back)
(define-key global-map (kbd "C-s") 'search-forward)
(define-key global-map (kbd "C-r") 'search-backward)
