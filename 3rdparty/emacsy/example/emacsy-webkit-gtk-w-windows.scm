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

;; Here's where the fun begins.

(use-modules (oop goops)
             (emacsy window)
             (srfi srfi-9) ;; record
             )

(format #t "current module ~a~%" (current-module))
;; FIXME: need minbuffer/modeline? aborts here
;; (message "Here I am!")

(define-class <web-buffer> (<buffer>)
  ;(buffer-webkit #:accessor buffer-webkit #:init-form (make-web-view))
  )

(define-method (emacsy-mode-line (buffer <web-buffer>))
  (format #f "~a~/~a~/~a" (next-method) (webkit-get-title) (webkit-get-url)))

(define-class <gtk-window> (<window>)
  (last-buffer-modified-tick #:accessor last-buffer-modified-tick #:init-value -1))

(define-method (needs-redisplay? (window <gtk-window>))
  (let* ((buffer (window-buffer window))
         (buffer-tick (buffer-modified-tick buffer))
         (window-tick (last-buffer-modified-tick window)))
    (not (= buffer-tick window-tick))))

(define-method (redisplayed! (window <gtk-window>))
  (let* ((buffer (window-buffer window))
         (buffer-tick (buffer-modified-tick buffer)))
    (set! (last-buffer-modified-tick window) buffer-tick)))

(define-method (window-clone (window <gtk-window>))
  (let ((new-window (next-method)))
    (set! (user-data new-window) #f)
    (set! (last-buffer-modified-tick new-window) -1)
    new-window))

(set! buffer-classes (list <web-buffer>))
(set! current-window (make <gtk-window> #:window-buffer messages))
(set! root-window (make <internal-window> #:window-children (list current-window)))

(define-interactive
  (load-url #:optional
        (url (read-from-minibuffer "URL: ")))
  (webkit-load-url url))

;; Load-url is all right, but it requires an actual URL.
;; Let's fix that with a new command: GOTO.
(define-interactive
  (goto #:optional
        (urlish (read-from-minibuffer "GOTO: ")))
  ;(set-buffer-name! urlish)
  (cond
   ((string-prefix? "http://" urlish)
    (load-url urlish))
   ((string-contains urlish " ")
    ;; It contains spaces. It's probably a search.
    (load-url
     (format #f "http://duckduckgo.com/search?q=~a"
             (string-map (lambda (c) (if (eq? c #\space) #\+ c)) urlish)))
    )
   (else
    ;; It's just one word.  Let's try adding a .com and http:// if it
    ;; needs it.
    (load-url (format #f "http://~a~a" urlish
                      (if (string-suffix? ".com" urlish) "" ".com"))))))

(define-interactive (go-forward)
  (webkit-forward))

(define-interactive (go-back)
  (webkit-backward))

(define-interactive (reload)
  (webkit-reload))

(define-interactive (reload-script)
  (load "emacsy-webkit-gtk-w-windows.scm"))

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

(define-record-type <window-user-data>
  (make-window-user-data widget web-view modeline)
  window-user-data?
  (widget wud-widget)
  (web-view wud-web-view)
  (modeline wud-modeline))

;; For some reason, I can't refer to record-types in C without first
;; referring to them in Scheme. The record accesses are inlined
;; apparently.  Seems like a Guile bug to me.
(define window-user-data?2 window-user-data?)
(define make-window-user-data2 make-window-user-data)
(define wud-widget2 wud-widget)
(define wud-modeline2 wud-modeline)

(define (current-web-view)
  (let ((wud (user-data current-window)))
    (and (window-user-data? wud)
         (wud-web-view wud))))

;; (define (gtk-after-change buffer)
;;   (set! (local-var 'needs-redisplay?) #t))

;; (add-hook! after-buffer-change-hook gtk-after-change)

(define-method (redisplay (window <window>))
  (let* ((buffer (window-buffer window))
         (userdata (user-data window)))
    (when (and buffer
               (window-user-data? userdata))
      ;;(format #t "redisplaying window ~a with buffer ~a~%" window buffer)

      (update-label! (wud-modeline userdata)
                     (emacsy-mode-line buffer)
                     (eq? window (selected-window)))
      (when (needs-redisplay? window)
        (cond
         ((is-a? buffer <text-buffer>)

          (web-view-load-string (wud-web-view userdata)
                                (buffer-string)))
         (else
          (web-view-load-string (wud-web-view userdata)
                                "")))

        (redisplayed! window)))))

(define-method (redisplay (window <internal-window>))
  (for-each redisplay (window-children window)))

(define (redisplay-windows)
  (redisplay root-window))

(define (instantiate-root-window)
  (instantiate-window root-window))

(define (gtk-window-configuration-change internal-window)
  (set-window-content! (warn 'instantiate-root-window (instantiate-root-window))))

(define-interactive (test-window-change)
  (gtk-window-configuration-change #f))

(add-hook! window-configuration-change-hook gtk-window-configuration-change)

(define-method (instantiate-window (window <window>))
  (let ((buffer (window-buffer window)))
    (create-web-view-window window buffer (is-a? buffer <text-buffer>))))

(define-method (instantiate-window (window <internal-window>))
  (warn 'gtk-window (create-gtk-window (map instantiate-window (window-children window))
                                       (eq? (orientation window) 'vertical))))

;; Now let's bind these to some keys.

(define-key global-map (kbd "M-g") 'goto)
(define-key global-map (kbd "s-g") 'goto)
;; Let's use the super key to go forward and backward.
(define-key global-map (kbd "s-f") 'go-forward)
(define-key global-map (kbd "s-b") 'go-back)
(define-key global-map (kbd "C-s") 'search-forward)
(define-key global-map (kbd "C-r") 'search-backward)
(define-key global-map (kbd "r") 'reload)
(define-key global-map (kbd "h") (lambda _ (goto "http://duckduckgo.com")))
(define-key global-map (kbd "H") (lambda _ (goto "http://shanecelis.github.io/2013/06/15/the-garden/")))

(export instantiate-window instantiate-root-window <window-user-data> make-window-user-data2 wud-widget2 wud-web-view wud-modeline window-user-data? window-user-data?2 redisplay redisplay-windows current-web-view)
