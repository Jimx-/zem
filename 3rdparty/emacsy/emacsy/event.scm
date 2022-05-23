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

;; @node Event
;; @section Event

;; One of the idioms we want to capture from Emacs is this.
;;
;; @verbatim
;;   (define-key global-map "M-f" 'some-command)
;; @end verbatim
;;
;; They [[keymap]] and [[command]] module will deal with most of the
;; above, except for the [[kbd]] procedure.  That's something events will
;; be concerned with.  One may define a converter for a [[kbd-entry]] to
;; an event of the proper type.  Note that a [[kbd-string]] is broken
;; into multiple [[kbd-entries]] on whitespace boundaries, e.g., ``C-x
;; C-f'' is a [[kbd-string]] that when parsed becomes two [[kbd-entries]]
;; ``C-x'' and ``C-f''.

;;; Code:

(define-module (emacsy event)
  #:use-module (ice-9 q)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (emacsy util))

;; @defmac define-kbd-converter name arg @dots{}
;; A convenience macro to work with a given buffer.
;; @end defmac

(define-syntax-public define-kbd-converter
  (syntax-rules ()
    ((define-kbd-converter (name args ...) expr ...)
     (begin (define* (name args ...)
              expr ...)
            (register-kbd-converter 'name name)))
    ((define-kbd-converter name value)
     (begin (define* name value)
            (register-kbd-converter 'name name)))))

;; Basic event class.
(define-class-public <event> ()
  (time #:getter time #:init-thunk (lambda () (emacsy-time)))
  (discrete-event? #:getter discrete-event? #:init-keyword #:discrete-event? #:init-value #t))
(export time discrete-event?)

;; Event to capture key strokes, including the modifier keys.
(define-class-public <modifier-key-event> (<event>)
  (modifier-keys #:getter modifier-keys
                 #:init-keyword #:modifier-keys
                 #:init-value '()))

;; Event to capture key strokes, including the modifier keys.
(define-class-public <key-event> (<modifier-key-event>)
  (command-char #:getter command-char
                #:init-keyword #:command-char))
(export modifier-keys command-char)

;; Event to capture mouse events.
(define-class-public <mouse-event> (<event>)
  (modifier-keys #:getter modifier-keys #:init-keyword #:modifier-keys #:init-value '())
  (position #:getter position #:init-keyword #:position)
  (button #:getter button #:init-keyword #:button)
  (state #:getter state #:init-keyword #:state))
(export modifier-keys position button state)

;; Event to capture mouse drag events.
(define-class-public <drag-mouse-event> (<mouse-event>)
  (rect #:getter rect #:init-keyword #:rect))

;; A dummy event, useful to record when used ;;; temporal macros.
;;
;; @c This should probably be placed in the [[kbd-macro]] module.
(define-class-public <dummy-event> (<event>))

;; Now we have the function [[kbd-entry->key-event]].  [[kbd]] needs to
;; know about this and any other converter function.  So let's register it.
(define kbd-converter-functions '())

(define char-set:requires-shift-key (char-set-union
                                     char-set:symbol
                                     char-set:upper-case
                                     (char-set-delete char-set:punctuation
                                        ;punctuation = !"#%&'()*,-./:;?@[\\]_{}
                                         #\. #\; #\[ #\] #\, #\' #\\)))

;; Let's write the converter for the [[<key-event>]] class that will
;; accept the same kind of strings that Emacs does.  If the [[kbd-entry]]
;; does not match the event-type, we return false [[#f]].
(define (kbd-entry->key-event kbd-entry)
  (match (strip-off-modifier-keys kbd-entry)
    ((mod-keys kbd-entry)
     (let ((regex "^([^ ]|RET|DEL|ESC|TAB|SPC)$"))
       (let ((match (string-match regex kbd-entry)))
         (if match
             (let* ((char (string->command-char (match:substring match 1))))
               (make <key-event> #:command-char char #:modifier-keys mod-keys))
             #f))))))

(define (get-modifier-keys str)
      (if str
          (map modifier-char->symbol
               (filter (lambda (x) (not (char=? x #\-))) (string->list str)))
          '()))

(define-public (strip-off-modifier-keys kbd-entry)
  "Parse the kbd-entry and strip off the modifier-keys and return the kbd-entry
and a list of modifier keys."
  (let ((regex "^(([ACHMsS]-)*)(.*)$"))
    (let ((match (string-match regex kbd-entry)))
      (if match
          (let ((mod-keys (get-modifier-keys (match:substring match 1))))
            (list mod-keys (match:substring match 3)))
          (list '() kbd-entry)))))

;; For the modifier keys, we are going to emulate Emacs to a fault.
(define-public (modifier-char->symbol char)
  (case char
    ((#\A) 'alt)
    ((#\C) 'control)
    ((#\H) 'hyper)
    ((#\M) 'meta)
    ((#\s) 'super)
    ((#\S) 'shift)
    (else (warn (format #f "Invalid character for modifier key: ~a" char))
          #f)))

(define (string->command-char str)
  (if (= (string-length str) 1)
      ;; One character string, return first character; simple!
      (string-ref str 0)
      (string-case str
                   ("RET" #\cr)
                   ("DEL" #\del)
                   ("ESC" #\esc)
                   ("TAB" #\tab)
                   ("SPC" #\space)
                   (else (warn (format #f "Invalid command character: ~a" str)) ))))

;;.
(define-public (register-kbd-converter function-name function)
  (set! kbd-converter-functions
        (assq-set! kbd-converter-functions function-name function)))

;;; Our code doesn't account for duplicate modifier keys.  For the keymap,
;;; we want a unique identifier of an event.  Rather than massaging the
;;; conversion while in its string form, it seems reasonable to convert
;;; the [[kbd-entry]] into an event, then make the event canonical, then
;;; convert back into a string.  [[kbd]] will look like this:
(define*-public (kbd key-string #:optional (canonical? #t))
  (if canonical?
      (map event->kbd (map canonize-event! (kbd->events key-string)))
      (map event->kbd (kbd->events key-string))))

(define (kbd-entry->event kbd-entry)
    (or (find-first (lambda (f) (f kbd-entry))
                 (alist-values kbd-converter-functions))
        (throw 'invalid-kbd-entry kbd-entry)))

;;.
(define-public (kbd->events kbd-string)
  (let ((kbd-entries (string-tokenize kbd-string)))
        (map kbd-entry->event kbd-entries)))

;;.
(define-method-public (canonize-event! (event <key-event>))
  ;;; <event:Deal with shift key.>=
  (if (memq 'shift (modifier-keys event))
      (if (char-set-contains? char-set:requires-shift-key (command-char event))
          ;; Remove extraneous shift.
          (slot-set! event 'modifier-keys (delq 'shift (modifier-keys event)))
          ;; No shift required, but there is a shift in the kbd-entry.
          (if (char-lower-case? (command-char event))
              (begin
                ;; Change the character to uppercase.
                (slot-set! event 'command-char (char-upcase (command-char event)))
                ;; Get rid of the shift.
                (slot-set! event 'modifier-keys (delq 'shift (modifier-keys event)))))))
  (let ((mod-keys (modifier-keys event)))
    ;; Put them in alphabetical order: ACHMsS.
    (slot-set! event 'modifier-keys
               (intersect-order mod-keys
                                '(alt control hyper meta super shift))))
  event)

;; Now we convert the [[<key-event>]] back to a [[kbd-entry]].
(define-method-public (event->kbd (event <key-event>))
  (let ((mods (next-method))
        (cmd-char (command-char->string (command-char event))))
    (format #f "~a~a" mods cmd-char)))

;;.
(define-method-public (event->kbd (event <modifier-key-event>))
  (let ((mods (map string (map modifier-symbol->char (modifier-keys event)))))
    (string-join `(,@mods "") "-")))

;; Instead of using [[define-generic]] I've written a convenience macro
;; [[define-generic-public]] that exports the symbol to the current
;; module.  This mimics the functionality of [[define-public]].  In
;; general, any *-public macro will export the symbol or syntax to the
(define-public (modifier-symbol->char sym)
    (case sym
      ((alt) #\A)
      ((control) #\C)
      ((hyper) #\H)
      ((meta) #\M)
      ((super) #\s)
      ((shift) #\S)
      (else (error "Bad modifier symbol " sym))))

(define (command-char->string c)
    (case c
      ((#\cr #\newline) "RET")
      ((#\del) "DEL")
      ((#\esc) "ESC")
      ((#\tab) "TAB")
      ((#\space) "SPC")
      (else (string c))))

;; Display the @var{<key-event>} in a nice way.
(define-method (write (obj <key-event>) port)
  (display "#<key-event " port)
  (display (event->kbd obj) port)
  (display ">" port))

;;; A few procedures to determine whether what kind of objects is nice.
(define-public (event? obj)
  (is-a? obj <event>))

(define-public (key-event? obj)
  (is-a? obj <key-event>))

;;; <event:procedure>=
(define-method (canonize-event! (event <mouse-event>))
  (let ((mod-keys (modifier-keys event)))
    ;; Put them in alphabetical order: ACHMsS.
    (slot-set! event 'modifier-keys
               (intersect-order mod-keys
                                '(alt control hyper meta super shift)))
    event))

;; The @var{kbd-entry} for mouse events is similar to key events.  The
;; regular expression is
;; @verb{@^(([ACHMsS]-)*)((up-|down-|drag-)?mouse-([123]))\$@}.
(define-kbd-converter (kbd-entry->mouse-event kbd-entry)
  (let* ((regex "^(([ACHMsS]-)*)((up-|down-|drag-)?mouse-([123]))$")
         (match (string-match regex kbd-entry)))
      (if match
          (let* ((symbol (string->symbol (match:substring match 3)))
                 (modifier-keys (get-modifier-keys (match:substring match 1))))
            ;; Warning that symbol is not used; squelch with this noop ref.
            symbol
            ;;; <event:Make and return mouse event.>=
            (make <mouse-event> #:position #f
                  #:button (string->number (match:substring match 5))
                  #:state (let ((state-string (match:substring match 4)))
                            (if state-string
                                (string->symbol
                                 (string-trim-right state-string #\-))
                                'click))
                  #:modifier-keys modifier-keys))
          ;; It doesn't specify a mouse event; return false.
          #f)))

(define-method (event->kbd (event <mouse-event>))
  (define (state->list state)
    (case state
      ((up down drag)
       (list (symbol->string state)))
      ((click)
       '())
      (else
       (error "Bad state state for mouse event " state))))
  (let ((mods (map string (map modifier-symbol->char (modifier-keys event))))
        (state-list (state->list (state event))))
    (string-join
     `(,@mods ,@state-list "mouse" ,(number->string (button event)))
     "-")))

;;; Finally, let's add some interrogative procedures that mirror Emacs'.
(define*-public (mouse-event? obj #:optional (of-state #f))
  (and (is-a? obj <mouse-event>)
       (if of-state
           (eq? of-state (state obj))
           #t)))

;;.
(define-public (up-mouse-event? e)
  (mouse-event? e 'up))

;;.
(define-public (down-mouse-event? e)
  (mouse-event? e 'down))

;;.
(define-public (drag-mouse-event? e)
  (mouse-event? e 'drag))

;;.
(define-public (click-mouse-event? e)
  (mouse-event? e 'click))

;;.
(define-public (motion-mouse-event? e)
  (mouse-event? e 'motion))

;;; <event:procedure>=
(define-method (canonize-event! (event <event>))
  event)

(define-method (event->kbd (event <event>))
  #f)

;;; Now we can register it.
(register-kbd-converter 'kbd-entry->key-event kbd-entry->key-event)
