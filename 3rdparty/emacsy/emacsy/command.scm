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

;; @node Command
;; @section Command

;; @quotation
;; If words of command are not clear and distinct, if orders are not thoroughly understood, then the general is to blame.
;; @author Sun Tzu
;; @end quotation

;; The command module is responsible for a couple things.  In Emacs one
;; defines commands by using the special form [[(interactive)]] within
;; the body of the procedure.  Consider this simple command.
;;
;; @verbatim
;; (defun hello-command ()
;;   (interactive)
;;   (message "Hello, Emacs!"))
;; @end verbatim
;;
;; Emacsy uses a more Scheme-like means of defining commands as shown
;; below.
;;
;; @verbatim
;; (define-interactive (hello-command)
;;   (message "Hello, Emacsy!"))
;; @end verbatim
;;
;; One deviation from Emacs I want to see within Emacsy is to have the
;; commands be more context sensitive.  To illustrate the problem when I
;; hit @verb{|M-x TAB TAB|} it autocompletes all the available commands
;; into a buffer.  In my case that buffer contains 4,840 commands.  This
;; doesn't seem to hurt command usability, but it does hurt the command
;; discoverability.
;;
;; I want Emacsy to have command sets that are analogous to keymaps.
;; There will be a global command set [[global-cmdset]] similar to the
;; global keymap [[global-map]].  And in the same way that major and
;; minor modes may add keymaps to a particular buffer, so too may they
;; add command maps.
;;
;; @c \todo[inline]{Figure out where to look up any given
;; @c  function/variable using this kind of code (apropos-internal
;; @c  "\^emacsy.*").  Refer to ice-9 readline package for an example of
;; @c  its usage.}
;;
;; The class holds the entries, a string completer for tab completion,
;; and potentially a parent command map.
;;
;; @c \todo[inline]{Wouldn't this better be thought of as a command set
;; @c  rather than map.  Also, having it as a map means there could be two
;; @c  different implementations of the command; the one referred to by the
;; @c  procedure, and the one referred to in the map.  They could be become
;; @c  unsynchronized.}

;;; Code:

(define-module (emacsy command)
  #:use-module (string completion)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy coroutine)
  #:export-syntax (export-command))

;;; \todo[inline]{Perhaps procedure-properties should be used to denote a
;;;   procedure as a command?}

;;.
(define-public (module-command-interface mod)
  (unless (module-variable mod '%module-command-interface)
    (module-define! mod '%module-command-interface
                    (let ((iface (make-module)))
                      (set-module-name! iface (module-name mod))
                      (set-module-version! iface (module-version mod))
                      (set-module-kind! iface 'command)
                      ;(module-use! iface (resolve-interface '(guile)))
                      iface)))
  (module-ref mod '%module-command-interface))

;;.
(define-public (module-export-command! m names)
  (let ((public-i (module-command-interface m)))
    ;; Add them to this module.
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  (module-add! public-i external-name var)))
              names)))

;;.
(define-syntax-rule (export-command name ...)
  (eval-when (eval load compile expand)
    (call-with-deferred-observers
     (lambda ()
       (module-export-command! (current-module) '(name ...))))))

;;.
(define-syntax-public define-interactive
  (syntax-rules ()
    ((define-interactive (name . args) . body)
     (begin (define-cmd global-cmdset (name . args)
              . body)
            (export-command name)))
    ((define-interactive name value)
     (begin (define-cmd global-cmdset name value)
            (export-command name))
     )))

;;; \todo[inline]{Need to fix: define-cmd doesn't respect documentation strings.}

;;.
(define-syntax-public define-cmd
  (lambda (x)
    (syntax-case x ()
      ((define-cmd (name . args) e0)
       #'(begin
           (define* (name . args)
             (with-fluids ((in-what-command 'name))
               e0))
           (export name)
           (emacsy-kind-set!
            (module-variable (current-module) 'name)
            'command)
           (set-command-properties! name 'name)))
      ((define-cmd (name . args) e0 e1 . body)
       (string? (syntax->datum #'e0))
       ;; Handle the case where there is a documentation string.
       #'(begin
           (define* (name . args)
                      e0
                      (with-fluids ((in-what-command 'name))
                        (let ()
                          e1 . body)))
           (export name)
           (emacsy-kind-set!
            (module-variable (current-module) 'name)
            'command)
           (set-command-properties! name 'name)))
      ((define-cmd (name . args) e0 e1 . body)
       #'(begin
           (define* (name . args)
                      (with-fluids ((in-what-command 'name))
                        (let ()
                          e0 e1 . body)))
           (export name)
           (emacsy-kind-set!
            (module-variable (current-module) 'name)
            'command)
           (set-command-properties! name 'name)))
      ((define-cmd name value)
       #'(begin
           (define name #f)
           (let ((v value))
             (set! name (colambda args
                          (with-fluids ((in-what-command 'name))
                            (apply v args))))
             (export name)
             (emacsy-kind-set!
              (module-variable (current-module) 'name)
              'command)
             (set-command-properties! name 'name))))
      ((define-cmd cmap (name . args) . body)
       #'(begin
           (define-cmd (name . args) . body)
           (command-add! cmap 'name)))
      ((define-cmd cmap name value)
       #'(begin
           (define-cmd name value)
           (command-add! cmap 'name))))))

;;.
(define-syntax-public lambda-cmd
  (syntax-rules ()
    ((lambda-cmd args . body)
     (let ((proc (lambda* args
                          (with-fluids ((in-what-command #f))
                            . body))))
       (set-command-properties! proc)
       proc))))

(define-class-public <command-set> ()
  (commands #:getter commands #:init-form (list))
  (completer #:getter completer #:init-form (make <string-completer>))
  (parent #:accessor parent #:init-keyword #:parent #:init-value #f))
(export commands completer)

;; The global command map.

(define-public global-cmdset (make <command-set>))

;;.
(define in-what-command (make-fluid #f))

;;.
(define-public this-command #f)

;;.
(define-public last-command #f)

;;.
(define-public kill-rogue-coroutine? #f)

;;.
(define-public seconds-to-wait-for-yield 2)

;;.
(define this-interactive-command (make-fluid))

;; We have accessors for adding, removing, and testing what's in the
;; set. Note that the parent set is never mutated.
(define-method-public (command-contains? (cmap <command-set>) command-symbol)
  (or (memq command-symbol (commands cmap))
      (and (parent cmap) (command-contains? (parent cmap) command-symbol))))

;;.
(define-method-public (command-add! (cmap <command-set>) command-symbol)
  (when (not (command-contains? cmap command-symbol))
      (add-strings! (completer cmap) (list (symbol->string command-symbol)))
      (slot-set! cmap 'commands (cons command-symbol (commands cmap)))))

;;.
(define-method-public (command-remove! (cmap <command-set>) command-symbol)
  (when (command-contains? cmap command-symbol)
    (slot-set! cmap 'commands (delq! command-symbol (commands cmap)))
    ;; Must rebuild the completer.
    (let ((c (make <string-completer>)))
      (add-strings! c (map symbol->string (commands cmap)))
      (slot-set! cmap 'completer c))))

;;.
(define-public (register-interactive name proc)
  (command-add! global-cmdset name)
  (set-command-properties! proc name))

;;.
(define-public (command->proc command)
  (cond
   ((thunk? command)
    command)
   (else
    (warn "command->proc not given a command: ~a" command)
    #f)))

;;.
(define-public (command-name command)
  (procedure-name command))

;;.
(define-public (command? object)
  (thunk? object))

;; @subsection Determine Interactivity
;;
;; We would like to be able to determine within the command procedure's
;; body whether the command has been called interactively, by the user's
;; key press, or by a keyboard macro or another procedure call.  The best
;; way I can think to do this is to have a means of answering the
;; following questions: 1) What command am I in? 2) What is the current
;; interactive command?
;;
;; Determining the current command is not that difficult.  That's
;; generally set by the [[this-command]] variable.  However, determining
;; what command I am in is a little troublesome.  One can examine the
;; stack and look for the first procedure that has some property
;; associated with commands.

;;.
(define* (set-command-properties! proc #:optional (name #f))
  (let ((cname (or name (procedure-name proc) #f)))
    (set-procedure-property! proc 'command-name
                             (if (eq? cname 'proc)
                                 #f
                                 cname))))
;;.
(define-public (what-command-am-i?)
  (fluid-ref in-what-command))

;;.
(define-public (command-execute command . args)
  (if (command? command)
      (let ((cmd-proc (command->proc command))
            (cmd-name (command-name command)))
        (emacsy-log-info "Running command: ~a" cmd-name)
        (set! last-command this-command)
        (set! this-command cmd-name)
        (apply cmd-proc args))
      (error (emacsy-log-warning "command-execute not given a command: ~a" command))))

;;.
(define-public (call-interactively command . args)
  (dynamic-wind
   (lambda () (if kill-rogue-coroutine?
                  (alarm seconds-to-wait-for-yield)))
   (lambda () (with-fluids ((this-interactive-command (command-name command)))
                (apply command-execute command args)))
   (lambda () (if kill-rogue-coroutine?
                  (alarm 0)))))

;;.
(define*-public (called-interactively? #:optional (kind 'any))
  (eq? (fluid-ref in-what-command) (fluid-ref this-interactive-command)))
