;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (emacsy text)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (zem api rope)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy mru-stack)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy event)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (emacsy mode)
  #:use-module (emacsy buffer)
  #:declarative? #f)

;;; Commentary:

;; @node Text
;; @section Text

;; Editing and stuff.

;;; Code:

;; Our minibuffer and messages buffer require a text buffer.  We try to
;; stay very close to the Emacs interface with respect to naming,
;; optional parameters and interactiveness.

;;.
(define-public (buffer-string)
  ((compose buffer:buffer-string current-buffer)))

;;.
(define-public (buffer-substring start end)
  (buffer:buffer-substring (current-buffer) start end))

;;.
(define-public (point)
  ((compose buffer:point current-buffer)))

;;.
(define-public (point-min)
  ((compose buffer:point-min current-buffer)))

;;.
(define-interactive (beginning-of-buffer #:optional arg)
  (goto-char (point-min)))

;;.
(define-public (point-max)
  ((compose buffer:point-max current-buffer)))

;;.
(define-interactive (end-of-buffer #:optional arg)
  (goto-char (point-max)))

;;.
(define*-public (mark #:optional force)
  (buffer:mark (current-buffer)))

;;.
(define-public (set-mark pos)
  (buffer:set-mark (current-buffer) pos))

;;.
(define-interactive (set-mark-command #:optional arg)
  (if arg (goto-char (mark))
      (set-mark (point))))

;;.
(define-interactive (mark-whole-buffer)
  (goto-char (point-min))
  (set-mark (point))
  (goto-char (point-max)))

;;.
(define-interactive (exchange-point-and-mark)
  (let ((m (mark)))
    (set-mark (point))
    (goto-char m)))

;;.
(define*-public (char-after #:optional (point (point)))
  (buffer:char-after (current-buffer) point))

(define*-public (char-before #:optional (point (point)))
  (buffer:char-before (current-buffer) point))

;;.
(define-interactive (goto-char #:optional (point (point)))
  (buffer:goto-char (current-buffer) point))

;;.
(define-interactive (forward-char #:optional (n 1))
  (buffer:goto-char (current-buffer) (+ (point) n)))

;;.
(define-interactive (backward-char #:optional (n 1))
  (forward-char (- n)))

;;.
(define-interactive (beginning-of-line #:optional (n 1))
  (buffer:beginning-of-line (current-buffer) n))

;;.
(define-interactive (end-of-line #:optional (n 1))
  (buffer:end-of-line (current-buffer) n))

;;.
(define-interactive (move-beginning-of-line #:optional (n 1))
  (beginning-of-line n))

;;.
(define-interactive (move-end-of-line #:optional (n 1))
  (end-of-line n))

;;.
(define-interactive (re-search-forward regex #:optional (bound #f) (no-error? #f) (repeat 1))
  (buffer:re-search-forward (current-buffer) regex bound no-error? repeat))


;;.
(define-interactive (re-search-backward regex #:optional (bound #f) (no-error? #f) (repeat 1))
  (buffer:re-search-backward (current-buffer) regex bound no-error? repeat))


(define forward-word-regex (make-regexp "\\s*\\w+\\b"))
;;.
(define-interactive (forward-word #:optional (n 1))
  (if (< n 0) (backward-word (- n))
      (re-search-forward forward-word-regex #f #t n)))

(define backward-word-regex (make-regexp "\\b\\w+\\s*"))
;;.
(define-interactive (backward-word #:optional (n 1))
  (if (< n 0) (forward-word (- n))
      (re-search-backward backward-word-regex #f #t n)))

(define %target-column #f)
;;.
(define-interactive (forward-line #:optional (n 1))
  (unless (zero? n)
    (if (< n 0) (backward-line (- n))
        (let* ((column (current-column))
               (len (line-length))
               (pos (+ (point) (- len column))))
          (when (eq? pos (1- (point-max)))
            (set! pos (1+ pos)))
          (unless (and %target-column
                       (or (eq? last-command this-command)
                           (eq? last-command 'previous-line-trampoline)
                           (eq? last-command 'next-line-trampoline)))
            (set! %target-column column))
          (if (>= pos (point-max)) (begin
                                     (goto-char (point-max))
                                     (when #f ;;(> (current-column) %target-column)
                                       (goto-char (- (point) (- (current-column) %target-column)))))
              (begin
                (goto-char pos)
                (let ((len (line-length)))
                  (goto-char (+ (point) (min (1- len) %target-column))))
                (when (and (< (min (1- len) %target-column))
                           (eq? (point) (1- (point-max)))
                           (eq? 0 (and=> (char-after (1+ (point))) char->integer)))
                  (goto-char (point-max)))
                (forward-line (1- n))))))))

;;.
(define*-public (backward-line #:optional (n 1))
  (unless (zero? n)
    (if (< n 0) (forward-line (- n))
        (let* ((column (current-column))
               (pos (- (point) column 1)))
          (unless (and %target-column
                       (or (eq? last-command this-command)
                           (eq? last-command 'previous-line-trampoline)
                           (eq? last-command 'next-line-trampoline)))
            (set! %target-column column))
          (if (< pos 1) (goto-char 0)
              (begin
                (goto-char pos)
                (let ((len (line-length)))
                  (goto-char (- (point) (min (1- len) (current-column))))
                  (goto-char (+ (point) (min (1- len) %target-column)))
                  (backward-line (1- n)))))))))

(define-public (current-column)
  (buffer:current-column (current-buffer)))

(define-public (line-length)
  (buffer:line-length (current-buffer)))

(define-interactive (next-line #:optional (n 1))
  (forward-line n))

(define-interactive (previous-line #:optional (n 1))
  (backward-line n))

;;.
(define (signal-before-change start end)
  (run-hook before-buffer-change-hook (current-buffer))
  (run-hook (local-var 'before-change-functions) start end))

;;.
(define (signal-after-change charpos lendel lenins)
  (run-hook after-buffer-change-hook (current-buffer))
  (run-hook (local-var 'after-change-functions) charpos (+ charpos lenins) lendel))

;;.
(define-method-public (insert-char char)
  (buffer:insert-string (current-buffer) char))

;;.
(define-interactive (insert #:rest args)
  (and (current-buffer)
       (if (null? args) 0
           (let ((arg (car args))
                 (opoint (point)))
             (signal-before-change opoint opoint)
             (cond
              ((string? arg)
               (buffer:insert-string (current-buffer) arg))
              ((char? arg)
               (buffer:insert-char (current-buffer) arg))
              (else #f))
             (set! (buffer-modified? (current-buffer)) #t)
             (incr! (buffer-modified-tick (current-buffer)))
             (signal-after-change opoint 0 (- (point) opoint))))))

;;.
(define-interactive (self-insert-command #:optional (n 1))
  (and (>= n 1)
       (insert (command-char this-command-event))))

;; .
(define-variable kill-ring '("")
  "List of killed text sequences.")

(define (kill-command? command)
  (memq command '(kill-region-trampoline kill-line-trampoline kill-word-trampoline)))

(define-interactive (append-next-kill #:optional interactive?)
  #t)

(define (append-kill?)
  (memq last-command '(append-next-kill-trampoline kill-region-trampoline kill-line-trampoline kill-word-trampoline)))

(define (add-kill! text)
  (set! kill-ring
        (if (append-kill?) (cons (string-append (car kill-ring) text) (cdr kill-ring))
            (cons text kill-ring))))

(define-interactive (yank)
  (when (pair? kill-ring)
    (insert (car kill-ring))))

(define-interactive (yank-pop)
  (cond ((or (eq? last-command this-command)
             (eq? last-command 'yank-trampoline))
         (let ((len (string-length (car kill-ring))))
           (goto-char (- (point) len))
           (delete-region (point) (+ (point) len))
           (set! kill-ring (append (cdr kill-ring) (list-tail kill-ring 1)))
           (yank)))
        (else (message "Previous command was not a yank"))))

(define-interactive (delete-char #:optional (n 1))
  (let ((beg (point)))
    (forward-char n)
    (delete-region beg (point))))

;;.
(define-interactive (delete-forward-char #:optional (n 1))
  "Delete the following N characters (previous if N is negative)."
  (buffer:delete-char (current-buffer) n))

;; Alias for delete-forward-char
(define-interactive (forward-delete-char #:optional (n 1))
  "Alias for delete-forward-char."
  (buffer:delete-char (current-buffer) n))

;;.
(define-interactive (delete-backward-char #:optional (n 1))
  "Delete the previous N characters (following if N is negative)."
  (let ((start (max (- (point) n) 0))
        (end (point)))
    (signal-before-change start end)
    (buffer:delete-char (current-buffer) (- n))
    (signal-after-change start n 0)))

;; Alias for delete-backward-char
(define-interactive (backward-delete-char #:optional (n 1))
  "Alias for delete-backward-char."
  (delete-backward-char n))

;;.
(define-interactive (delete-region #:optional (start (point)) (end (mark)))
  (signal-before-change start end)
  (let ((text (buffer:delete-region (current-buffer) start end)))
    (when text
      (set! (buffer-modified? (current-buffer)) #t)
      (incr! (buffer-modified-tick (current-buffer)))
      (signal-after-change start (- end start) 0))
    text))

;;.
(define-interactive (kill-region #:optional (start (point)) (end (mark)))
  (add-kill!
   (delete-region (if (> end start) start end) (if (> end start) end start))))

;;.
(define-public (delete-line n)
  (let ((beg (point))
        (column (current-column))
        (len (line-length)))
    (forward-line n)
    (unless (or (and (zero? column)
                     (= len 1))
                (eq? (point) (point-max)))
      (goto-char (- (point) (current-column) 1)))
    (delete-region beg (point))))

;;.
(define-interactive (kill-line #:optional (n 1))
  (add-kill! (delete-line n)))

;;.
(define-public (delete-word n)
  (let ((beg (point)))
    (forward-word n)
    (delete-region beg (point))))

;;.
(define-interactive (kill-word #:optional (n 1))
  (add-kill! (delete-word n)))

;;.
(define-interactive (backward-kill-word #:optional (n 1))
  (kill-word (- n)))

;; @subsection Optional text buffer interface
;;
;; A child of <buffer>, such as <text-buffer>, <minibuffer> or a custom UI buffer
;; may override these, for efficiency or otherwise.
;;.

(define-method-public (buffer:re-search-forward (buffer <buffer>) regex bound no-error? repeat)
  (if (= repeat 0) (buffer:point buffer)
      (let* ((string (buffer:buffer-string buffer))
             (pt (buffer:point buffer))
             (m (regexp-exec regex string (1- pt))))
        (cond (m (buffer:goto-char buffer (1+ (match:end m 0)))
                 (buffer:re-search-forward buffer regex bound no-error? (1- repeat)))
              (no-error? #f)
              (else (scm-error 'no-match 're-search-forward
                               "No match found for regex '~a' in ~s after point ~a"
                               (list regex string pt) #f))))))

(define-method-public (buffer:re-search-backward (buffer <buffer>) regex bound no-error? repeat)
  (if (= repeat 0) (buffer:point buffer)
      (let loop ((start-search 0) (last-match-start #f))
        (let* ((string (buffer:buffer-string buffer))
               (pt (buffer:point buffer))
               (m (regexp-exec regex string start-search)))
          (cond ((and m (< (match:start m 0) (1- pt)))
                 (loop (match:end m 0) (match:start m 0)))
                (last-match-start
                 (buffer:goto-char buffer (1+ last-match-start))
                 (buffer:re-search-backward buffer regex bound no-error? (1- repeat)))
                (no-error? #f)
                (else (scm-error
                       'no-match 're-search-backward
                       "No match found for regex '~a' in ~s before point ~a"
                       (list regex string pt) #f)))))))


;; @var{<text-buffer>} inherits from buffer and implements the simplest
;; text editing for the Gap Buffer.
;;.
(define-class-public <text-buffer> (<buffer>)
  ;;define-class <text-buffer> (<buffer>)
  (rope-buffer #:accessor rope-buffer #:init-form (make-rope ""))
  (intervals #:accessor buffer-intervals #:init-form '())
  (marker #:accessor buffer-marker #:init-value #f))
(export rope-buffer buffer-intervals)

;;.
(define newline-regex (make-regexp "\\\n"))

;;.
(define-method-public (buffer:line-length (buffer <buffer>))
  (let ((start (1+ (or (save-excursion (move-to-last-newline)) 0)))
        (end (or (save-excursion (re-search-forward newline-regex #f #t)) (point-max))))
    (- end start)))

(define-method-public (buffer:line-length (buffer <text-buffer>))
  (let ((start (1+ (or (save-excursion (move-to-last-newline)) 0)))
        (end (save-excursion (rope-next-lines (rope-buffer buffer) 1))))
    (- end start)))


;;.
(define (move-to-last-newline)
  (let ((c (char-before)))
    (cond
     ((= (point) (point-min))
      #f)
     ((eqv? c #\newline)
      (backward-char)
      (point))
     (else
      (backward-char)
      (move-to-last-newline)))))

;;.
(define-method-public (buffer:current-column (buffer <buffer>))
  (let ((start (1+ (or (save-excursion (move-to-last-newline)) 0))))
    (- (point) start)))

;;.
(define-method-public (buffer:beginning-of-line (buffer <buffer>) n)
  (goto-char (- (point) (current-column))))

;;.
(define-method-public (buffer:end-of-line (buffer <buffer>) n)
  (goto-char (+ (point) (- (line-length) (current-column) 1)))
  (when (and (eq? (point) (1- (point-max)))
             (not (eqv? (char-after) #\newline)))
    (goto-char (point-max))))

;;.
(define-method-public (buffer:set-mark (buffer <buffer>) pos)
  (message "buffer:set-mark not implemented" buffer))

;;.
(define-method-public (buffer:mark (buffer <buffer>))
  (message "buffer:mark not implemented" buffer))

;; @subsection Editing for Gap Buffer
;;.

(define (rope-char-before rope point)
  (and (> point (rope-point-min rope))
       (string-ref (rope-substr rope (1- point) 1) 0)))

(define (rope-char-after rope point)
  (and (< point (rope-point-max rope))
       (string-ref (rope-substr rope point 1) 0)))

;;.
(define-method-public (buffer:buffer-string (buffer <text-buffer>))
  (rope->string (rope-buffer buffer)))

;;.
(define-method-public (buffer:buffer-substring (buffer <text-buffer>) start end)
  (rope-substr (rope-buffer buffer) start (- end start)))

;;.
(define-method-public (buffer:goto-char (buffer <text-buffer>) pos)
  (rope-goto-char (rope-buffer buffer) pos))

;;.
(define-method-public (buffer:point (buffer <text-buffer>))
  (rope-point (rope-buffer buffer)))

;;.
(define-method-public (buffer:point-min (buffer <text-buffer>))
  (rope-point-min (rope-buffer buffer)))

;;.
(define-method-public (buffer:point-max (buffer <text-buffer>))
  (rope-point-max (rope-buffer buffer)))

;;.
(define-method-public (buffer:set-mark (buffer <text-buffer>) pos)
  (set! (buffer-marker buffer) pos))

;;.
(define-method-public (buffer:mark (buffer <text-buffer>))
  (buffer-marker buffer))

;;.
(define-method-public (buffer:char-before (buffer <text-buffer>) point)
  (rope-char-before (rope-buffer buffer) point))

;;.
(define-method-public (buffer:char-after (buffer <text-buffer>) pos)
  (rope-char-after (rope-buffer buffer) pos))

;;.
(define-method-public (buffer:insert-string (buffer <text-buffer>) string)
  (rope-insert-string! (rope-buffer buffer) string))

;;.
(define-method-public (buffer:insert-char (buffer <text-buffer>) char)
  (rope-insert-char! (rope-buffer buffer) (char->integer char)))

;;.
(define-method-public (buffer:delete-char (buffer <text-buffer>) n)
  (rope-delete-char! (rope-buffer buffer) n))

;;.
(define-method-public (buffer:delete-region (buffer <text-buffer>) start end)
  (let* ((point (buffer:point buffer))
         (rope (rope-buffer buffer))
         (s (if (< start end) start end))
         (e (if (< start end) end start)))
    (goto-char s)
    (let ((text (buffer:buffer-substring buffer s e))
          (point (cond ((> point e) (- point e s))
                       ((> point s) s)
                       (else point))))
      (rope-delete-char! (rope-buffer buffer) (- e s))
      (goto-char point)
      text)))
