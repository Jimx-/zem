(define-module (zem core buffer)
  #:use-module (zem core mode)
  #:use-module (zem api rope)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 curried-definitions)
  #:use-module (rnrs io ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1))

(define-public (position-bytes pos) pos)

(define-public (get-buffer name)
  (find (lambda (buffer)
          (string=? (buffer-name buffer) name))
        (buffer-list)))

(define-public (get-buffer-create name)
  (or (get-buffer name)
      (let ((new-buffer (make <text-buffer> #:name name)))
        (add-buffer! new-buffer)
        new-buffer)))

(define-public (bolp)
  (let ((c (char-before)))
    (and c (eqv? c #\newline))))

(define newline-regex (make-regexp "\\\n"))

(define-method (buffer:count-lines (buffer <buffer>) start end)
  (with-buffer buffer
    (if (> end start)
        (string-count (substring (buffer-string) start (- end start)) #\newline)
        0)))

(define-method (buffer:count-lines (buffer <text-buffer>) start end)
  (if (> end start)
      (rope-count-lines (rope-buffer buffer) start (- end start))
      0))

(define-public (count-lines start end)
  (save-excursion
      (let ((lines (buffer:count-lines (current-buffer) start end)))
        (goto-char end)
        (if (and (> end start)
                 (not (bolp)))
            (1+ lines)
            lines))))

(define-interactive (count-lines-page)
  (let ((total (count-lines (point-min) (point-max)))
        (before (count-lines (point-min) (point)))
        (after (count-lines (point) (point-max))))
    (message "Page has ~a line(s) (~a + ~a)" total before after)))

(define*-public (line-number-at-pos #:optional (pos (point)))
  (let ((lines (count-lines (point-min) pos)))
    (if (bolp)
        (1+ lines)
        lines)))

(define-interactive (goto-line #:optional line)
  #t)
(define-interactive (goto-line #:optional (line (string->number
                                                 (read-from-minibuffer "Goto line: "))))
  (goto-char (point-min))
  (re-search-forward newline-regex #f #t (max 0 (- line 1))))
(define-key global-map "M-g M-g" 'goto-line)

(define (after-find-file)
  (set-auto-mode))

(add-hook! find-file-hook after-find-file)

(define-interactive (save-buffer #:optional arg)
  (if (buffer-modified?)
      (let ((filename (or (buffer-file-name (current-buffer))
                          (expand-file-name (read-file-name "File to save in: ")))))
        (call-with-output-file filename (lambda (port) (put-string port (buffer-string))))
        (set! (buffer-modified? (current-buffer)) #f)
        (message "Wrote ~a" filename))
      (message "(No changes need to be saved)")))
(define-key global-map "C-x C-s" 'save-buffer)
