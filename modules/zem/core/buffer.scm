(define-module (zem core buffer)
  #:use-module (zem core mode)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 curried-definitions)
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

(define newline-regex (make-regexp "\\\n"))

(define-public (count-lines start end)
  (if (> end start)
      (1+ (string-count (substring (buffer-string) start (- end start)) #\newline))
      0))

(define*-public (line-number-at-pos #:optional (pos (point)))
  (max 1 (count-lines (point-min) pos)))

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
