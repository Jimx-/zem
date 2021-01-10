(define-module (zem core commands)
  #:use-module (emacsy emacsy)
  #:use-module (statprof))

(define newline-regex (make-regexp "\\\n"))

(define-interactive (goto-line #:optional line)
  #t)

(define-interactive (goto-line #:optional (line (string->number
                                                 (read-from-minibuffer "Goto line: "))))
  (goto-char (point-min))
  (re-search-forward newline-regex #f #t (max 0 (- line 1))))

(define-key global-map "M-g M-g" 'goto-line)

(define-interactive (profiler-start)
  (statprof-start))

(define-interactive (profiler-end)
  (statprof-stop))

(define-interactive (profiler-report)
  (statprof-display))
