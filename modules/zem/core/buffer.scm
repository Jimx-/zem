(define-module (zem core buffer)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1))

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
  (save-excursion
      (goto-char (point-min))
    (let* ((max-point (max start end))
           (done1 (let loop ((x 0)
                             (p (point)))
                    (if (and
                         (re-search-forward newline-regex #f #t 40)
                         (<= (point) max-point))
                        (loop (+ x 40)
                              (point))
                        (begin
                          (goto-char p)
                          x))))
           (done2 (let loop ((x 0))
                    (if (and
                         (re-search-forward newline-regex #f #t 1)
                         (<= (point) max-point))
                        (loop (1+ x))
                        x)))
           (done (+ done1 done2)))
      (goto-char max-point)
      (if (and (not (= start end)))
          (1+ done)
          done))))

(define*-public (line-number-at-pos #:optional (pos (point)))
  (count-lines (point-min) pos))
