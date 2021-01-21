(define-module (zem syntax tree-sitter)
  #:use-module (zem core buffer)
  #:use-module (zem core text-prop)
  #:use-module (zem core mode)
  #:use-module (zem core font-lock)
  #:use-module (zem progmodes cc-mode)
  #:use-module (ice-9 match)
  #:use-module (emacsy emacsy))

(load-extension "libguile-tree-sitter" "init_tree_sitter")

(define* (position->ts-point #:optional (pos (point)))
  (save-excursion
      (goto-char pos)
    (cons (line-number-at-pos) (current-column))))

(define (ts-before-change start end)
  (let ((start-byte (position-bytes start))
        (end-byte (position-bytes end))
        (start-point (position->ts-point start))
        (end-point (position->ts-point end)))
    (set! (local-var 'tree-sitter:before-change-point) (vector start-byte end-byte start-point end-point))))

(define (ts-after-change start end len)
  (match-let ((#(start-byte old-end-byte start-point old-end-point)
               (local-var 'tree-sitter:before-change-point)))
             (tree-edit (local-var 'tree-sitter:tree)
                              start-byte
                              old-end-byte
                              (position-bytes end)
                              start-point
                              old-end-point
                              (position->ts-point end))
             (update-buffer (current-buffer))))

(define (highlight-query-capture-mapper name)
  (cond
   ((string= name "keyword") font-lock-keyword-face)
   ((string= name "number") font-lock-constant-face)
   ((string= name "string") font-lock-string-face)
   ((string= name "operator") font-lock-keyword-face)
   ((or
     (string= name "type")
     (string= name "type.builtin")) font-lock-type-face)
   ((or
     (string= name "function")
     (string= name "function.call")) font-lock-function-name-face)
   ((string= name "comment") font-lock-comment-face)
   (else 'default)))

(define (ensure-highlight-query)
  (unless (local-var 'tree-sitter:highlight-query)
    (set! (local-var 'tree-sitter:highlight-query)
          (query-new (local-var 'tree-sitter:language)
                     (local-var 'tree-sitter:highlight-patterns)
                     highlight-query-capture-mapper)))
  (local-var 'tree-sitter:highlight-query))

(define-public (setup-buffer buffer)
  (with-buffer buffer
    (let* ((parser (parser-new (local-var 'tree-sitter:language)))
           (tree (parser-parse-string parser (buffer-string))))
      (set! (local-var 'tree-sitter:parser) parser)
      (set! (local-var 'tree-sitter:tree) tree)
      (set! (local-var 'tree-sitter:query-cursor) (query-cursor-new))
      (set! (local-var 'tree-sitter:highlight-query) #f)
      (ensure-highlight-query)
      (highlight-region buffer (point-min) (point-max))
      (set! (local-var 'tree-sitter:before-change-point) #(0 0 (0 . 0) (0 . 0)))
      (add-hook! (local-var 'before-change-functions) ts-before-change)
      (add-hook! (local-var 'after-change-functions) ts-after-change))))

(define (invalidate-highlight old-tree)
  (let ((ranges (tree-changed-ranges old-tree (local-var 'tree-sitter:tree))))
    (for-each (match-lambda
               ((beg . end) (highlight-region (current-buffer) beg (point-max))))
              ranges)))


(define-public (update-buffer buffer)
  (with-buffer buffer
    (let ((old-tree (local-var 'tree-sitter:tree)))
      (set! (local-var 'tree-sitter:tree)
          (parser-parse-string (local-var 'tree-sitter:parser)
                                     (buffer-string)
                                     (local-var 'tree-sitter:tree)))
      (invalidate-highlight old-tree))))

(define (highlight-capture capture)
  (match capture
         (((start . end) . face)
          (when (not (= start end))
            (put-text-property start end 'face face)))))

(define-public (highlight-region buffer beg end)
  (with-buffer buffer
    (let ((cursor (local-var 'tree-sitter:query-cursor))
          (root (tree-root-node (local-var 'tree-sitter:tree))))
      (query-cursor-set-byte-range cursor
                                         (position-bytes beg)
                                         (position-bytes end))
      (let ((captures (query-cursor-captures
                       cursor
                       (local-var 'tree-sitter:highlight-query)
                       root)))
        (for-each highlight-capture captures)))))

(define-minor-mode tree-sitter-highlight-mode #f "TreeSitter-Hl"
  (font-lock-mode)
  (setup-buffer (current-buffer)))
