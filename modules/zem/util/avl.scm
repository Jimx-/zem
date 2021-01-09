(define-module (zem util avl)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9 gnu)
  #:export (<?
            >?
            make-empty-avl
            avl-empty?
            avl-insert
            avl-erase
            avl-update
            avl-start-iter-equal
            avl-start-iter-greater
            avl-start-iter-greater-equal
            avl-start-iter-less
            avl-start-iter-less-equal
            avl-iter-at
            avl-iter-incr
            avl-iter-decr
            avl-for-each))

(define-method (<? a . rest) #f)
(define-method (>? a . rest) #f)

(define-method (<? (a <number>) . rest) (apply < a rest))
(define-method (>? (a <number>) . rest) (apply < a rest))

(define-method (<? (a <string>) . rest) (apply string<? a rest))
(define-method (>? (a <string>) . rest) (apply string>? a rest))

(define-immutable-record-type <avl-node>
  (make-node key val left right)
  avl-node?
  (key node-key)
  (val node-val)
  (left node-left)
  (right node-right))

(define (node->kvpair node)
  (cons (node-key node) (node-val node)))

(define-immutable-record-type <avl-iter>
  (make-iter tree depth trace)
  avl-iter?
  (tree iter-tree)
  (depth iter-depth)
  (trace iter-trace))

(define (make-empty-avl) '())

(define avl-empty? null?)

(define (tree-height rt)
  (if (null? rt) 0
      (1+ (max (if (null? (node-left rt)) 0 (tree-height (node-left rt)))
               (if (null? (node-right rt)) 0 (tree-height (node-right rt)))))))

(define (avl-violate? tr)
  (if (null? tr) #f
      (let ((left-h (tree-height (node-left tr)))
            (right-h (tree-height (node-right tr))))
        (or (> right-h (1+ left-h)) (> left-h (1+ right-h))))))

(define (tree-single-right-rotate root)
  (let* ((k1 root)
         (k2 (node-left k1))
         (k3 (node-right k2))
         (k4 (node-left k2))
         (k5 (node-right k1)))
    (set-fields k2
                ((node-left) k4)
                ((node-right) (set-fields k1
                                          ((node-left) k3)
                                          ((node-right) k5))))))

(define (tree-single-left-rotate root)
  (let* ((k1 root)
         (k2 (node-right k1))
         (k3 (node-right k2))
         (k4 (node-left k2))
         (k5 (node-left k1)))
    (set-fields k2
                ((node-left) (set-fields k1
                                          ((node-left) k5)
                                          ((node-right) k4)))
                ((node-right) k3))))

(define (tree-left-right-rotate root)
  (tree-single-right-rotate
   (set-fields root
               ((node-left) (tree-single-left-rotate (node-left root))))))

(define (tree-right-left-rotate root)
  (tree-single-left-rotate
   (set-fields root
               ((node-right) (tree-single-right-rotate (node-right root))))))

(define (insert-fixup root trace)
  (if (or
       (null? trace)
       (null? (cdr trace)))
      root
      (match (cons (car trace) (cadr trace))
        ('(left . left) (tree-single-right-rotate root))
        ('(right . right) (tree-single-left-rotate root))
        ('(left . right) (tree-left-right-rotate root))
        ('(right . left) (tree-right-left-rotate root)))))

(define (node-choose-branch node dir)
  (if (eq? dir 'left)
      (node-left node)
      (node-right node)))

(define (insert-node/recur root key val)
  (if (null? root)
      (list (make-node key val '() '()) '())
      (let* ((dir (if (<? key (node-key root))
                      'left
                      'right))
             (insert-res (insert-node/recur (node-choose-branch root dir) key val))
             (trace (cons dir (cdr insert-res)))
             (new-root (if (eq? dir 'left)
                           (set-fields root
                                       ((node-left) (car insert-res)))
                           (set-fields root
                                       ((node-right) (car insert-res))))))
        (cons (if (avl-violate? new-root)
                  (insert-fixup new-root trace)
                  new-root)
              trace))))

(define (avl-insert root key val)
  (car (insert-node/recur root key val)))

(define (delete-fixup root)
  (case (- (tree-height (node-left root)) (tree-height (node-right root)))
    ((-2) (if (> (tree-height (node-left (node-right root)))
                 (tree-height (node-right (node-right root))))
              (tree-right-left-rotate root)
              (tree-single-left-rotate root)))
    ((2) (if (< (tree-height (node-left (node-left root)))
                (tree-height (node-right (node-left root))))
             (tree-left-right-rotate root)
             (tree-single-right-rotate root)))))

(define (delete-min root)
  (define (delete-min/recur root)
    (cond ((null? root) (list '() #f #f))
          ((null? (node-left root)) (list
                                     (node-right root)
                                     (node-key root)
                                     (node-val root)))
          (else (let* ((delete-res (delete-min/recur (node-left root)))
                       (new-root (set-fields root
                                             ((node-left) (car delete-res))))
                       (kv (cdr delete-res)))
                  (cons
                   (if (avl-violate? new-root)
                       (delete-fixup new-root)
                       new-root)
                   kv)))))
  (delete-min/recur root))

(define-syntax choose-branch
    (syntax-rules ()
      ((_ root key left right equal)
       (cond
         ((<? key (node-key root)) left)
         ((>? key (node-key root)) right)
         (else equal)))))

(define (delete-node/recur root key)
  (let* ((delete-res
          (choose-branch
           root key
           (delete-node/recur (node-left root) key)
           (delete-node/recur (node-right root) key)
           (list (cond
                  ((and (null? (node-left root)) (null? (node-right root))) '())
                  ((null? (node-left root)) (node-right root))
                  ((null? (node-right root)) (node-left root))
                  (else (let* ((right-min-res (delete-min (node-right root)))
                               (right-min-kv (cdr right-min-res))
                               (right-node (car right-min-res)))
                          (make-node (car right-min-kv)
                                     (cadr right-min-kv)
                                     (node-left root)
                                     right-node))))
                 (node-key root)
                 (node-val root))))
         (updated-node (car delete-res))
         (new-root (choose-branch root key
                                  (set-fields root
                                              ((node-left) updated-node))
                                  (set-fields root
                                              ((node-right) updated-node))
                                  updated-node))
         (kv (cdr delete-res)))
    (cons (if (avl-violate? new-root)
              (delete-fixup new-root)
              new-root)
          kv)))

(define (avl-erase root key)
  (delete-node/recur root key))

(define (avl-update root key proc)
  (define (update-node/recur root key proc)
    (cond ((null? root)
           (cons #f '()))
          ((equal? key (node-key root))
           (cons #t
                 (set-fields root
                             ((node-val) (proc (node-val root))))))
          (else
           (match-let* ((dir (if (<? key (node-key root)) 'left 'right))
                        ((updated . updated-node)
                         (update-node/recur (node-choose-branch root dir)
                                            key
                                            proc)))
                       (cons updated
                             (if (eq? dir 'left)
                                 (set-fields root
                                             ((node-left) updated-node))
                                 (set-fields root
                                             ((node-right) updated-node))))))))
  (update-node/recur root key proc))

(define (avl-start-iter root key preds depth)
  (if (null? root)
      (make-iter root #f '())
      (let ((key-equal? (equal? key (node-key root))))
        (if (and key-equal? (memq 'equal preds))
            (make-iter root depth '())
            (let* ((key-<? (<? key (node-key root)))
                   (dir (cond
                         (key-equal? (if (memq 'less preds) 'left 'right))
                         (key-<? 'left)
                         (else 'right)))
                   (next (avl-start-iter (node-choose-branch root dir)
                                         key
                                         preds
                                         (1+ depth)))
                   (cur-depth (if (or
                                   (and (memq 'less preds)
                                        (eq? dir 'right))
                                   (and (memq 'greater preds)
                                        (eq? dir 'left)))
                                  depth
                                  #f))
                   (cur-trace (cons (cons (node-choose-branch root dir)
                                          dir)
                                    (iter-trace next))))
              (make-iter root
                         (or (iter-depth next) cur-depth)
                         cur-trace))))))

(define (avl-start-iter-equal root key)
  (avl-start-iter root key '(equal) 0))
(define (avl-start-iter-less root key)
  (avl-start-iter root key '(less) 0))
(define (avl-start-iter-greater root key)
  (avl-start-iter root key '(greater) 0))
(define (avl-start-iter-less-equal root key)
  (avl-start-iter root key '(less equal) 0))
(define (avl-start-iter-greater-equal root key)
  (avl-start-iter root key '(greater equal) 0))

(define (avl-iter-node iter)
  (cond
   ((not (iter-depth iter)) #f)
   ((zero? (iter-depth iter)) (iter-tree iter))
   (else (car (list-ref (iter-trace iter) (- (iter-depth iter) 1))))))

(define (avl-iter-dir iter depth)
   (cdr (list-ref (iter-trace iter) (- depth 1))))

(define (avl-iter-at iter)
  (let ((node (avl-iter-node iter)))
    (and node (node->kvpair node))))

(define (avl-iter-backup/recur iter dir)
  (cond
   ((or
     (not (iter-depth iter))
     (negative? (iter-depth iter)))
    (make-iter (iter-tree iter) #f '()))
   ((eq? dir (avl-iter-dir iter (1+ (iter-depth iter))))
    (avl-iter-backup/recur (set-fields iter
                                       ((iter-depth) (- (iter-depth iter) 1))) dir))
   (else iter)))

(define (avl-iter-backup iter dir)
  (if (or
       (not (iter-depth iter))
       (zero? (iter-depth iter)))
      (make-iter (iter-tree iter) #f '())
      (avl-iter-backup/recur (set-fields iter
                                         ((iter-depth) (- (iter-depth iter) 1))) dir)))

(define (avl-iter-descend node dir depth)
  (if (null? node)
      (cons depth '())
      (match-let (((new-depth . trace)
                   (avl-iter-descend (node-choose-branch node dir)
                                     dir
                                     depth)))
                 (cons (1+ new-depth)
                       (cons (cons (node-choose-branch node dir)
                                   dir)
                             trace)))))

(define (avl-iter-incr iter)
  (let ((node (avl-iter-node iter)))
    (if node
        (let ((cur (node-right node)))
          (if (null? cur)
              (avl-iter-backup iter 'right)
              (match-let* (((new-depth . next-trace)
                            (avl-iter-descend cur
                                              'left
                                              (iter-depth iter)))
                           (new-trace (append (list-head (iter-trace iter) (iter-depth iter))
                                              (cons (cons cur 'right)
                                                    next-trace))))
                          (make-iter (iter-tree iter) new-depth new-trace))))
        (make-iter (iter-tree iter) #f '()))))

(define (avl-iter-decr iter)
  (let ((node (avl-iter-node iter)))
    (if node
        (let ((cur (node-left node)))
          (if (null? cur)
              (avl-iter-backup iter 'left)
              (match-let* (((new-depth . next-trace)
                            (avl-iter-descend cur
                                              'right
                                              (iter-depth iter)))
                           (new-trace (append (list-head (iter-trace iter) (iter-depth iter))
                                              (cons (cons cur 'left)
                                                    next-trace))))
                          (make-iter (iter-tree iter) new-depth new-trace))))
        (make-iter (iter-tree iter) #f '()))))

(define (avl-for-each root proc)
  (when (not (null? root))
    (avl-for-each (node-left root) proc)
    (proc (node-key root) (node-val root))
    (avl-for-each (node-right root) proc)))
