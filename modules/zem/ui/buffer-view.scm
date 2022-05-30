(define-module (zem ui buffer-view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module ((zem api font) #:prefix f:)
  #:use-module (zem core buffer)
  #:use-module (zem core commands)
  #:use-module (zem core text-prop)
  #:use-module (zem core faces)
  #:use-module (zem ui view)
  #:use-module (zem ui root-view)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module (zem util plist)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:export (<buffer-view>)
  #:declarative? #f)

(define-class <buffer-view> (<view>)
  (buffer #:init-keyword #:buffer #:accessor buffer-view:buffer)
  (mouse-selecting? #:init-value #f #:accessor buffer-view:mouse-selecting?)
  (last-point #:init-form '(0 1 0) #:accessor buffer-view:last-point)
  (last-visible-point-min #:init-form '(0 . 1) #:accessor buffer-view:last-visible-point-min)
  (display-matrix #:init-form '() #:accessor buffer-view:display-matrix)
  (suppressed-cursor-move? #:init-value #f #:accessor buffer-view:suppressed-cursor-move?))

(define-method (initialize (view <buffer-view>) . args)
  (next-method)
  (set! (view:cursor view) 'i-beam))

(define* (seconds->ticks seconds #:optional (default-ticks #f))
  (if ticks-per-second
      (* seconds ticks-per-second)
      default-ticks))

(define show-caret? #f)
(define (blink-period)
  (seconds->ticks 0.5 1))
(define (toggle-caret)
  (set! show-caret? (not show-caret?))
  (agenda-schedule toggle-caret (blink-period)))
(agenda-schedule toggle-caret (blink-period))

(define (get-line-height-font font)
  (floor (* (f:get-font-height font) 1.1)))

(define (get-line-height-default)
  (get-line-height-font (face-attribute 'default ':font)))

(define (get-line-height-max intervals)
  (fold (lambda (iv prev)
          (max prev (get-line-height-font (face-attribute (plist-get (cdr iv) 'face) ':font #t))))
        (get-line-height-default)
        intervals))

(define (get-text-y-offset font)
  (floor (/ (f:get-font-height font) 4.0)))

(define (get-text-x-offset line-max)
  (match-let* (((w . _) (r:text-size-hint (face-attribute 'line-number ':font #t)
                                          (number->string line-max))))
              (+ w (floor (* 1.5 (car style:padding))))))

(define (get-visible-line-range view line-max)
  (match-let* (((x0 y0 x1 y1) (view:get-visible-bbox view))
               (lh (get-line-height-default))
               (visible-line-min (max 1 (floor (/ y0 lh))))
               (visible-line-max (min line-max (1+ (floor (/ y1 lh))))))
              (cons (inexact->exact visible-line-min) (inexact->exact visible-line-max))))

(define (get-selection view)
  (let ((point (car (buffer-view:last-point view)))
        (mark (mark)))
    (and mark
         (cons (min mark point)
               (max mark point)))))

(define-method (view:get-scroll-limit (view <buffer-view>))
  (with-buffer (buffer-view:buffer view)
    (* (get-line-height-default) (- (count-lines (point-min) (point-max)) 1))))

(define (collect-line)
  (let ((opoint (point))
        (length (max 1 (line-length))))
    (forward-line)
    (move-beginning-of-line)
    (buffer-substring opoint (1- (+ opoint length)))))

(define (face->foreground face)
  (let ((foreground (face-attribute face ':foreground #t)))
    (if (eq? foreground 'unspecified)
        (face-attribute 'default ':foreground)
        foreground)))

(define (draw-caret pos line-height)
  (when show-caret?
    (r:add-rect pos (cons style:caret-width line-height) (face-attribute 'cursor ':foreground))))

(define (draw-line-number num x y width)
  (let ((font (face-attribute 'line-number ':font #t)))
    (r:add-text font
                (cons x
                      (- y (get-text-y-offset font)))
                (format #f (string-append "~" (number->string width) "@a") num)
                (face-attribute 'line-number ':foreground))))

(define (intersect-selection view fragment start)
  (let ((selection (get-selection view))
        (len (string-length fragment)))
    (and selection
         (cond
          ((>= start (cdr selection)) #f)
          ((<= (+ start len) (car selection)) #f)
          (else
           (cons
                 (max 0 (min (- (car selection) start) len))
                 (max 0 (min (- (cdr selection) start) len))))))))

(define (draw-mode-line x y width mode-line)
  (let* ((font (face-attribute 'mode-line ':font #t))
         (height (get-line-height-font font)))
    (r:add-rect (cons x (- y height))
                (cons width height)
                (face-attribute 'mode-line ':background))
    (r:add-text font
                (cons (+ x (car style:padding))
                      (- y (get-text-y-offset font)))
                mode-line
                (face-attribute 'mode-line ':foreground))))

;; Draw a single interval fontified by `put-text-property'.
(define (draw-interval view line col x y word face line-height cur-point point-line point-col)
  (let* ((draw-caret? (and (view:active? view)
                           (= line point-line)))
         (new-col (+ (string-length word) col))
         (new-point (+ (string-length word) cur-point))
         (selection (intersect-selection view word cur-point))
         (font (face-attribute face ':font #t))
         (foreground (face->foreground face))
         (pos (cons x (- y (get-text-y-offset font)))))
    (when selection
      (let ((lx (car (r:text-size-hint font (substring word 0 (car selection)))))
            (rx (car (r:text-size-hint font (substring word 0 (cdr selection))))))
        (r:add-rect (cons (+ x lx) (- y line-height))
                    (cons (- rx lx) line-height)
                    (face-attribute 'region ':background))))
    (when (and draw-caret?
               (>= point-col col)
               (< point-col new-col))
      (match-let* ((split (- point-col col))
                   (head (substring word 0 split))
                   ((hx . _) (if (string-null? head)
                                 (cons 0 0)
                                 (r:text-size-hint font head))))
                  (draw-caret (cons (+ x hx) (- y line-height))
                              line-height)))
    (list
     new-col
     (car (r:add-text font
                      pos
                      word
                      foreground))
     new-point
     (list pos font word))))

;; Draw all intervals (including filler text between intervals) in a line.
(define (draw-intervals view line intervals lidx col tx ty text-x line-height line-beg cur-point point-line point-col last-end row-acc)
  (let ((line-offset (lambda (pt)  ;; Map points to offsets within lines
                       (min (string-length line)
                            (max 0 (- pt line-beg))))))
    (if (not (null? intervals))
        (match-let*
         ((((start . end) . props) (car intervals))
          (token-min (line-offset start))
          (token-max (line-offset end))
          (filler (substring line last-end token-min))
          (text (substring line token-min token-max))
          ((fcol fx fpoint felt)
           ;; Draw filler text between the previous and the current interval
           (draw-interval view lidx col tx ty filler 'default line-height cur-point point-line point-col))
          ((ncol nx npoint nelt)
           ;; Draw the current interval
           (draw-interval view lidx fcol fx ty text (plist-get props 'face) line-height fpoint point-line point-col)))
         ;; Draw next line
         (draw-intervals view line (cdr intervals) lidx ncol nx ty text-x line-height line-beg npoint point-line point-col token-max (cons nelt (cons felt row-acc))))
        ;; Draw text at the end
        (match-let
         (((_ _ _ elt) (draw-interval view lidx col tx ty (substring line last-end) 'default line-height cur-point point-line point-col)))
          (cons elt row-acc)))))

(define (draw-line view lidx line line-beg line-max line-x text-x view-width y view-y point-line point-col)
  (let* ((line-end (+ line-beg (string-length line)))
         (intervals (text-property-list (current-buffer)
                                        line-beg
                                        line-end))
         (line-height (max
                       (get-line-height-max intervals)
                       (get-line-height-font (face-attribute 'line-number ':font #t))
                       (get-line-height-default)))
         (line-y (+ view-y y line-height)))
    (when (and
           (view:active? view)
           (= lidx point-line))
      ;; Highlight current line
      (r:add-rect (cons text-x (- line-y line-height))
                  (cons view-width line-height)
                  (face-attribute 'highlight ':background)))
    (draw-line-number lidx
                      line-x
                      line-y
                      (string-length (number->string line-max)))
    (let ((row (draw-intervals view (string-append line " ") intervals lidx 0 text-x line-y text-x line-height line-beg line-beg point-line point-col 0 '())))
      (cons line-height (cons line-y (reverse row))))))

(define (draw-lines view lidx visible-line-max line-max line-x text-x view-width y text-height view-y point-line point-col matrix-acc)
  (if (and (< (point) (point-max))
           (< y text-height)
           (<= lidx visible-line-max))
      (match-let*
       ((line-beg (point))
        (line (collect-line))
        ((line-height . row)
         (draw-line view lidx line line-beg line-max line-x text-x view-width y view-y point-line point-col)))
       (draw-lines view (1+ lidx) visible-line-max line-max line-x text-x view-width (+ y line-height) text-height view-y point-line point-col (cons row matrix-acc)))
      (cons y matrix-acc)))

(define (draw-scrollbar view visible-line-min visible-line-max line-max)
  (match-let* (((view-x . view-y) (view:pos view))
               ((view-width . view-height) (view:size view))
               (x (- (+ view-x view-width) style:scrollbar-size))
               (y (+ view-y (* view-height (/ visible-line-min line-max))))
               (h (max 20 (/ (- visible-line-max visible-line-min) line-max))))
              (r:add-rect (cons x y)
                          (cons style:scrollbar-size h)
                          (face-attribute 'scroll-bar ':foreground))))

(define (move-to-visible-point-min view visible-line-min)
  (if (not (= visible-line-min (cdr (buffer-view:last-visible-point-min view))))
      (begin
        (goto-line visible-line-min)
        (move-beginning-of-line)
        (set! (buffer-view:last-visible-point-min view) (cons (point) visible-line-min)))
      (goto-char (car (buffer-view:last-visible-point-min view))))
  (point))

(define-method (view:draw (view <buffer-view>))
  (draw-view-background view style:background-color)

  (with-buffer
      (buffer-view:buffer view)
    (when (not (= (point) (car (buffer-view:last-point view))))
      (update-point view))
    (let ((point-line (max 1 (cadr (buffer-view:last-point view))))
          (point-col (caddr (buffer-view:last-point view)))
          (mode-line (emacsy-mode-line)))
      (save-excursion
          (match-let*
           (((view-x . view-y) (view:pos view))           ;; View position
            ((view-width . view-height) (view:size view)) ;; View size
            (text-height view-height)              ;; Text area height
            (line-max                                     ;; Last line of buffer
             (1+ (count-lines (point-min) (point-max))))       ;;
            ((visible-line-min . visible-line-max)        ;; Visible line range
             (get-visible-line-range view line-max))      ;;
            (line-x (+ (car style:padding) view-x))       ;; Left of the gutter
            (text-x (+ (get-text-x-offset line-max) view-x))
            (mode-line-y text-height))
           (move-to-visible-point-min view visible-line-min)
           ;; Draw background for gutter
           (r:add-rect (cons view-x view-y)
                       (cons text-x text-height)
                       (face-attribute 'line-number ':background))
           ;; Iterate each line
           (match-let
            (((end-y . matrix) (draw-lines view visible-line-min visible-line-max line-max line-x text-x view-width 0 text-height view-y point-line point-col '())))
            (set! (buffer-view:display-matrix view) (reverse matrix))
            (when (and (= (car (buffer-view:last-point view)) (point-max))
                       (bolp))
              ;; Draw an empty last line
              (draw-line view line-max " " (point-max) line-max line-x text-x view-width end-y view-y point-line point-col)))
           (when (> line-max visible-line-max)
             (draw-scrollbar view visible-line-min visible-line-max line-max))
           (draw-mode-line view-x (+ view-y view-height) view-width mode-line))))))

(define-public (switch-buffer-view-buffer view buffer)
  (set! (buffer-view:buffer view) buffer))

(define (scroll-to-point view)
  (let* ((size (view:size view))
         (point-line (cadr (buffer-view:last-point view)))
         (y-min (* (get-line-height-default) (- point-line 2)))
         (y-max (- (* (get-line-height-default) (+ point-line 5)) (cdr size)))
         (scroll-target (view:scroll-target view)))
    (set! (view:scroll-target view)
          (cons (car scroll-target)
                (max y-max (min y-min (cdr scroll-target)))))
    point-line))

(define* (update-point view #:optional (point (point)) (line (line-number-at-pos)) (col (current-column)))
  (set! (buffer-view:last-point view) (list point line col)))

(define-method (view:update (view <buffer-view>) delta)
  (with-buffer (buffer-view:buffer view)
    (when (not (= (point) (car (buffer-view:last-point view))))
      (update-point view)
      (scroll-to-point view)))
  (next-method))

(define (resolve-line-offset elts x)
  (let loop ((elts elts)
             (col 0))
    (match elts
           ((cur next rest ...)
            (match-let
             ((((x0 . _) font word) cur)
              (((x1 . _) _ _) next))
             (if (and (<= x0 x)
                      (< x x1))
                 (+ col
                    (r:char-offset font word (- x x0)))
                 (loop (cdr elts)
                       (+ col (string-length word))))))
           ((cur)
            (match-let
             ((((x0 . _) font word) cur))
                 (+ col
                    (r:char-offset font word (- x x0)))))
           (else col))))

(define (resolve-screen-position view x y)
  (match-let
   (((rel-line . col)
     (let loop ((matrix (buffer-view:display-matrix view))
                (rel-line 0))
       (if (null? matrix)
           (cons rel-line 0)
           (let* ((row (car matrix))
                  (line-y (car row))
                  (elts (cdr row)))
             (if (>= line-y y)
                 (cons rel-line (resolve-line-offset elts x))
                 (loop (cdr matrix)
                       (1+ rel-line))))))))
   (cons (+ (cdr (buffer-view:last-visible-point-min view)) rel-line)
         col)))

(define (move-point-to view x y)
  (match-let (((_ last-line last-col) (buffer-view:last-point view))
              ((line . col) (resolve-screen-position view x y)))
             (when (or (not (= last-line line))
                       (not (= last-col col)))
               (goto-line line)
               (move-beginning-of-line)
               (forward-char (min col (1- (line-length))))
               (update-point view (point) line (current-column)))
             (cons line col)))

(define-method (view:mouse-position-callback (view <buffer-view>) x y)
  (when (and (buffer-view:mouse-selecting? view)
             (not (buffer-view:suppressed-cursor-move? view)))
    (with-buffer (buffer-view:buffer view)
                 (let ((old-point (point)))
                   (move-point-to view x y)
                   (unless (eq? (point) old-point)
                     (set! (buffer-view:mouse-selecting? view) 'moved))

                   ;; Need some rate limiting because the whole goto-line thing is too damn slow.
                   (set! (buffer-view:suppressed-cursor-move? view) #t)
                   (agenda-schedule
                     (lambda ()
                       (set! (buffer-view:suppressed-cursor-move? view) #f))
                       (seconds->ticks 0.05 1))))))

(define-method (view:mouse-press-callback (view <buffer-view>) button x y)
  (case button
    ((left)
     (with-buffer (buffer-view:buffer view)
                  (move-point-to view x y)
                  (set-mark (point))
                  (set! (buffer-view:mouse-selecting? view) 'clicked)))))

(define-method (view:mouse-release-callback (view <buffer-view>) button)
  (next-method)
  (unless (eq? (buffer-view:mouse-selecting? view) 'moved)
    (set-mark #f))
  (set! (buffer-view:mouse-selecting? view) #f))
