(define-module (zem ui buffer-view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module ((zem api font) #:prefix f:)
  #:use-module (zem core buffer)
  #:use-module (zem ui view)
  #:use-module (zem ui root-view)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module (zem ui highlight)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 gap-buffer)
  #:use-module (oop goops)
  #:export (<buffer-view>))

(define-class <buffer-view> (<view>)
  (buffer #:init-keyword #:buffer #:accessor buffer-view:buffer))

(define show-caret? #f)
(define (blink-period)
  (if ticks-per-second
      (* 0.5 ticks-per-second)
      1))
(define (toggle-caret)
  (set! show-caret? (not show-caret?))
  (queue-redraw)
  (agenda-schedule toggle-caret (blink-period)))
(agenda-schedule toggle-caret (blink-period))

(define (get-line-height)
  (floor (* (f:get-font-height style:font) 1.1)))

(define (get-text-y-offset)
  (floor (/ (f:get-font-height style:font) 4.0)))

(define (get-text-x-offset line-max)
  (match-let* (((w . _) (r:text-size-hint style:font (number->string line-max))))
              (+ w (floor (* 1.5 (car style:padding))))))

(define (get-visible-line-range view line-max)
  (match-let* (((x0 y0 x1 y1) (view:get-visible-bbox view))
               (lh (get-line-height))
               (visible-line-min (max 1 (floor (/ y0 lh))))
               (visible-line-max (min line-max (1+ (floor (/ y1 lh))))))
              (cons (inexact->exact visible-line-min) (inexact->exact visible-line-max))))

(define-method (view:get-scroll-limit (view <buffer-view>))
  (with-buffer (buffer-view:buffer view)
    (* (get-line-height) (- (count-lines (point-min) (point-max)) 1))))

(define (collect-line acc)
    (let ((c (char-after)))
      (cond
       ((= (point) (point-max))
        acc)
       ((eqv? c #\newline)
        (forward-char)
        acc)
       (else
        (forward-char)
        (collect-line (string-append acc (string c)))))))

(define (syntax->color syn)
  (case syn
    ((keyword special) style:keyword-color)
    ((symbol) style:text-color)
    ((string) style:string-color)
    (else style:text-color)))

(define (draw-caret pos line-height)
  (when show-caret?
    (r:add-rect pos (cons style:caret-width line-height) style:caret-color)))

(define (map-cdr fn c)
  (cons (car c) (fn (cdr c))))

(define (draw-line-number num x y width)
  (r:add-text style:font
              (cons x
                    (- y (get-text-y-offset)))
              (format #f (string-append "~" (number->string width) "@a") num)
              style:line-number-color))

(define (draw-line lidx x y line line-height point-line point-col)
  (let ((lex (get-syntax-lexer (buffer-name (current-buffer))))
        (draw-caret? (= lidx point-line)))
    (let ((pos (let loop ((tokens (highlight-code line lex))
                          (col 0)
                          (tx x))
                 (if (not (null? tokens))
                     (match-let* (((color . text)
                                   (match (car tokens)
                                          ((syn text) (cons (syntax->color syn) text))
                                          (text (cons style:text-color text))))
                                  (new-col (+ (string-length text) col)))
                                 (if (and draw-caret?
                                          (>= point-col col)
                                          (< point-col new-col))
                                     ;; Caret is inside this token
                                     (match-let* ((split (- point-col col))
                                                  (head (substring text 0 split))
                                                  (tail (substring text split))
                                                  ((hx . _) (if (string-null? head)
                                                                (cons tx y)
                                                                (r:add-text style:font
                                                                            (cons tx
                                                                                  (- y (get-text-y-offset)))
                                                                            head
                                                                            color)))
                                                  ((nx . _) (if (string-null? tail)
                                                                (cons hx y)
                                                                (r:add-text style:font
                                                                            (cons hx
                                                                                  (- y (get-text-y-offset)))
                                                                            tail
                                                                            color))))
                                                 (draw-caret (cons hx (- y line-height))
                                                             line-height)
                                                 (loop (cdr tokens)
                                                       new-col
                                                       nx))
                                     ;; Normal draw
                                     (loop (cdr tokens)
                                           new-col
                                           (car (r:add-text style:font
                                                            (cons tx
                                                                  (- y (get-text-y-offset)))
                                                            text
                                                            color)))))
                     (cons tx y)))))
      (if (and draw-caret?
               (= point-col (string-length line)))
          ;; Caret is at EOL
          (draw-caret (map-cdr (lambda (y)
                                 (- y line-height)) pos)
                      line-height)))))

(define (draw-mode-line x y width height mode-line)
  (r:add-rect (cons x y)
              (cons width height)
              style:mode-line-background-color)
  (r:add-text style:font
              (cons (+ x (car style:padding))
                    (- (+ y height) (get-text-y-offset)))
              mode-line
              style:text-color))

(define-method (view:draw (view <buffer-view>))
  (draw-view-background view style:background-color)

  (with-buffer (buffer-view:buffer view)
    (let ((point-line (max 1 (line-number-at-pos)))
          (point-col (current-column))
          (mode-line (emacsy-mode-line)))
      (save-excursion
        (match-let* (((view-x . view-y) (view:pos view))
                     ((view-width . view-height) (view:size view))
                     (lh (get-line-height))
                     (text-height (- view-height lh))
                     (line-max (count-lines (point-min) (point-max)))
                     ((visible-line-min . visible-line-max) (get-visible-line-range view line-max))
                     (line-x (+ (car style:padding) view-x))
                     (text-x (+ (get-text-x-offset line-max) view-x))
                     (mode-line-y text-height))
                    ;; Move to the beginnin of the first visible line
                    (goto-line visible-line-min)
                    (move-beginning-of-line)
                    ;; Draw background for line numbers
                    (r:add-rect (cons view-x view-y)
                                (cons text-x text-height)
                                style:line-number-background-color)
                    ;; Iterate each line
                    (let loop ((y lh)
                               (lidx visible-line-min))
                      (if (and (< (point) (point-max))
                               (< y (+ text-height lh))
                               (<= lidx visible-line-max))
                          (let* ((line (collect-line ""))
                                 (line-y (+ view-y y)))
                            (when (= lidx point-line)
                              ;; Highlight current line
                              (r:add-rect (cons text-x (- line-y lh))
                                          (cons view-width lh)
                                          style:highlight-color))
                            (draw-line-number lidx
                                              line-x
                                              line-y
                                              (string-length (number->string line-max)))
                            (draw-line lidx text-x line-y line lh point-line point-col)
                            (loop (+ y lh)
                                  (+ 1 lidx)))))
                    (draw-mode-line view-x mode-line-y view-width lh mode-line))))))

(define-public (switch-buffer-view-buffer view buffer)
  (set! (buffer-view:buffer view) buffer))
