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
;;; <windows:Module>=

;;; @section Windows (Optional)
;;;
;;; Emacsy aims to offer the minimal amount of intrusion to acquire big
;;; gains in program functionality.  Windows is an optional module for
;;; Emacsy.  If you want to offer windows that behave like Emacs windows,
;;; you can, but you aren't required to.

(define-module (emacsy windows)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  ;;; <windows:Include Modules>=
    #:use-module (ice-9 match)
  ;;; <windows:Include Modules>=
  #:use-module (emacsy vector-math)
  #:export ( ;;; <windows:Exported Symbols>=
             <window> <internal-window> <pixel-window>
             ;;; <windows:Exported Symbols>=
             window?
             ;;; <windows:Exported Symbols>=
              split-window )
  #:export-syntax (  )
)
;;; @section Classes
;;;
;;; The window class contains a renderable window that is associated with
;;; a buffer.
;;;
;;;
;;; <windows:Classes>=
(define-class <window> ()
  (window-buffer #:accessor window-buffer #:init-value #f)
  (window-parent #:accessor window-parent #:init-value #f)
  (window-dedicated? #:accessor window-dedicated? #:init-value #f)
  (user-data #:accessor user-data #:init-value #f)
  (to-parent-transform #:accessor to-parent-transform #:init-value (make-identity-matrix 3))
  (from-parent-transform #:accessor from-parent-transform #:init-value (make-identity-matrix 3))
  )
;;; The internal window class contains other windows.
;;;
;;;
;;; <windows:Classes>=
(define-class <internal-window> ()
  (window-children #:accessor window-children #:init-keyword #:window-children #:init-value '()) ; just two!
  (window-parent #:accessor window-parent #:init-value #f)
  (orientation #:accessor orientation #:init-keyword #:orientation #:init-value 'vertical) ; or 'horizontal
  (size #:accessor size #:init-keyword #:size #:init-value .5)
  )
;;; <windows:Classes>=
(define-class <pixel-window> (<internal-window>)
  (window-child #:accessor window-child #:init-value #f)
  (pixel-size #:accessor pixel-size #:init-keyword #:pixel-size #:init-value '(640 480)))
;;; <windows:State>=
(define root-window (make <window>))
;;; The best way I can think to tile and scale all these windows is like
;;; this.  Let's use a normalized bounded coordinates for the internal
;;; windows.  This way the frame size can change and the pixel edges can
;;; be recomputed.
;;;
;;; @figure
;;;   \centering
;;;   \includegraphics[scale=0.75]{window-diagram.pdf}
;;;   \caption[Window Diagram]{\label{window-diagram}Window $A$ can be
;;;     fully described by two vectors: its origin $\bv o_a = (ox, oy)$
;;;     and its end $\bv e_a = (w_a, h_a)$.}
;;; @end figure
;;;
;;;
;;; Imagine the frame has a width $W$ and a height H.  My root window has
;;; the bounded coordinates \verb|(0 0 1 1)|.  When I call
;;; \verb|window-pixel-coords| on it, it will return \verb|(0 0 W H)|.
;;;
;;; Consider the case where my root window is split vertically in half.
;;; My root window would be an internal window with the same bounded
;;; coordinates as before.  The top child, however, will have its pixel
;;; bounded coordinates as \verb|(0 (/ H 2) W (/ H 2)|. And the bottom
;;; child will have \verb|(0 0 W (/ H 2))|.
;;;
;;; One way to think of this is every \verb|<window>| takes up all its
;;; space; intrinsically, they are all set to \verb|(0 0 1 1)|.  The trick
;;; is each \verb|<internal-window>| divides up the space recursively.  So
;;; the internal window in the preceding example that was split
;;; vertically, it passes \verb|0 .5 1 .5| to the top child and
;;; \verb|0 0 1 .5|.
;;;
;;; One thing we need is the absolute pixel bounded coordinates of the
;;; frame.  Emacsy integrators need to set and update this appropriately.
;;;
;;;
;;; <windows:State>=
(define emacsy-root-frame-absolute-pixel-bcoords '(0 0 640 320))
;;; <windows:State>=
(define current-window #f)
;;; <windows:Procedures>=
(define-method (initialize (obj <pixel-window>) initargs)
  (next-method)
  (let ((child-window (make <window>)))
    (set! (window-parent child-window) obj)
    (set! (window-child obj) child-window)
  )
)
;;; @section Procedures
;;;
;;;
;;; <windows:Procedures>=
(define (window? o)
  (or (is-a? o <window>) (is-a? o <internal-window>) (is-a? o <pixel-window>))
  )
;;; <windows:Procedures>=
(define (window-live? o)
  (is-a? o <window>))
;;; <windows:Procedures>=
(define (frame-root-window)
  root-window)
;;; Emacs uses the edges of windows \verb|(left top right bottom)|, but
;;; I'm more comfortable using bounded coordinate systems
;;; \verb|(left bottom width height)|.  So let's write some converters.
;;;
;;;
;;; <windows:Procedures>=
(define (edges->bcoords edges)
  (match edges
   ((left top right bottom)
    (list left bottom (- right left) (- top bottom)))))
;;; <windows:Procedures>=
(define (bcoords->edges coords)
  (match coords
  ((x y w h)
    (list x (+ y h) (+ x w) y))))
;;; @section Units
;;;
;;; There are two different units: pixel (\unit{px}) and proportion
;;; (unitless but denoted \unit{pr} for explicitness).  The size of a
;;; pixel is the same for all windows reference frames.  It is an absolute
;;; measure.  However, the size of a proportion is relative to the window
;;; it is in.  The end of every window $\bv e$ is $(1,1)$ \unit{pr}, or
;;; equivalently $(w, h)$ \px, and the origin of every window is $(0, 0)$
;;; in pixels or proportions.
;;;
;;; When the root window, or frame in Emacs parlance, is resized, we want
;;; each windows by default to resize proportionately.  The windows will
;;; be tiled; therefore, it seems appropriate to use the unit of
;;; proportions as our representation over pixels. There will be some
;;; windows that will have a size of a particular pixel size, like the
;;; minibuffer window.  A little bit of specialization to maintain a
;;; particular pixel height will require some callbacks or hooks.
;;;
;;; @section Converting Between Window Reference Frames
;;;
;;; @figure
;;;   \centering
;;;   \includegraphics[scale=0.75]{child-window-diagram.pdf}
;;;   \caption[Child Window Diagram]{\label{child-window-diagram}This diagram shows
;;;     two windows $A$ and $B$.  Window $B$ can be said to be a child of
;;;     window $A$. Each window has its origin $\bv o$ and end $\bv e$
;;;     with its width $w$ and height $h$.  The point $(x,y)$ may be
;;;     referenced by either coordinate system denoted as follows.
;;;     $(x,y)_A$ denotes the coordinates with respect to the $A$
;;;     reference frame; $(x,y)_B$ denotes the coordinates with respect to
;;;     the $B$ reference frame. }
;;; @end figure
;;;
;;; Figure~@pxref{child-window-diagram} shows a diagram of two windows: one
;;; parent and one child.  The valid range of each variable is given
;;; below.
;;;
;;; @align
;;;   \bv o_b &= (ox, oy) \\
;;;   ox &\in [0, 1]  \\
;;;   oy &\in [0, 1] \\
;;;   w_b &\in [0, 1 - ox]  \\
;;;   h_b &\in [0, 1 - oy]
;;; @end align
;;;
;;; Let's assume that we know the coordinates of the point with respect to
;;; RF $B$ which we'll denote as $(x,y)_B$.  How do we determine the
;;; coordinate wrt RF $A$, $(x',y')_A$?  First, let's define some matrix
;;; operators: translate $\M T(tx, ty)$ and scale $\M S(sw, sh)$.
;;;
;;; @align
;;;   \M T(tx,ty) &= @bmatrix
;;;     1 & 0 & tx \\
;;;     0 & 1 & ty \\
;;;     0 & 0 & 1
;;;   @end bmatrix \\
;;;   \M S(sw,sh) &= @bmatrix
;;;     sw & 0 & 0 \\
;;;     0 & sh & 0\\
;;;     0 & 0 & 1
;;;   @end
;;; @end align
;;;
;;; Now we can determine $(x', y')_A$.
;;;
;;; @align
;;;   %(x',y')_A \px &= \M T(\bv o_b) \, (x,y)_B \, \px  \\
;;;   %(x',y')_A \px &= \M S(\bv e_b) \, (x,y)_A \, \pr  \\
;;;   (x',y')_A &= \M T(\bv o_b) \, \M S(\bv e_b) \, (x,y)_B  \\
;;;   \where \bv e_b &= (w_b, h_b)
;;; @end align
;;;
;;; Note that the multiplication between a $3 \times 3$ matrix and a two
;;; dimensional vector actually is shorthand for this $$\M M ~ (x,y)
;;; \equiv \M M~@bmatrix x \\ y \\ 1 @end bmatrix\text{.}$$ This
;;; is a homogenous coordinate system that allows us to capture affine
;;; transformations like translation.
;;;
;;; Let's denote the transformation to RF $A$ from RF $B$ as ${}_A\M M_B$.
;;;
;;; @align
;;;   (x', y')_A &= {}_A\M M_B \, (x, y)_B \\
;;;   \where{}_A\M M_B &= \M T(\bv o_b) \, \M S(\bv e_b)
;;; @end align
;;;
;;; \noindent And its inverse defines the opposite operation going to RF $B$
;;; from RF $A$.
;;;
;;; @align
;;;   _A\M M_B^{-1} &= (\M T(\bv o_b) \, \M S(\bv e_b))^{-1} \\
;;;   _A\M M_B^{-1} &= \M S(\bv e_b)^{-1} \, \M T(\bv o_b)^{-1}  \\
;;;   _A\M M_B^{-1} &=  {}_B\M M_A
;;; @end align
;;;
;;; @subsection Identities
;;;
;;; Here are a few identities.
;;;
;;; @align
;;;   [\bv o_b]_B &= (0,0) \\
;;;   [\bv e_b]_B &= (1,1) \\
;;;   [\bv o_b]_A &= {}_A\M M_B \, (0, 0) \\
;;;   [\bv e_b]_A &= {}_A\M M_B \, (1, 1)
;;; @end align
;;;
;;; Again but in code this time.
;;;
;;;
;;; <windows:Procedures>=
(define (translate-2d tx ty)
 (vector
  (vector 1 0 tx)
  (vector 0 1 ty)
  (vector 0 0 1)))
;;; <windows:Procedures>=
(define (scale-2d sx sy)
 (vector
  (vector sx 0  0)
  (vector 0  sy 0)
  (vector 0  0  1)))
;;; <windows:Procedures>=
(define (selected-window)
  current-window)
;;; If the internal window size is changed, we want to update the sizes of
;;; its children.  Also, normally we'd only need to keep one matrix and
;;; just invert it as necessary; however, I haven't written a matrix
;;; solver routine, so I'm just going to construct the matrix and its
;;; inverse.  (I wish guile-num were around.)
;;;
;;;
;;; <windows:Procedures>=
(define-method (update-window (window <internal-window>))
 (let ((children (window-children window)))
  (if (eq? (orientation window) 'vertical)
    ;;; <windows:Update vertical window.>=
    (let ((top-size (size window))
          (bottom-size (- 1 (size window))))
     (let ((top (car children))
           (to-parent (matrix. (translate-2d 0. top-size) (scale-2d 1. top-size)))
           (from-parent (matrix. (scale-2d 1. (/ 1 top-size)) (translate-2d 0. (- top-size)))))
      (set! (to-parent-transform top) to-parent)
      (set! (from-parent-transform top) from-parent))
     (let ((bottom (cdr children))
           (to-parent (matrix. (translate-2d 0. 0.) (scale-2d 1. bottom-size)))
           (from-parent (matrix. (scale-2d 1. (/ 1 bottom-size)) (translate-2d 0. 0.))))
      (set! (to-parent-transform bottom) to-parent)
      (set! (from-parent-transform bottom) from-parent)))
    ;;; <windows:Update horizontal window.>=
    (let ((left-size (size window))
            (right-size (- 1 (size window))))
      (let ((left (car children))
            (to-parent (matrix. (translate-2d 0. 0.) (scale-2d left-size 1.)))
            (from-parent (matrix. (scale-2d (/ 1 left-size) 1.) (translate-2d 0. 0.))))
       (set! (to-parent-transform left) to-parent)
       (set! (from-parent-transform left) from-parent))
      (let ((right (cdr children))
            (to-parent (matrix. (translate-2d left-size 0.) (scale-2d right-size 1.)))
            (from-parent (matrix. (scale-2d (/ 1 right-size) 1.) (translate-2d (- left-size) 0.))))
       (set! (to-parent-transform right) to-parent)
       (set! (from-parent-transform right) from-parent))))))
;;; @subsection Window Project
;;;
;;; Let's project a point in the current window to the point in its
;;; ultimate parent window.
;;;
;;;
;;; <windows:Procedures>=
(define-method (window-project (window <window>) position)
  (let ((parent-position (matrix. (to-parent-transform window) position)))
    (if (window-parent window)
      (window-project (window-parent window) parent-position)
      parent-position)))
;;; For internal-windows, we just pass the information through.
;;;
;;;
;;; <windows:Procedures>=
(define-method (window-project (window <internal-window>) position)
  (if (window-parent window)
    (window-project (window-parent window) position)
    position))
;;; <windows:Procedures>=
(define-method (window-project (window <pixel-window>) position)
  (let ((psize (pixel-size window)))
    (matrix. (scale-2d (car psize) (cadr psize)) position)))
;;; @subsection Window Unproject
;;;
;;; Let's unproject from a point in the ultimate parent window to a point
;;; in the given window.  Note that if the point is not within the bounds
;;; of the given window, the resulting point will not be within $[0, 1]^2$.
;;;
;;;
;;; <windows:Procedures>=
(define-method (window-unproject (window <window>) position)
  (let ((parent-position (window-unproject (window-parent window) position)))
    (if (window-parent window)
     (matrix. (from-parent-transform window) parent-position)
      parent-position)))
;;; For internal-windows, we just pass the information through.
;;;
;;;
;;; <windows:Procedures>=
(define-method (window-unproject (window <internal-window>) position)
  (if (window-parent window)
    (window-unproject (window-parent window) position)
    position))
;;; <windows:Procedures>=
(define-method (window-unproject (window <pixel-window>) position)
  (let ((psize (pixel-size window)))
  (matrix. (scale-2d (/ 1 (car psize)) (/ 1 (cadr psize))) position)))
;;; <windows:Procedures>=
(define (window-pixel-bcoords window)
  (let ((origin (window-project window #(0 0 1)))
        (end    (window-project window #(1 1 0))))
    (list
     (vector-ref origin 0)
     (vector-ref origin 1)
     (vector-ref end 0)
     (vector-ref end 1))))
;;; @subsection Window List
;;;
;;;
;;; <windows:Procedures>=
(define-method (window-tree (w <internal-window>))
  (let ((cs (window-children w)))
    (list (window-tree (car cs))
          (window-tree (cdr cs)))))

(define-method (window-tree (w <window>))
  w)

(define-method (window-tree (w <pixel-window>))
  (window-tree (window-child w)))
;;; <windows:Procedures>=

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define* (window-list #:optional (w root-window))
  (flatten (window-tree w)))
;;; @subsection Split Window
;;;
;;; Be careful with \verb|deep-clone|. If you deep clone one window that
;;; has references to other windows, you will clone entire object graph.
;;;
;;;
;;; <windows:Commands>=
(define-interactive (split-window #:optional
                     (window (selected-window))
                     (size 0.5)
                     (side 'below))
  (let* ((original-parent (window-parent window))
         (new-child (shallow-clone window))
         (internal-window (make <internal-window>
                                #:window-children (cons window new-child)
                                #:size size
                                #:orientation (if (memq side '(below above))
                                                  'vertical
                                                  'horizontal))))
    (set! (window-parent internal-window) original-parent)
    (set! (window-parent window)    internal-window)
    (set! (window-parent new-child) internal-window)
    (update-window internal-window)
  internal-window))
;;; @section Window Commands
;;;
;;;
;;; <windows:Commands>=
(define-interactive (split-window-below #:optional (size .5))
  (split-window (selected-window) size 'below))
;;; @section Window Key Bindings
;;;
;;; It will come as no surprise that these key bindings will mimic the
;;; behavior of Emacs.
;;;
;;;
;;; <windows:Key bindings>=
(define-key global-map (kbd "C-x 2") 'split-window-below)
