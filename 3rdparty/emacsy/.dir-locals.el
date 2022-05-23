;;; Emacsy --- An embeddable Emacs-like library using GNU Guile.
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Emacsy.
;;;
;;; Emacsy is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Emacsy is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.

;; The GNU project defaults.  These are also the GNU Emacs defaults.
;; Re-asserting theme here, however, as a courtesy for setups that use
;; a global override.
(
 ;; For writing GNU C code, see
 ;; https://www.gnu.org/prep/standards/html_node/Writing-C.html
 (c-mode . ((c-file-style . "gnu")
            (indent-tabs-mode . nil)))

 (makefile-mode . ((indent-tabs-mode . t)))

 (nil . ((indent-tabs-mode . nil)
         (fill-column . 72)
         (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))

 (scheme-mode
  .
  ((eval . (put 'with-buffer 'scheme-indent-function 1))
   (eval . (put 'save-excursion 'scheme-indent-function 1))
   (eval
    .
    (progn
      (defun prefix-dir-locals-dir (elt)
        (concat (locate-dominating-file buffer-file-name ".dir-locals.el") elt))
      (mapcar
       (lambda (dir) (add-to-list 'geiser-guile-load-path dir))
       (mapcar
        #'prefix-dir-locals-dir
        '("." "test")))))))

 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72))))
