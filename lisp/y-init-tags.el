;;; ~/.emacs.d/lisp/y-init-tags.el --- Tags jump

;; Copyright (C) 2017-2019 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING, if not see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Company config for different major-mode.

;;; Code:

;;; C/C++

(require 'xref)
(define-key emacs-lisp-mode-map (kbd "C-c x .") #'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "C-c x d") #'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "C-c x r") #'xref-find-references)
(define-key lisp-interaction-mode-map (kbd "C-c x .") #'xref-find-definitions)
(define-key lisp-interaction-mode-map (kbd "C-c x d") #'xref-find-definitions)
(define-key lisp-interaction-mode-map (kbd "C-c x r") #'xref-find-references)

(use-package ggtags
  :diminish
  :bind
  (:map ggtags-mode-map
        ("C-c g s" . ggtags-find-other-symbol)
        ("C-c g ." . ggtags-find-tag-dwim)
        ("C-c g h" . ggtags-view-tag-history)
        ("C-c g r" . ggtags-find-reference)
        ("C-c g f" . ggtags-find-file)
        ("C-c g C" . ggtags-create-tags)
        ("C-c g c" . ggtags-completion-at-point)
        ("C-c g u" . ggtags-update-tags)))

(use-package counsel-gtags
  :diminish
  :bind
  (:map counsel-gtags-mode-map
        ("C-c g s" . counsel-gtags-find-symbol)
        ("C-c g ." . counsel-gtags-dwim)
        ("C-c g ," . counsel-gtags-pop)
        ("C-c g d" . counsel-gtags-find-definition)
        ("C-c g r" . counsel-gtags-find-reference)
        ("C-c g f" . counsel-gtags-find-file)
        ("C-c g C" . counsel-gtags-create-tags)
        ("C-c g u" . counsel-gtags-update-tags))
  :hook
  (c-mode-common . counsel-gtags-mode))

;; Join gtags-find-symbol and semantic-ia-fast-jump smoothly.
(defun y/tags-jump-symbol(pos)
  "Find tag at current point POS, and use current point if POS nil."
  (interactive "d")
  (or pos (setq pos (point)))
  (or (and (semantic-active-p)
           (semantic-ia-fast-jump pos))
      (and (bound-and-true-p counsel-gtags-mode)
           (counsel-gtags-dwim))
      (xref-find-definitions (xref-backend-identifier-at-point
                              (xref-find-backend)))
      (error "Could not find symbol at current point")))

(global-set-key (kbd "M-.") #'y/tags-jump-symbol)

(provide 'y-init-tags)

;;; y-init-tags.el ends here
