;;; ~/.emacs.d/lisp/y-init-company.el --- Company Config

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

;; Only company-elisp enough for lisp
(defun y/company-elisp-mode()
  "Set elisp company backends to company-elisp."
  (make-local-variable 'company-backends)
  (setq company-backends '(company-elisp)))

(use-package company
  :diminish
  :init
  (setf company-backends '()) ;; clear first
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-ispell-available t)
  :hook
  (after-init . global-company-mode) ;; auto enable
  (emacs-lisp-mode . y/company-elisp-mode)
  (lisp-interaction-mode . y/company-elisp-mode)
  (lisp-mode . y/company-elisp-mode)
  :bind
  (:map company-active-map
        ("[(tab)]" . 'company-select-next)
        ("C-n"     . 'company-select-next)
        ("C-p"     . 'company-select-previous)
        ("C-c h"   . 'company-quickhelp-manual-begin)))

(use-package company-quickhelp)

(provide 'y-init-company)

;;; y-init-company.el ends here
