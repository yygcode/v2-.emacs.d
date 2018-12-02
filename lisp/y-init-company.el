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

(require 'cc-mode)

;; default to complete for indent and completion.
;; Input C-q TAB if you need real tab.
(setq tab-always-indent 'complete)
;; c-mode often needs insert tab to align, set to nil default.
(setq c-tab-always-indent nil)

;;; company
;; load here to satisfy flychek
(require 'company nil t)

(defun y/company-elisp-hook()
  "Config company hook for elisp."
  (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  (setq company-backends '(company-elisp)
        company-idle-delay 0))

(defun y/company-c-mode-common()
  "Config company hook for C/C++."
  (setq company-backends '((company-semantic
                            company-c-headers
                            company-keywords
                            company-yasnippet)
                           ;; Outer group for performance
                           company-gtags)
        company-idle-delay 5))

(defun y/company-hook()
  "Comelete anything hook."
  (make-local-variable 'company-backends)
  (make-local-variable 'company-idle-delay)
  (and company-mode ;; do nothing if nil
       (cond ((or (equal major-mode 'emacs-lisp)
                  (equal major-mode 'lisp-interaction-mode))
              (y/company-elisp-hook))
             ((or (equal major-mode 'c-mode)
                  (equal major-mode 'c++-mode))
              (y/company-c-mode-common)))))

(use-package company
  :diminish
  :init
  (setq company-show-numbers t
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10
        company-selection-changed t
        company-selection-wrap-around t
        company-require-match nil)
  :config
  (bind-key [remap completion-at-point] #'company-complete)
  (bind-key [remap complete-symbol] #'company-complete)
  (bind-key [remap complete-tag] #'company-complete)
  (bind-key [remap completion-at-point-functions] #'company-complete)
  ;; Do not bind-key c-indent-line-or-region for default is better.
  ;; (bind-key [remap c-indent-line-or-region]
  ;;           #'company-indent-or-complete-common)
  :hook
  (after-init . global-company-mode)
  (company-mode . y/company-hook)
  ;; I don't know how to config face by company-frontends-set
  :custom-face
  (company-tooltip ((t (:foreground "orange1"))))
  :bind
  (:map c-mode-map
        ("C-<return>"   . company-complete)
        ("C-<tab> s"    . company-semantic)
        ("C-<tab> g"    . company-gtags)
        ("C-c s c"      . company-semantic)
        )
  (:map company-active-map
          ("<tab>"   . company-select-next)
          ("S-<tab>" . company-select-previous)
          ("C-n"     . company-select-next)
          ("C-p"     . company-select-previous)
          ("C-k"     . company-complete-selection)
          ;; company-quickhelp has no map, used map here.
          ("C-h" . company-quickhelp-manual-begin)
          ))

(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay nil
        company-quickhelp-max-lines 30)
  :hook
  (company-mode . company-quickhelp-mode))

;; If you like replace helm-company frontend.
(use-package helm-company)

(use-package company-c-headers
  :pin melpa
  :bind
  ("C-c h c" . helm-company))

(require 'semantic nil t)

(defun y/semantic-elisp-hook()
  "Config company hook for elisp."
  ;; Turn off
  (semantic-mode -1))

(defun y/semantic-c-common()
  "Config company hook for C/C++."
  (setq company-backends '((company-c-headers
                            company-keywords
                            company-yasnippet
                            company-semantic)
                           ;; Outer group for performance
                           company-gtags)
        company-idle-delay 0.5))

(defun y/semantic-hook()
  "Comelete anything hook."
  (make-local-variable 'company-backends)
  (make-local-variable 'company-idle-delay)
  (and company-mode ;; do nothing if nil
       (cond ((or (equal major-mode 'emacs-lisp)
                  (equal major-mode 'lisp-interaction-mode))
              (y/semantic-elisp-hook))
             ((or (equal major-mode 'c-mode)
                  (equal major-mode 'c++-mode))
              (y/company-c-mode-common)))))

;; https://www.gnu.org/software/emacs/manual/html_mono/semantic.html
(use-package semantic
  :config
  (global-semantic-idle-scheduler-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-summary-mode 1)
  (global-semantic-mru-bookmark-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-idle-local-symbol-highlight-mode 1)
  ;; (semanticdb-enable-gnu-global-databases 'c-mode t)
  ;; (semanticdb-enable-gnu-global-databases 'c++-mode t)
  ;; Turn off follow modes default
  ;; (global-semantic-highlight-func-mode 1)
  ;; (global-semantic-highlight-edits-mode 1)
  ;; (global-semantic-idle-completions-mode 1)
  ;; (global-semantic-show-unmatched-syntax-mode 1)
  ;; (global-semantic-decoration-mode 1)
  (setq semantic-idle-scheduler-idle-time 1
        semantic-idle-scheduler-work-idle-time 30
        semantic-displayor-tooltip-initial-max-tags 10
        semantic-displayor-tooltip-mode "verbose"
        ;; semanticdb-implied-include-tags t
        )
  (advice-add 'semantic-ia-fast-jump :before
              #'(lambda(point)
                  "Push marker for jump back."
                  (xref-push-marker-stack)))
  :bind
  ("C-c , m" . semantic-mode) ;; global-map
  ;; see y-init-tags.el
  ;; ("M-."     . semantic-ia-fast-jump)
  (:map semantic-mode-map
        ("C-c , ." . semantic-ia-fast-jump)
        ("C-c , r" . semantic-symref)
        ("C-c , c" . semantic-ia-complete-symbol)
        )
  :hook
  (c-mode-common . (lambda()
                     "C/C++ Semantic Hooks."
                     (semantic-mode 1)
                     (semantic-default-c-setup))))

(provide 'y-init-company)

;;; y-init-company.el ends here
