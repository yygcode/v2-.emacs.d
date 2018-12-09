;;; y-common.el --- Common Elisp Routines

;; Copyright (C) 2018 yanyg<yygcode@gmail.com>

;; Author: yanyg<yygcode@gmail.com>
;; Maintainer: yanyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Customization Configuration
;; URL: https://ycode.org; http://ycode.org

;;; Commentary:

;; Common Routines

;;; Code:

;; add-to-list-multi

(defun y/add-to-list-multi(list-var element &rest others-element)
  "Prepend multiple elements ELEMENT and OTHERS-ELEMENT to list LIST-VAR."
  (interactive)
  (dolist (e (reverse others-element))
    (add-to-list list-var e))
  (add-to-list list-var element)
  (symbol-value list-var))
(defun y/add-to-list-multi-append(list-var element &rest others-element)
  "Append multiple elements ELEMENT and OTHERS-ELEMENT to list LIST-VAR."
  (interactive)
  (add-to-list list-var element t)
  (dolist (e others-element)
    (add-to-list list-var e t))
  (symbol-value list-var))

(defun y/match-string-prefix-list(string list-var)
  "Return first element in string LIST-VAR that matche STRING prefix."
  (let (result)
    (dolist (e list-var result)
      (when (string-match "yasnippet-" e)
        (setq result e)))
    result))

;; quelpa self update
(defun y/upgrade-quelpa()
  "Upgrade quelpa package."
  (interactive)
  (unless (package-installed-p 'quelpa)
    (user-error "Package `quelpa' does not installed"))
  (require 'quelpa)
  (quelpa-self-upgrade)
  (quelpa-upgrade)
  (quelpa-checkout-melpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(defun y/date(&optional insert)
  "Show Today Date in echo area.  Insert to current buffer if INSERT."
  (interactive "P")
  (message (format-time-string "%Y-%m-%d"))
  (and insert
       (if buffer-read-only
           (error "Could not insert to read-only buffer")
         (insert (format-time-string "%Y-%m-%d")))))

(provide 'y-common)

;;; y-common.el ends here
