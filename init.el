;;; ~/.emacs.d/init.el --- Emacs Initialization/Customization File

;; Copyright (C) 2017-2019 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Customization Configuration
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

;; Emacs base customization, includes package-tool, proxy and org-babel
;; then use config.org to deep customize.
;;
;; Enviroment Variable Description
;; 1. package mirror: I use tsinghua mirror package-archive, If you are not
;;    live in China, you can set follow environment to disable it:
;;    (A) shell: ~$ export export EMACS_Y_PACKAGE_NO_MIRROR=1
;;    (B) or set env by LISP at the top of this file:
;;        (setenv "EMACS_Y_PACKAGE_NO_MIRROR" "1")
;;    If you want to use proxy, then set environment http_proxy/https_proxy
;;    to disable mirror and turn back to melpa archive.
;;
;;    Of course you can force to use tsinghua mirror even a proxy working.
;;    The env name is EMACS_Y_PACKAGE_FORCE_MIRROR, which override proxy
;;    and EMACS_Y_PACKAGE_NO_MIRROR.

;;; Code:

(setq debug-on-error nil)

(unless (version= emacs-version "26.1")
  (warn "NOTICE: The config just test for version 26.1"))

;; garbage collection change to 32MB for performance optimize
(setq gc-cons-threshold (* 32 1024 1024))

;; Define as early as possible.
(defvar user-init-config (expand-file-name "config.org" user-emacs-directory)
  "File name, including directory, of initialization file by org-babel.")

;;; My Helper/Common routine
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Proxy environment process
(defvar y/no-proxy-list (mapconcat 'identity
                         '("*.cn"
                          "*.jd.com"
                          "*.baidu.com"
                          "*.bing.*"
                          "*.csdn.*"
                          ;; Please add new site
                          ) ",")
  "Sites list for no-proxy, only used for proxy scene.")
(defun y/env-set(env val &optional force)
  "Set enviroment ENV to VAL if ENV unset or FORCE is t."
  (when (or (not (getenv env)) force)
    (setenv env val)))
(defun y/env-sync-partner(env1 env2)
  "Sync enviroment ENV1/ENV2 to another if one is nil and another is non-nil."
  (or (getenv env1) (y/env-set env1 env2))
  (or (getenv env2) (y/env-set env2 env1)))
(y/env-sync-partner "http_proxy" "https_proxy")
;; set no_proxy env if empty
(y/env-set "no_proxy" y/no-proxy-list)

;; set to utf-8-unix, otherwise pakcage install blames with
;; 'Selecting Coding System ...'
(prefer-coding-system 'utf-8-unix)

;; package - Simple package system for Emacs. Built-in
(require 'package)

;; Archive site
;; Emacs archives-site is isolated by GFW, use mirror site if no proxy
(defun y/set-package-archives-official()
  "Set package-archives to melpa.org if https-proxy enabled or FORCE non-nil."
  (interactive)
  (message "Set package-archives to melpa.org(official).")
  (setq package-archives
        '(("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("marmalade" . "https://marmalade-repo.org/packages/"))))

(defun y/set-package-archives-mirror()
  "Set package-archives to tsinghua if https-proxy disabled or FORCE non-nil."
  (interactive)
  (message "Set package-archives to tsinghua mirror.")
  (setq package-archives
        '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("melpa-stable" .
           "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))))

(if (or (getenv "EMACS_Y_PACKAGE_NO_MIRROR") (getenv "https_proxy"))
    (y/set-package-archives-official)
  (y/set-package-archives-mirror))

;; FORCE_MIRROR env overrides NO_MIRROR or proxy
(when (getenv "EMACS_Y_PACKAGE_FORCE_MIRROR")
  (y/set-package-archives-mirror)
  ;; Also remove proxy config.
  (dolist (e '("http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY"))
          (setenv e nil)))

;;; package manage and org babel config

(package-initialize)

;; Only refresh once for slowing.
;; FIXME: how to upgrade all use-package by one command ?
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set custom-file variable to prevent emacs mess up init.el.
;; But we don't need to load it.
(setq custom-file (expand-file-name ".custom-auto.el" user-emacs-directory))
;; (load custom-file t)

;; use-package simplifies emacs packages install and config.
;; GitHub: https://github.com/jwiegley/use-package
;; HomePage: https://jwiegley.github.io/use-package/
(unless (or (package-installed-p 'use-package)
            (and (package-install 'use-package)
                 ;; If you manaul remove use-package package
                 (normal-top-level-add-subdirs-to-load-path)))
  (error "Install use-package failed"))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(setq use-package-always-defer t)

;; quelpa - Install Emacs Lisp packages from source code.
;; https://github.com/quelpa/quelpa
(use-package quelpa
  :pin melpa
  :init
  ;; disable auto-upgrade for startup performance
  ;; call y/upgrade-quelpa manually if necessary
  (setq quelpa-checkout-melpa-p t)
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-stable-p t))

;; Provide quelpa option to use-package
;; https://github.com/quelpa/quelpa-use-package
(use-package quelpa-use-package
  :pin melpa
  :config
  (require 'quelpa-use-package)
  ;; I set use-package-always-ensure, so need advice here
  ;; https://github.com/quelpa/quelpa-use-package#overriding-use-package-always-ensure
  (quelpa-use-package-activate-advice))

;; Reload init emacs.
;; But reload is different with restart, old config may be left. e.g. key bind.
(global-set-key (kbd "C-c q r")
                #'(lambda()(interactive) (load-file user-init-file)))

;;; Emacs deep config with Org-mode literate programming

;; If you want to use the latest org, use the follows config:
;; 1. Download latest package or clone repo.
;;    URL: http://orgmode.org/
;;    REPO:
;;      ~$ git clone git://orgmode.org/org-mode.git
;;      ~$ make autoloads
;; 2. add load-path
;;    (add-to-list 'load-path "~/path/to/orgdir/lisp")
;; 3. If you want contributed libraries
;;    (add-to-list 'load-path "~/path/to/orgdir/contrib/lisp" t)
;; See homepage http://orgmode.org/ for more details.

;; use-package use 'package-installed-p' to check package installed or not
;; and org is a built-in package, so use-package would ignore org package
;; but org-plus-contrib is not installed default, so I think I can force install
;; org by routine package-install but failed.
(use-package org
    :pin org)
(use-package org-plus-contrib
    :pin org)

;; load literate config.
(when (file-exists-p user-init-config)
  (org-babel-load-file user-init-config))

;;; init.el ends here
