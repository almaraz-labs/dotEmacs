;;; pm-conf.el --- Emacs Package Management Configuration  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/dotEmacs

;;; Commentary

;;; References:

;;   [1] 
;;   [2] 
;;   [3] 

;;<=================================================================================================>


;;; Code:


;;; Package Management with "straight.el"

;; Initialize `straight' (instead of `package')
;; This requires setting up some code in "early-init.el", see that
;; file for more information.

;; Boostrap `straight' and load helper package `straight-x' for
;; other useful commands.

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load the helper package for commands such as 'straight-x-clean-unused-repos'
(require 'straight-x)

;;; `use-package' Setup with `straight'

;; `use-package' initialization to use `straight.el' rather than its default
;; use of `package.el'
;; setup use-package to use straight.el rather than it's default use of package.el.
(straight-use-package 'use-package)

;; Always enable straight.el with use-package
(setq straight-use-package-by-default t)

;; Set up `no-littering' package to help keep directories where we edit files
;; and the Emacs configuration directory clean! It knows about a wide variety
;; of variables for built in Emacs features as well as those from community
;; packages so it can be much easier than finding and setting these variables
;; yourself.
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name def/user-emacs-cache-directory)
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;;(load custom-file t)
(load custom-file 'noerror 'nomessage)


;;; ARCHIVE
;; `package.el' Configuration
;; Previous package management system using `package.el'
;; Disabled for now, since we are using/experimenting with `straight.el'
;; ;; Initialize package sources
;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)


;; Automatic Package updates
;; Disabled for now with use of `straight.el'
;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "11:00"))




;;; Footer


;;; pm-conf.el ends here
