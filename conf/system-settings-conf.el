;;; system-settings-conf.el --- Emacs System Specific Settings  -*- lexical-binding: t; -*-
;; This file is auto generated by Emacs.org

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/emacs-config

;;; Commentary

;;; References:

;;   [1] 
;;   [2] 
;;   [3] 

;;<=================================================================================================>


;;; Code:

;;; System Settings

;; Configure system specific PATHS.
(if (and (string-equal system-type "windows-nt") def/work)
    (let ((mypaths
           `(
             ,(concat def/win-app-path "Emacs/emacs-27.2-x86_64/bin")
             ,(concat def/win-app-path "MinGW/bin")
             ,(concat def/win-app-path "PortableGit/cmd")
             ,(concat def/win-app-path "Perl/perl/bin")
             ,(concat def/win-app-path "Python/python3.9.9")
             ,(concat def/win-app-path "MikTex/texmfs/install/miktex/bin/x64")
             ,(concat def/win-app-path "ImageMagick")
             ,(concat def/win-app-path "GnuPG/bin")
             ,(concat def/win-app-path "Hunspell/bin")
             )))
      (setenv "PATH" (mapconcat 'identity mypaths ";"))
      (setq exec-path (append mypaths (list "." exec-directory))
            def/default-font-size 90
            def/default-variable-font-size 90)))


;; Notes:
;; To add helper functions in Emacs setup
;; (add-hook 'after-init-hook #'ea/my-function)

;;; Footer


;;; system-settings-conf.el ends here
