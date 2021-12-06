;;; basic-ui-conf.el --- Emacs Basic UI Configuration  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
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


;;; Set column numbers only for certain modes 
;; See `early-init.el' for other basic ui configuration
;; Disable Line Numbers (per mode)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;; Font Configuration

;; Set the default face
(set-face-attribute 'default nil
                    :font def/default-font-family ;;Fira Code Retina
                    :height def/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font def/default-font-family ;;Fira Code Retina
                    :height def/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font def/default-variable-font-family ;;Iosevka Aile (prev "Canterell")
                    :height def/default-variable-font-size
                    :weight 'regular)




;;; Footer


;;; basic-ui-conf.el ends here
