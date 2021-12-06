;;; early-init.el --- Emacs Early Initilization File

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/emacs-config

;;; Commentary

;; Initialization sets up more "cleanly" when we set these fundamental UI
;; elements here in the early-init file [1]. In order to optimize initilization, we dial down
;; Emacs garbage collection [2], thereby directing resources to loading and initializing libraries,
;; this was inspired by David Wilson's Emacs configuration [3]

;;; References:

;;   [1] https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;;   [2] https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
;;   [3] https://config.daviwil.com/emacs

;;<=================================================================================================>


;;; Code:


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)     ; Disable visible scrollbar
(tool-bar-mode -1)       ; Disable the toolbar
(tooltip-mode -1)        ; Disable tooltips
(menu-bar-mode -1)       ; Disable the menu bar
(set-fringe-mode 10)     ; Give some breathing room


;; Set up the visible bell
(setq visible-bell nil
      ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Show column number in modeline and line numbers in buffers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame to fullscreen
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set frame transparency (only accepts integer values)
(set-frame-parameter (selected-frame) 'alpha '(88 . 88))
(add-to-list 'default-frame-alist `(alpha . ,'(88 . 88)))

;; disable package.el -> use instead straight.el (better package management system)
(setq package-enable-at-startup nil)




;;; Footer


;;; early-init.el ends here
