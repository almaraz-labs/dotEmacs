;;; ui-conf.el --- Emacs UI Configuration  -*- lexical-binding: t; -*-

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


;;; Startup Message

;; Disable default Emacs startup message
(setq inhibit-startup-echo-area-message t)

(defun def/emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (let ((str
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))))
    str))

(defun def/display-startup-init-info ()
  "Calculate and display Emacs initialization time, packages loaded, and 
  garbage collections."
  (let ((package-count 0)
        (time (def/emacs-init-time)))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
    (if (zerop package-count)
        (message "Emacs started in %s with %d garbage collection(s)." time gcs-done)
      (message
       "Emacs started in %s, loaded %d package(s) and performed %d garbage collection(s)."
       time package-count gcs-done))))

(add-hook 'emacs-startup-hook #'def/display-startup-init-info)

;;; Keybinding Configuration

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use `general' for its keybinding functionality
(use-package general
  ;;:after evil
  :config
  ;;(general-evil-setup t)
  (general-create-definer def/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(def/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org"))))

;; 
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;;; Color Theme
;; Use doom-themes. You can also run `counsel-load-themes' to choose between them easily.
(use-package doom-themes
    :init (load-theme 'doom-palenight t))

;;; Modeline
;; 
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 27)))


;;; Text Scaling
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(def/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;; Command Log Mode
;; Disabled for now
;; (use-package command-log-mode
;;   :commands command-log-mode)




;;; Footer


;;; ui-conf.el ends here
