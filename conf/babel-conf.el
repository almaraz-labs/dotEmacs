;;; babel-conf.el --- Emacs Org Babel Configuration  -*- lexical-binding: t; -*-

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


;;; Initialization
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     ;; Disable for now
     (scheme . nil)
     (python . nil)
     (shell . nil)
     ))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (setq org-confirm-babel-evaluate nil))


;;; Structure Templates
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell")))

;;; Guile Scheme REPL Configuration
;;
(use-package geiser
  :if (not def/work)
  :hook (scheme-mode-hook . geiser-mode)
  :config
  (geiser-default-implementation 'guile)
  (geiser-repl-use-other-window nil))

(use-package geiser-guile
  :if (not def/work)
  :after geiser)

;;; Auto-Tangle Select Files
;; Automatically tangle Literate Programming files when we save them
;;
;; Since I am no longer doing a literate config with org, just setup
;; to auto-tangle research literate programing files
(defun def/org-babel-tangle-literate ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name def/lp-dir))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))
    ))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook
                               #'def/org-babel-tangle-literate)))




;;; Footer


;;; babel-conf.el ends here
