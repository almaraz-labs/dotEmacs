;;; completion-framework-conf.el --- Emacs Completion Framework  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https://github.com/skribest/dotEmacs

;;; Commentary

;; Set up completions with Vertico and completion actions with embark...
;; So far I have this setup fully installed and working very well,
;; this is actually working better than Ivy, Counsel and Which Key
;; - that is, Vertico and EMBARK, also this setup works better with
;; Marginalia, unlike the former setup.

;;; References:

;;   [1] 
;;   [2] 
;;   [3] 

;;<=================================================================================================>


;;; Code:


;;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight nil
  :init
  (setq history-length 25)
  (savehist-mode))

;; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25))

;;; Completions with Vertico
(defun def/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent folder otherwise delete word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

;; Enable vertico
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . def/minibuffer-backward-kill))
  :custom
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  ;; Different scroll margin, default 2
  (vertico-scroll-margin 1)
  ;; Show more/less candidates, default 10
  (vertico-count 12)
  ;; Grow and shrink the Vertico minibuffer - I find this too distracting...
  ;;(vertico-resize t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

;;; Completions in regions with Corfu
;;
;; (use-package corfu
;;   :straight '(corfu :host github
;;                     :repo "minad/corfu")
;;   :bind (:map corfu-map
;;               ("C-j" . corfu-next)
;;               ("C-k" . corfu-previous)
;;               ("C-f" . corfu-insert))
;;   :custom
;;   (corfu-cycle t)
;;   :config
;;   (corfu-global-mode))

;;; Improved Candidtae Filtering with Orderless
;; Enable Orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Completion Actions with EMBARK
;; Adding in some helper packages to work with EMBARK
(use-package embark
  :bind
  (("C-."   . embark-act)
   ("M-."   . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Show Embark action via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-dispatch-always t))

;; (global-set-key (kbd "M-o") 'ace-window)
;; (setq aw-dispatch-always t)

(use-package 0x0
  :bind (:map embark-region-map
              ("U" . 0x0-dwim)))

;; (define-key embark-region-map (kbd "U") '0x0-dwim)

;;; Consult Commands
(use-package consult
  :demand t
  :bind (("C-s"   . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r"   . consult-history))
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  ;;(consult-preview-mode)
  )

;;; Completion Annotations with Marginalia
(use-package marginalia
  :after vertico
  ;; Either bind 'marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;;; Keybinding Panel `which-key'
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;; Help Commands with Helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))




;;; Footer


;;; completion-framework-conf.el ends here
