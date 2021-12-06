;;; file-system-conf.el --- Emacs File System Configuration  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/dotEmacs

;;; Commentary

;; Key Bindings:

;; Navigation
;; *Emacs* / *Evil*
;; =n= / =j= - next line
;; =p= / =k= - previous line
;; =j= / =J= - jump to file in buffer
;; =RET= - select file or directory
;; =^= - go to parent directory
;; =S-RET= / =g O= - Open file in "other" window
;; =M-RET= - Show file in other window without focusing (previewing files)
;; =g o= (=dired-view-file=) - Open file but in a "preview" mode, close with =q=
;; =g= / =g r= Refresh the buffer with =revert-buffer= after changing configuration (and after filesystem changes!)

;; Marking Files
;; =m= - Marks a file
;; =u= - Unmarks a file
;; =U= - Unmarks all files in buffer
;; =* t= / =t= - Inverts marked files in buffer
;; =% m= - Mark files in buffer using regular expression
;; =*= - Lots of other auto-marking functions
;; =k= / =K= - "Kill" marked items (refresh buffer with =g= / =g r= to get them back)
;; Many operations can be done on a single file if there are no active marks!

;; Copying and Renaming Files
;; =C= - Copy marked files (or if no files are marked, the current file)
;; Copying single and multiple files
;; =U= - Unmark all files in buffer
;; =R= - Rename marked files, renaming multiple is a move!
;; =% R= - Rename based on regular expression: =^test= , =old-\&=
;; *Power command*: =C-x C-q= (=dired-toggle-read-only=)
;;  - Makes all file names in the buffer editable directly to rename them!
;;    Press =Z Z= to confirm renaming or =Z Q= to abort.

;; Deleting Files
;; =D= - Delete marked file
;; =d= - Mark file for deletion
;; =x= - Execute deletion for marks
;; =delete-by-moving-to-trash= - Move to trash instead of deleting permanently

;; Creating and Extracting Archives
;; =Z= - Compress or uncompress a file or folder to (=.tar.gz=)
;; =c= - Compress selection to a specific file
;; =dired-compress-files-alist= - Bind compression commands to file extension

;; Other Common Operations
;; =T= - Touch (change timestamp)
;; =M= - Change file mode
;; =O= - Change file owner
;; =G= - Change file group
;; =S= - Create a symbolic link to this file
;; =L= - Load an Emacs Lisp file into Emacs

;;; References:

;;   [1] 
;;   [2] 
;;   [3] 

;;<=================================================================================================>


;;; Code:


;;; Direc Configuration
;; use `use-package' to configure `dired-jump' and bind to "C-x C-j".
;; Also, add helper packages with this configuration.
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;;; Eshell Configuration
;; *Useful key bindings:*

;; - =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts (also =[[= and =]]= with evil-mode)
;; - =M-p= / =M-n= - go back and forward in the input history
;; - =C-c C-u= - delete the current input string backwards up to the cursor
;; - =counsel-esh-history= - A searchable history of commands typed into Eshell

;; for more thoughts on Eshell, check out these articles by Pierre Neidhardt:
;; - https://ambrevar.xyz/emacs-eshell/index.html
;; - https://ambrevar.xyz/emacs-eshell-versus-shell/index.html
(defun def/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . def/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;;; File Encryption

;;; AES Encryption
;; see aes.el  
(use-package aes
  :init
  (require 'aes)
  :config
  (aes-enable-auto-decryption))

;;; EasyPG Native Encryption
;; Disabled for now, experimenting with AES
;; (use-package epa-file
;;   :straight nil
;;   :config
;;   (require 'epa-file)
;;   (epa-file-enable)
;;   ;;(setq epg-gpg-program "path-exe") ; Try this next, I tried the one's below (didn't work)
;;   ;;(setq agent-info 1)
;;   ;;(setenv "GPG_AGENT_INFO" "1")
;;   ;; configuration settings
;;   )

;; (use-package org-crypt
;;   :straight nil
;;   ;; configuration settings
;;   )




;;; Footer


;;; file-system-conf.el ends here
