;;; utils.el --- Emacs Lisp Utilities -  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/dotEmacs

;;; Commentary

;; Rename File and Buffer is inspired from Steve Yegge's .emacs file [1] and
;; I found New Empty Buffer while looking through Xah's very helpful Emacs
;; blog/website [2].

;; References:

;;   [1] https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;   [2] http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html
;;   [3]

;;<=================================================================================================>


;;; Code:

;;; Rename File and Buffer
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let (
        (name (buffer-name))
        (filename (buffer-file-name))
        )

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))
    )
  )

;;; Provide New Empty (Lisp Interaction) Buffer
;;
(defun new-empty-buffer ()
  "Create a new empty buffer. New buffer will be named “untitled” or “untitled<2>”, 
“untitled<3>”, etc. It returns the buffer (Lisp Interaction Mode).

        Version 2021-11-16"
  (interactive)
  (let ((_buf (generate-new-buffer "untitled")))
    (switch-to-buffer _buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    _buf
    ))




;;; Footer


;;; utils.el ends here
