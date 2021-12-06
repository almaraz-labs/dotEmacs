;;; roam-conf.el --- Emacs Org Roam Configuration  -*- lexical-binding: t; -*-

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


;;; Initialize Org Roam
(use-package org-roam
  ;;:demand t ; This is significantly slowing my startup performance, but is needed for hacks!
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory def/org-roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                 "#+title: ${title}\n#+category: ${title}\n#+filetags: Default")
      :unnarrowed t)
     ("p" "project" plain
      ;;"* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      (file "~/.emacs.d/OrgFiles/Roam/Templates/ProjectTemplate.org")
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                 "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ;; This template is still under development
     ("s" "sample requests" table-line
      ;; This org file template give a "bad template" error after capture is complete...
      ;;(file "~/.emacs.d/OrgFiles/Roam/Templates/SampleRequests.org")
      "| %U | %^{PDR#} | %^{Trade Name} | %^{Supplier} | %^{Notes} | %? |"
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                 "#+title: ${title}\n#+category: ${title}\n#+filetags: Sample Requests")
      :unnarrowed t
      :kill-buffer t)
     ))
  (org-roam-dailies-directory "Journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %<%I:%M %p>: %?"
      :if-new
      (file+head "%<%Y-%m-%d-%a>.org" "#+title: %<%Y-%m-%d-%a>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ;;("C-c n I" . org-roam-node-insert-immediate)
         ;; ("C-c n p" . my/org-roam-find-project)
         ;; ("C-c n b" . my/org-roam-capture-inbox)
         ;; ("C-c n t" . my/org-roam-capture-task)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  ;; Key binding for sample request entries
  ;;("C-c s r" . (lambda () (interactive) (org-capture nil "s"))) 
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-setup) ;; Not exactly sure what this does or why I initially used this...
  (org-roam-db-autosync-mode))


;;; Hacks


;;; Fast note insertion for streamlined notes

;; Bind this to C-c n I
;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   (interactive "P")
;;   (let ((args (cons arg args))
;;         (org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;                                                   '(:immediate-finish t)
;;                                                   ;;'(:kill-buffer t)
;;                                                   ))))

;;     (apply #'org-roam-node-insert args)))


;;; Build your own Org agenda from Org Roam notes

;; The buffer you put this code in must have lexical-binding set to t!
;; See the final configuration at the end for more details.
;; (defun my/org-roam-filter-by-tag (tag-name)
;;   (lambda (node)
;;     (member tag-name (org-roam-node-tags node))))

;; (defun my/org-roam-list-notes-by-tag (tag-name)
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (my/org-roam-filter-by-tag tag-name)
;;            (org-roam-node-list))))

;; (defun my/org-roam-refresh-agenda-list ()
;;   (interactive)
;;   (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
;; (my/org-roam-refresh-agenda-list)


;;; Selecting from a list of notes with a speciic tag

;; (defun my/org-roam-project-finalize-hook ()
;;   "Adds the captured project file to `org-agenda-files' if the
;;   capture was not aborted."
;;   ;; Remove the hook since it was added temporarily
;;   (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Add project file to the agenda list if the capture was confirmed
;;   (unless org-note-abort
;;     (with-current-buffer (org-capture-get :buffer)
;;       (add-to-list 'org-agenda-files (buffer-file-name)))))

;; (defun my/org-roam-find-project ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;; Select a project file to open, creating it if necessary
;; (org-roam-node-find
;;  nil
;;  nil
;;  (my/org-roam-filter-by-tag "Project")
;;  :templates
;;  '(("p" "project" plain
;;     "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
;;     ;;(file "~/.emacs.d/OrgFiles/Roam/Templates/ProjectTemplate.org")
;;     :if-new 
;;     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
;;                "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
;;     :unnarrowed t))))

;;; Stremlined custom capture for tasks & notes

;; Keep an Inbox of notes & tasks
;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates 
;;                      '(("i" "inbox" plain 
;;                         "* %?"
;;                         :if-new 
;;                         (file+head "Inbox.org" "#+title: Inbox")))))

;; Capture a task directly into a specific project
;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates 
;;                      '(("i" "inbox" plain 
;;                         "* %?"
;;                         :if-new 
;;                         (file+head "Inbox.org" "#+title: Inbox")))))





;;; Footer


;;; roam-conf.el ends here
