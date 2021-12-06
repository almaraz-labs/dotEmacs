;;; init.el --- Emacs Initialization File  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/dotEmacs

;;; Commentary

;;; Introduction
;; The Next Generation Emacs Configuration - dotEmacs Framework (DEF)

;; This is brand-spankin-new configuration that I compiled this weekend. I originally setup my
;; Emacs configuration in this manner, the style which was inspired by Steve Yegge [1].
;; My Emacs passion was reignited by David Wilsons System Crafters YouTube channel, where
;; he presented his emacs-from-scratch series [2]. I then went through the exercise of setting
;; up a literal configuration based on Org Babel [3]. I tried this style for a month, however,
;; I found it was quite cumbersome when needing to revert... My Current approach of keeping config
;; files disparate, and letting init load in the needed files.

;;; References:

;;   [1] https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;   [2] https://github.com/daviwil/emacs-from-scratch
;;   [3] https://orgmode.org/worg/org-contrib/babel/intro.html

;;<=================================================================================================>


;;; Code:

(defgroup dotEmacs nil
  "The Next Generation Emacs Configuration."
  :group 'applications
  :version "0.1")

;;; Define Global variables
(defcustom def/default-font-size 110
  "Global variable for font size."
  :type 'integer
  :group 'dotEmacs
  :version "0.1")

(defcustom def/default-variable-font-size 110
  "Global variable for variable font size."
  :type 'integer
  :group 'dotEmacs
  :version "0.1")

(defcustom def/default-font-family "Fira Code Retina"
  "Global variable for font family."
  :type 'string
  :group 'dotEmacs
  :version "0.1")  

(defcustom def/default-variable-font-family "Iosevka Aile"
  "Global variable for variable font family."
  :type 'string
  :group 'dotEmacs
  :version "0.1")

(defcustom def/work t
  "Global boolean variable for Windows work machine."
  :type 'boolean
  :group 'dotEmacs
  :version "0.1")

(defcustom def/home-win-path "C:/Users/erik/AppData/Roaming/"
  "Global variable for Emacs HOME PATH for windows work machine."
  :type 'string
  :group 'dotEmacs
  :version "0.1")

(defcustom def/win-app-path (concat def/home-win-path ".apps/")
  "Global variable for Emacs HOME PATH for windows work machine."
  :type 'string
  :group 'dotEmacs
  :version "0.1")  

(defcustom def/lp-dir "~/.emacs.d/OrgFiles/Babel/"
  "Global variable for Org Bable Literate Programing files directory."
  :type 'string
  :group 'dotEmacs
  :version "0.1")

(defcustom def/user-emacs-directory "~/.emacs.d/"
  "Global variable for Emacs Literal Configuration."
  :type 'string
  :group 'dotEmacs
  :version "0.1")

(defcustom def/user-emacs-cache-directory "~/.cache/emacs/"
  "Global variable for Emacs user cache. Set when used in 
conjunction with `no-littering'."
  :type 'string
  :group 'dotEmacs
  :version "0.1")

(defcustom def/org-agenda-dir "~/.emacs.d/OrgFiles/Notes/"
  "Global variable for Org Agenda files directory."
  :type 'string
  :group 'dotEmacs
  :version "0.1")

(defcustom def/org-roam-dir "~/.emacs.d/OrgFiles/Roam/"
  "Global variable for Org Roam files/database directory."
  :type 'string
  :group 'dotEmacs
  :version "0.1")


;;; Startup Performance

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))


;;; Emacs Configuration

;; Set directories that contain Emacs configuration files and load..
(let (
      ;; Define variables here
      (confs "~/.emacs.d/conf/")
      (elisp "~/.emacs.d/elisp/")

      )
  ;; let body...
  
  ;; Add paths to `load-path'
  (add-to-list 'load-path confs)
  (add-to-list 'load-path elisp)
  
  ;; Load config files here...
  ;; Order matters!
  (load "system-settings-conf"      'noerror 'nomessage)
  (load "basic-ui-conf"             'noerror 'nomessage)
  (load "pm-conf"                   'noerror 'nomessage)
  (load "ui-conf"                   'noerror 'nomessage)
  (load "completion-framework-conf" 'noerror 'nomessage)
  (load "file-system-conf"          'noerror 'nomessage)
  (load "org-conf"                  'noerror 'nomessage)
  (load "babel-conf"                'noerror 'nomessage)
  (load "roam-conf"                 'noerror 'nomessage)
  (load "ide-conf"                  'noerror 'nomessage)
  (load "exploratory-pkgs-conf"     'noerror 'nomessage)
  
  ;; Load personal elisp utils/configs here...
  ;; 
  (load "utils" 'noerror 'nomessage)
  
  );; End of let


;;; Notes:
;; To add helper functions in Emacs setup
;; (add-hook 'after-init-hook #'qr/my-function)


;;; Outro
;; Some things to impliment after Emacs is done loading config libararies
;;

;;; Runtime Performance
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;;(add-hook 'focus-out-hook #'garbage-collect)




;;; Footer


;;; init.el ends here
