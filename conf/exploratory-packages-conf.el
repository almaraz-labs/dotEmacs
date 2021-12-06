;;; exploratory-pkgs-conf.el --- Emacs Exploratory Packages Configuration  -*- lexical-binding: t; -*-

;; Author: Erik Almaraz <erik@almarazlabs.com>
;; Maintainer: Erik Almaraz <erik@almarazlabs.com>
;; Created 2021-12-05 Sun
;; Version: 0.1
;; Keywords: Emacs, .emacs, init, Emacs Lisp
;; URL: https;//github.com/skribest/emacs-config

;;; Commentary

;; Introduction (TBD)


;;; References:

;;   [1] 
;;   [2] 
;;   [3] 

;;<=================================================================================================>


;;; Code:

;;; Definitions


;;; Private/Helper Functions

;;; Configuration

;; Chemtable & chembalance
(use-package chemtable
  :init
  (require 'chemtable)
  ;;:config
  )

(use-package chembalance
  :init
  (require 'chembalance)
  ;;:config
  ;;(chembalance-arrow-syntax (list of accepted arrows))
  ;;(chembalance-insert-string 1) ;if non-nil, when you call chembalance with selected region,
  ;;chembalance will kill that region and insert the balanced reaction.
  )


;;; EXWM & StumpWM
;; EXWM => https://github.com/ch11ng/exwm
;; StumpWM => https://github.com/stumpwm/stumpwm


;;; Notes:
;; To add helper functions in Emacs setup
;; (add-hook 'after-init-hook #'ea/my-function)




;;; Footer


;;; exploratory-pkgs-conf.el ends here
