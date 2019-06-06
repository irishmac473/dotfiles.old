;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init-ivy.el                                              ###
;; ######################################################################
;; ### Provides the ivy eco-system and settings                       ###
;; ######################################################################

;; ======================================================================
;; 1.0 Packages
;; ======================================================================
;;{{{

(use-package ivy
  :ensure t)

;; These are required by ivy
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t)

;; Counsel/projectile intergration
(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

;; Smex to give counsel-M-x "recents"
(use-package smex
  :ensure t)

;;}}}

;; ======================================================================
;; 2.0 Ivy settings
;; ======================================================================
;;{{{

(ivy-mode 1)
;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 10)
;; does not count candidates
(setq ivy-count-format "")
;; no regexp by default
(setq ivy-initial-inputs-alist nil)
;; configure regexp engine.
(setq ivy-re-builders-alist
;; allow input not in order
      '((t   . ivy--regex-ignore-order)))

;;}}}

(provide 'init-ivy)
