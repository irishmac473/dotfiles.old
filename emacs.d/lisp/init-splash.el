;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init-splash.el                                           ###
;; ######################################################################
;; ### Provides a splash screen on startup                            ###
;; ######################################################################

;; ======================================================================
;; 1.0 Dashboard
;; ======================================================================
;;{{{

;; required by dashboard
(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;;}}}

;; ======================================================================
;; Dashboard settings
;; ======================================================================
;;{{{

;; Set the title
(setq dashboard-banner-logo-title "Time to get to work SUCKER!!!!")

;; Set the banner
;; Value can be:
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer
(setq dashboard-startup-banner 'official)

;; Set the widgets displayed
;; available widgets:
;; recents, bookmarks, projects, agenda and registers
(setq dashboard-items '((recents  . 5)
			(projects . 5)))

;;}}}

(provide 'init-splash)
