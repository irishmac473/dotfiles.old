;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-colors.el                                                 ###
;; ############################################################################
;; ### Provides color schemes and color settings                            ###
;; ############################################################################

;; ============================================================================
;; 1.0 Color schemes
;; ============================================================================
;;{{{

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t)

;; (use-package monokai-theme
;;   :ensure t)

;; (use-package ample-theme
;;   :ensure t)

;; (use-package grandshell-theme
;;   :ensure t)

;; (use-package apropospriate-theme
;;   :ensure t)

(use-package spacemacs-common
  :ensure spacemacs-theme)

;;}}}

;; ============================================================================
;; 2.0 Color scheme settings
;; ============================================================================
;;{{{

(load-theme 'spacemacs-dark t)
(setq spacemacs-theme-comment-bg nil)
(setq spacemacs-theme-comment-italic t)

;; ;;
;; ;; Update the color of the company-mode context menu to fit the Monokai theme
;; ;; @source: https://github.com/search?q=deftheme+company-tooltip&type=Code
;; ;;
;; (deftheme monokai-overrides)

;; (let ((class '((class color) (min-colors 257)))
;;       (terminal-class '((class color) (min-colors 89))))

;;   (custom-theme-set-faces
;;    'monokai-overrides

;;    ;; Company tweaks.
;;    `(company-tooltip-common
;;      ((t :foreground "#F8F8F0"
;;          :background "#474747"
;;          :underline t)))

;;    `(company-template-field
;;      ((t :inherit company-tooltip
;;          :foreground "#C2A1FF")))

;;    `(company-tooltip-selection
;;      ((t :background "#349B8D"
;;          :foreground "#BBF7EF")))

;;    `(company-tooltip-common-selection
;;      ((t :foreground "#F8F8F0"
;;          :background "#474747"
;;          :underline t)))

;;    `(company-scrollbar-fg
;;      ((t :background "#BBF7EF")))

;;    `(company-tooltip-annotation
;;      ((t :inherit company-tooltip
;;          :foreground "#C2A1FF")))

;;    ;; Popup menu tweaks.
;;    `(popup-menu-face
;;      ((t :foreground "#A1EFE4"
;;          :background "#49483E")))

;;    `(popup-menu-selection-face
;;      ((t :background "#349B8D"
;;          :foreground "#BBF7EF")))))

;;}}}

(provide 'init-colors)
