;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-smartparens.el                                            ###
;; ############################################################################
;; ### Provides plugins and settings for smartparens                        ###
;; ############################################################################

;; ============================================================================
;; 1.0 Packages
;; ============================================================================
;;{{{

(use-package smartparens
  :ensure t)

;; evil intergration with smartparens
(use-package evil-smartparens
  :after evil
  :ensure t)

;;}}}

;; ============================================================================
;; 2.0 Settings
;; ============================================================================
;;{{{

(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;;}}}

(provide 'init-smartparens)
