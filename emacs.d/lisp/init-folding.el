;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-folding.el                                                ###
;; ############################################################################
;; ### Provides package(s) and settings for code folding                    ###
;; ############################################################################

;; ============================================================================
;; 1.0 Vimish-fold-mode settings
;; ============================================================================
;;{{{

(use-package vimish-fold
  :ensure t
  :config
  (vimish-fold-global-mode 1))

;;}}}

(provide 'init-folding)
