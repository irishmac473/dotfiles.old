;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-evil.el                                                   ###
;; ############################################################################
;; ### Provides 'evil-mode' and various other useful 'evil' packages and    ###
;; ### settings                                                             ###
;; ############################################################################

;; ============================================================================
;; 1.0 Evil-mode settings
;; ============================================================================

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

;; ============================================================================
;; 2.0 Evil-collection settings
;; ============================================================================

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

(provide 'init-evil)
