;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-spell.el                                                  ###
;; ############################################################################
;; ### Provides Packages and settings for spell checking                    ###
;; ############################################################################

;; ============================================================================
;; 1.0 Packages
;; ============================================================================

(use-package flyspell
  :ensure t)

;; ============================================================================
;; 2.0 Settings
;; ============================================================================

(setq flyspell-issue-message-flg nil)
(add-hook 'prog-mode-hook
          (lambda () (flyspell-prog-mode)))
(add-hook 'text-mode-hook
          (lambda () (flyspell-mode)))


(provide 'init-spell)
