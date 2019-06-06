;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init-ace.el                                              ###
;; ######################################################################
;; ### Provides package and configuration for Ace-window              ###
;; ######################################################################

;; ======================================================================
;; 1.0 Packages
;; ======================================================================

(use-package ace-window
  :ensure t)

;; ======================================================================
;; 2.0 Settings
;; ======================================================================

(progn
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(provide 'init-ace)
