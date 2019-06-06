;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init.el                                                  ###
;; ######################################################################
;; ### Emacs config focused on evil-mode and Web Development          ###
;; ######################################################################

(package-initialize)

;; ======================================================================
;; 1.0 Load paths
;; ======================================================================

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; ======================================================================
;; 2.0 Load configs for various settings, features and modes
;; ======================================================================

;; ======================================================================
;; Bootstrap config
;; ======================================================================

(require 'init-elpa)

;; ======================================================================
;; Emacs
;; ======================================================================

(require 'init-basics)
(require 'init-key-bindings)

;; ======================================================================
;; Checkers
;; ======================================================================

(require 'init-syntax)
(require 'init-spell)

;; ======================================================================
;; Completion
;; ======================================================================

(require 'init-company)
(require 'init-ivy)
(use-package avy
  :ensure t)

;; ======================================================================
;; Other
;; ======================================================================

(require 'init-smartparens)
(require 'init-rainbow)
(require 'init-folding)
(require 'init-ace)
(require 'init-entertainment)

;; ======================================================================
;; Source Control
;; ======================================================================

(require 'init-git)

;; ======================================================================
;; Tools
;; ======================================================================

(use-package projectile
  :ensure t)

;; ======================================================================
;; Vim
;; ======================================================================

(require 'init-evil)
(require 'init-matchit)
(use-package evil-nerd-commenter
  :ensure t)

;; ======================================================================
;; Themes
;; ======================================================================

(require 'init-colors)
; (require 'init-splash)
(require 'init-mode-line)


; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(package-selected-packages
;    (quote
;     (aggressive-indent linum-relative use-package exec-path-from-shell))))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-collection-setup-minibuffer t)
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "  ")
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   (quote
    (evil-escape which-key general use-package spacemacs-theme smart-mode-line linum-relative exec-path-from-shell evil-nerd-commenter evil-matchit evil-collection dashboard aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
