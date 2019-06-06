;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-git.el                                                    ###
;; ############################################################################
;; ### Provides Packages and settings for git integration                   ###
;; ############################################################################

(use-package magit
  :ensure t)

(use-package evil-magit
  :after evil
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:update-interval 2)
   '(git-gutter:modified-sign "  ")
   '(git-gutter:added-sign "++")
   '(git-gutter:deleted-sign "--")
   '(git-gutter:hide-gutter t))

  (set-face-background 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red"))

(provide 'init-git)
