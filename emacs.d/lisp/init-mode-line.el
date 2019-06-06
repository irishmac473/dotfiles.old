;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init-mode-line.el                                        ###
;; ######################################################################
;; ### Provides mode line configuration                               ###
;; ######################################################################

;; ======================================================================
;; 1.0 Mode line configuration
;; ======================================================================
;;{{{

(use-package smart-mode-line
  :ensure t)
(setq rm-blacklist '(" WK" " SP" " =>" " Undo-Tree" " ivy" " company" " Fly" " FlyC" " FlyC-" " FlyC:0/4" " FlyC:1/4" " FlyC:0/8" " es" " PgLn" " LR" " jk" " ARev" ))
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)

;;}}}

(provide 'init-mode-line)
