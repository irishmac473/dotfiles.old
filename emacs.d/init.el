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
