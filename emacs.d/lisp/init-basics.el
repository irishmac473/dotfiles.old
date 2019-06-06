;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>               ###
;; ### File: init-basics.el                                          ###
;; #####################################################################
;; ### Basic configuration settings                                  ###
;; #####################################################################

;; =====================================================================
;; 1.0 Basic Configuration settings
;; =====================================================================

;; Personal information
(setq user-full-name "Caleb McCaffery"
      user-mail-address "irishmac473@gmail.com")

;; Scrollbar, tool bar & menu bar
;;(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Get rid of annoying notifications
(setq ring-bell-function 'ignore)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

;; Frame startup size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;; Highlight current line
(global-hl-line-mode)

;; Ibuffer
(defalias 'list-buffers 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Line by line scrolling
(setq scroll-step 1)

;; Set fill column
(setq-default fill-column 72)

;; Set the default comment column
(setq-default comment-column 72)

;; Turn off word wrap
(setq-default truncate-lines 1)

;; Indent size
(setq standard-indent 2)

;; Treat new buffers as text
(setq default-major-mode 'text-mode)

;; Turn off tab
(setq-default indent-tabs-mode nil)

;; Time in modeline
(setq display-time-24hr-format t)
(display-time-mode 1)

;; Column numbers
(column-number-mode 1)

;; Yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; Font
(add-to-list 'default-frame-alist
	     '(font . "Ubuntu Mono-13"))

;; ======================================================================
;; 2.0 Packages (these should be basic settings provided)
;; ======================================================================

;; Line numbers
(use-package linum-relative
  :ensure t)
(add-hook 'text-mode-hook 'linum-relative-mode)
(add-hook 'prog-mode-hook 'linum-relative-mode)
(setq linum-relative-current-symbol "")

;; Aggressive-indent-mode
(use-package aggressive-indent
  :ensure t)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)

;; ======================================================================
;; 3.0 Functions
;; ======================================================================

;; Change window splitting to prefer side by side
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'my-split-window-sensibly)

(provide 'init-basics)
