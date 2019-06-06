;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init-key-bindings.el                                     ###
;; ######################################################################
;; ### Provides user configured key-bindings and packages             ###
;; ######################################################################

;; ======================================================================
;; 1.0 Packages
;; ======================================================================
;;{{{

;; General
(use-package general
  :ensure t)

;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Evil-escape
(use-package evil-escape
  :after evil
  :ensure t)

;;}}}

;; ======================================================================
;; 2.0 General bindings
;; ======================================================================
;;{{{

;; Normal mode specific key bindings
(general-define-key
 :states '(normal visual)
 "f" 'evil-avy-goto-char
 "F" 'evil-avy-goto-char)

;; Message-mode specific key bindings
(general-define-key
 :states '(normal insert emacs)
 :keymaps 'message-mode-map
 "<f2>" 'external-abook-try-expand)

;; Org-agneda specific key bindings
(general-define-key
 :states '(normal visual motion emacs)
 :keymaps 'org-agenda-mode-map
 "j" 'org-agenda-next-item
 "J" 'air-org-agenda-next-header
 "k" 'org-agenda-previous-item 
 "K" 'air-org-agenda-previous-header)

;; Leader settings
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 ;; No prefix commands
 "<SPC>" 'counsel-M-x
 "'" 'eshell

 ;; Prefixed commands
 "a" '(:ignore t :which-key "Applications")
 "ac" 'calculator
 ;; "ag" 'go-playground
 "ae" 'mu4e
 "am" '(:ignore t :which-key "Music")
 "amp" '(:ignore t :which-key "Pandora")
 "amp-" 'pianobar-ban-current-song
 "ampc" 'pianobar-change-station
 "amp=" 'pianobar-love-current-song
 "ampn" 'pianobar-next-song
 "ampp" 'pianobar-play-or-pause
 "amps" 'pianobar
 "ams" '(:ignore t :which-key "Spotify")
 "amsc" 'spotify-current
 "amsl" 'spotify-previous
 "amsn" 'spotify-next
 "amsp" 'spotify-playpause
 "aw" '(:ignore t :which-key "Web")
 "awb" 'browse-url-at-point
 "awe" 'eww
 "aww" 'w3m-search
 "b" '(:ignore t :which-key "Buffers")
 "bb" 'ivy-switch-buffer
 "bd" 'kill-buffer
 "bn" 'next-buffer
 "bp" 'previous-buffer
 "c" '(:ignore t :which-key "Comments")
 "ci" 'evilnc-comment-or-uncomment-lines
 "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "cc" 'evilnc-copy-and-comment-lines
 "cp" 'evilnc-comment-or-uncomment-paragraphs
 "cr" 'comment-or-uncomment-region
 "cv" 'evilnc-toggle-invert-comment-line-by-line
 "f" '(:ignore t :which-key "Files")
 "fe" '(:ignore t :which-key "Edit")
 "ff" 'counsel-find-file
 "g" '(:ignore t :which-key "Git")
 "gs" 'magit-status
 "h" '(:ignore t :which-key "help")
 "hk" 'describe-key
 "j" '(:ignore t :which-key "Jump")
 "jj" 'evil-avy-goto-char
 "jr" 'jump-to-register
 "o" '(:ignore t :which-key "Org")
 "oa" 'org-agenda
 "oc" 'org-capture
 "r" '(:ignore t :which-key "Repl")
 "rj" 'cider-jack-in
 "s" '(:ignore t :which-key "Search")
 "si" 'imenu
 "ss" 'swiper
 "e" '(:ignore t :which-key "Edit")
 "es" '(:ignore t :which-key "Spelling")
 "esc" 'flyspell-correct-word-before-point
 "w" '(:ignore t :which-key "Windows")
 "w/" 'split-window-right
 "w-" 'split-window-below
 "w0" 'delete-window
 "wd" 'delete-other-windows
 "wo" 'other-window
 "ws" 'ace-select-window
 )

(provide 'init-key-bindings)
