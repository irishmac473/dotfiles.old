;; ############################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                      ###
;; ### File: init-elpa.el                                                   ###
;; ############################################################################
;; ### Provides the package-archives to use and makes use of John           ###
;; ### Wiegley's 'use-package' for package management.                      ###
;; ############################################################################

(package-initialize nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("org" . 4)
                                   ("melpa-stable" . 3)
                                   ("melpa" . 2)
                                   ("gnu" . 1)))
(add-to-list 'package-pinned-packages
             '(use-package . "melpa-stable"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))

;; Use shell $PATH env
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(provide 'init-elpa)
