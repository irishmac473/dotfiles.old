;; ######################################################################
;; ### Author: Caleb McCaffery <irishmac473@gmail.com>                ###
;; ### File: init-org.el                                              ###
;; ######################################################################
;; ### Provides packages and settings for org-mode                    ###
;; ######################################################################

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :config
  (progn
    ;; Inspired by Aaron Bieber's blog post https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
    ;; We build a custom agenda view. It starts with a "High-priority" task view that
    ;; only list priorty "A" task. It's followed by an agenda view. Lastly is a listing
    ;; of the remaining lower priorty tasks.
    (setq org-agenda-files '("~/Dropbox/org/"))
    (defun air-org-skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.
      PRIORITY may be one of the characters ?A, ?B, or ?C."
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (pri-value (* 1000 (- org-lowest-priority priority)))
            (pri-current (org-get-priority (thing-at-point 'line t))))
        (if (= pri-value pri-current)
            subtree-end
          nil)))
    (defun air-org-skip-subtree-if-habit ()
      "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (if (string= (org-entry-get nil "STYLE") "habit")
            subtree-end
          nil)))
    (setq org-agenda-custom-commands
          '(("c" "Simple agenda view"
             ((tags "PRIORITY=\"A\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "High-priority unfinished tasks:")))
              (agenda "" ((org-agenda-span 1)))
              (alltodo ""
                       ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                       (air-org-skip-subtree-if-priority ?A)
                                                       (org-agenda-skip-if nil '(scheduled deadline))))
                        (org-agenda-overriding-header "ALL normal priority tasks:")))))))
    ;; Navigate arround in agenda view. j and k move up and down. J and K skip to
    ;; headers. Another Aaron Bieber steal.
    (defun air-org-agenda-next-header ()
      "Jump to the next header in an agenda series."
      (interactive)
      (air--org-agenda-goto-header))

    (defun air-org-agenda-previous-header ()
      "Jump to the previous header in an agenda series."
      (interactive)
      (air--org-agenda-goto-header t))

    (defun air--org-agenda-goto-header (&optional backwards)
      "Find the next agenda series header forwards or BACKWARDS."
      (let ((pos (save-excursion
                   (goto-char (if backwards
                                  (line-beginning-position)
                                (line-end-position)))
                   (let* ((find-func (if backwards
                                         'previous-single-property-change
                                       'next-single-property-change))
                          (end-func (if backwards
                                        'max
                                      'min))
                          (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                             (funcall find-func (point) 'org-agenda-date-header)))
                          (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                          (prop-pos (if all-pos (apply end-func all-pos) nil)))
                     prop-pos))))
        (if pos (goto-char pos))
        (if backwards (goto-char (line-beginning-position)))))

    ;; org-capture
    (setq org-capture-templates
          '(("t" "todo" entry (file "~/Dropbox/org/refile.org")
             "* TODO [#B] %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
            ("a" "appointment" entry (file "~/Dropbox/org/refile.org")
             "*  %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%^T\n%a\n")
            ("n" "note" entry (file "~/Dropbox/org/refile.org")
             "* %? :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
            ("i" "idea" entry (file "~/Dropbox/org/refile.org")
             "* %? :IDEA:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
            ("h" "habit" entry (file "~/Dropbox/org/refile.org")
             "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:STYLE:    haibt\n:END:")))
    ;; Another Aaron Bieber [[https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html][blog]] inspired block. Refile to any org file in my
    ;; agenda path. Show the file and headings in the ivy search and also allow the
    ;; creating of new headings.
    (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    ;; org-todo keywords
    (setq org-todo-keywords '((sequence "TODO(t/!)" "NEXT(n/!)" "WAIT(w@/!)")
                              (sequence "|" "DONE(d!)" "CANCELLED(c@/!)")))
    (setq org-highest-priority ?A)
    (setq org-lowest-priority ?C)
    (setq org-default-priority ?B)
    ;; Template selector taken from here: https://emacs.stackexchange.com/questions/12841/quickly-insert-source-blocks-in-org-mode
    (add-to-list 'org-structure-template-alist
                 '("e" "#+BEGIN_SRC emacs-lisp tangle:yes \n?\n#+END_SRC"))
    ))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;(use-package evil-org
;;:ensure t
;;:after org evil
;;:config
;;(add-hook 'org-mode-hook 'evil-org-mode)
;;(add-hook 'evil-org-mode-hook
;;(lambda ()
;;(evil-org-set-key-theme))));; ;; Dash and Monitor are packages required my org-evil
;; (use-package dash
;;   :ensure t)

;; (use-package monitor
;;   :ensure t)

;; (use-package org-evil
;;   :after evil
;;   :ensure t)

(provide 'init-org)
