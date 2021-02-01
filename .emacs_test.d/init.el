;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)
(setq package-enable-at-startup nil)

;; Adds the Melpa archive to the list of available repositories
;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

;; Initializes the package infrastructure
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; load org package and our emacs-config.org file
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs_test.d/config.org"))
(org-babel-load-file (expand-file-name "~/.emacs_test.d/config_custom.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diary-list-entries-hook '(diary-include-other-diary-files diary-sort-entries))
 '(excorporate-configuration '("bas.chatel@radboudumc.nl" . "webmail.radboudumc.nl"))
 '(ivy-mode t)
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files nil)
 '(org-agenda-include-diary t)
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(org-clock-into-drawer "CLOCKING")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii html icalendar latex odt))
 '(org-habit-show-habits-only-for-today nil)
 '(org-log-into-drawer t)
 '(org-log-repeat 'time)
 '(org-log-reschedule 'time)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets '((org-agenda-files :tag . ":maxlevel . 3")))
 '(org-refile-use-outline-path 'file)
 '(org-src-lang-modes
   '(("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("dot" . fundamental)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)
     ("jupyter" . python)))
 '(org-track-ordered-property-with-tag t)
 '(package-selected-packages
   '(ox-hugo ox-reveal company-quickhelp darkroom elpy quelpa-use-package unicode-fonts org-fancy-priorities excorporate rotate blacken smartparens crux projectile company-lsp lsp-ui pyvenv ein org-drill doom-modeline amx all-the-icons doom-themes eglot yasnippet-snippets which-key web-mode use-package try symon switch-window spacemacs-theme spaceline smex rainbow-mode rainbow-delimiters popup-kill-ring org-roam-server org-roam-bibtex org-ref org-ql org-pomodoro org-noter org-journal org-gcal org-bullets ob-async multiple-cursors mark-multiple magit ido-vertical-mode hungry-delete google-this expand-region emmet-mode dumb-jump dmenu diminish dashboard counsel company-org-roam buffer-move beacon avy))
 '(python-shell-completion-native-disabled-interpreters '("pypy" "ipython" "python3.9"))
 '(python-shell-interpreter "/usr/local/opt/python@3.9/bin/python3.9"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
