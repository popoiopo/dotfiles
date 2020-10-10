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
 '(ivy-mode t)
 '(org-agenda-files
   '("/Users/chatel/Dropbox/RoamNotes/org-journal/2020-10-09.org"))
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(package-selected-packages
   '(doom-modeline amx all-the-icons doom-themes eglot yasnippet-snippets which-key web-mode use-package try symon switch-window spacemacs-theme spaceline smex rainbow-mode rainbow-delimiters popup-kill-ring ox-twbs ox-reveal org-roam-server org-roam-bibtex org-ref org-ql org-pomodoro org-noter org-journal org-gcal org-bullets ob-async multiple-cursors mark-multiple magit ido-vertical-mode hungry-delete google-this expand-region emmet-mode dumb-jump dmenu diminish dashboard counsel company-org-roam buffer-move beacon avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
