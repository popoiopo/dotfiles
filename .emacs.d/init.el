;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)

;; Adds the Melpa archive to the list of available repositories
;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))

;; Initializes the package infrastructure
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Enable Org mode
(require 'org)

;; User-Defined init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(org-html-postamble
   (quote
    (("en" "<p class=\"postamble\">Hallo dit is een test</p>"))))
 '(org-html-postamble-format
   (quote
    (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date wekrkt dit dan?!?!?! aaaah: %d</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p>"))))
 '(org-pomodoro-expiry-time 240)
 '(org-pomodoro-finished-sound
   "c:/Users/chatel/AppData/Roaming/.emacs.d/elpa/org-pomodoro-20190530.1445/resources/nice-work.wav")
 '(org-pomodoro-finished-sound-args "-volume 0.3")
 '(org-pomodoro-long-break-sound
   "c:/Users/chatel/AppData/Roaming/.emacs.d/elpa/org-pomodoro-20190530.1445/resources/shall-we.wav")
 '(org-pomodoro-overtime-sound
   "c:/Users/chatel/AppData/Roaming/.emacs.d/elpa/org-pomodoro-20190530.1445/resources/nice-work.wav")
 '(org-pomodoro-short-break-sound
   "c:/Users/chatel/AppData/Roaming/.emacs.d/elpa/org-pomodoro-20190530.1445/resources/focus.wav")
 '(package-selected-packages
   (quote
    (org-noter-pdftools pdf-tools ox-twbs sphinx-doc anaconda-mode jedi-direx company-jedi auctex-latexmk org-pdfview powershell sound-wav org-pomodoro buffer-move org-noter org-plus-contrib ag dumb-jump eyebrowse all-the-icons-ivy org-gcal undo-tree google-this ob-session-async-R ob-async emmet-mode org-ref ess-smart-underscore ess epc jedi htmlize ox-reveal counsel try yasnippet-snippets pretty-mode expand-region mark-multiple swiper popup-kill-ring symon dmenu diminish spaceline dashboard rainbow-delimiters hungry-delete switch-window rainbow-mode avy smex ido-vertical-mode org-bullets beacon spacemacs-theme which-key use-package material-theme better-defaults)))
 '(safe-local-variable-values (quote ((org-confirm-babel-evaluate))))
 '(send-mail-function (quote mailclient-send-it))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Courier New")))))
(put 'downcase-region 'disabled nil)
