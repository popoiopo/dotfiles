;; General directories
(setq github-dir "~/github/")
(setq dropbox-dir "~/Dropbox/")
(setq emacs-dir "~/.emacs_test.d/")

;; Configuration files
(setq emacs-config-org-file (concat emacs-dir "config.org"))
(setq emacs-config-custom-org-file (concat emacs-dir "config_custom.org"))

;; More specific files
(setq amx-items (concat emacs-dir "amx-items"))
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")

;; Backup files
(setq backup-per-save (concat emacs-dir "backup/per-save"))
(setq backup-per-session (concat emacs-dir "backup/per-session"))

;; Configuration files
(setq zshrc-file "~/.zshrc")
(setq skhdrc-file "~/.skhdrc")
(setq qmk-keymap-file "~/qmk_firmware/keyboards/keebio/iris/keymaps/popoiopo/keymap.c")
(setq yabai-file "~/.yabairc")
(setq qutebrowser-file "~/.qutebrowser/qutemacs.py")

;; Roam
(setq roamnotes-path (concat dropbox-dir "RoamNotes/"))
(setq roam-db-path "~/org-roam.db")

;; More specific files
(setq org-journal-path (concat roamnotes-path "org-journal/"))
(setq journal-file (concat org-journal-path "2021.org"))

;; Reference managing
(setq bib-folder-path (concat roamnotes-path "papers_and_articles/"))
(setq bib-org-file (concat bib-folder-path "references.org"))
(setq references-bib-file (concat bib-folder-path "references.bib"))

;; GTD
(setq gtd-path (concat roamnotes-path "GTD/"))
(setq work-gtd-file (concat gtd-path "work.org"))
(setq personal-gtd-file (concat gtd-path "personal.org"))
(setq phone-gtd-file (concat gtd-path "phone.org"))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\< . ?\>)
                            ))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)

(use-package amx
  :ensure t
  :after ivy
  :custom
  (amx-backend 'auto)
  (ams-save-file amx-items)
  (amx-history-length 50)
  (amx-show-key-bindings nil)
  :config
  (amx-mode 1))

(use-package yasnippet
  :ensure t
  :config (use-package yasnippet-snippets
            :ensure t)
  (yas-reload-all))
(yas-global-mode 1)

(use-package buffer-move
  :ensure t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'kill-all-buffers)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(global-set-key (kbd "C-c b") 'er-switch-to-previous-buffer)

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "h" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key (kbd "C-M-f") 'toggle-maximize-buffer)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package rotate
   :ensure t)

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t  ;; Copy all files, don't rename them.
      auto-save-interval 100 ;; Change interval of characters to which auto-save is enabled
      )

(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs_test.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs_test.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(defun config-visit ()
  (interactive)
  (find-file emacs-config-org-file))

(defun config-custom-visit ()
  (interactive)
  (find-file emacs-config-custom-org-file))

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name emacs-config-org-file)))

(defun save-and-exit()
  "Simple convenience function.
    Saves the buffer of the current day's entry and kills the window
    Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))
(global-set-key (kbd "C-x C-S-s") 'save-and-exit)
(global-set-key (kbd "C-x C-M-S-s") 'org-save-all-org-buffers)

;; set up my own map for files, folder and windows
(define-prefix-command 'z-map)
(global-set-key (kbd "C-z") 'z-map)
(define-key z-map (kbd "a") 'org-agenda)
(define-key z-map (kbd "n") 'narrow-or-widen-dwim)
(define-key z-map (kbd "C-t") 'toggle-transparency)

;; Grammarly
(define-key z-map (kbd "g g") 'grammarly-pull)
(define-key z-map (kbd "g p") 'grammarly-push)

;; Buffer movement
(define-key z-map (kbd "<left>") 'shrink-window-horizontally)
(define-key z-map (kbd "<right>") 'enlarge-window-horizontally)
(define-key z-map (kbd "<down>") 'shrink-window)
(define-key z-map (kbd "<up>") 'enlarge-window)
(define-key z-map (kbd "C-<up>") 'buf-move-up)
(define-key z-map (kbd "C-<down>") 'buf-move-down)
(define-key z-map (kbd "C-<left>") 'buf-move-left)
(define-key z-map (kbd "C-<right>") 'buf-move-right)
(define-key z-map (kbd "C-r") 'rotate-layout)
(define-key z-map (kbd "C-m v") 'rotate:main-vertical)
(define-key z-map (kbd "C-m h") 'rotate:main-horizontal)
(define-key z-map (kbd "C-e t") 'rotate:tiled)
(define-key z-map (kbd "C-e v") 'rotate:even-vertical)
(define-key z-map (kbd "C-e h") 'rotate:even-horizontal)

;; UNCOMMENT IF YOU'RE NOT ME :)
(define-key z-map (kbd "z") (defun zshrcEdit () (interactive) (find-file zshrc-file)))
(define-key z-map (kbd "s") (defun skhdEdit() (interactive) (find-file skhdrc-file)))
(define-key z-map (kbd "k") (defun keyboardEdit() (interactive) (find-file qmk-keymap-file)))
(define-key z-map (kbd "y") (defun yabaiEdit() (interactive) (find-file yabai-file)))
(define-key z-map (kbd "q") (defun qutebrowserEdit() (interactive) (find-file qutebrowser-file)))

;; Bibfile and ref management files
(define-key z-map (kbd "b") (defun bibOrgEdit() (interactive) (find-file bib-org-file)))
(define-key z-map (kbd "C-b") (defun bibtexEdit() (interactive) (find-file references-bib-file)))

;; GTD files
(define-key z-map (kbd "p") (defun work-GTD() (interactive) (find-file personal-gtd-file)))
(define-key z-map (kbd "w") (defun personal-GTD() (interactive) (find-file work-gtd-file)))
(define-key z-map (kbd "f") (defun phone-GTD() (interactive) (find-file phone-gtd-file)))
(define-key z-map (kbd "t") (defun phone-GTD() (interactive) (find-file journal-file)))
(define-key z-map (kbd "o p") 'org-focus-private)
(define-key z-map (kbd "o f") 'org-focus-phone)
(define-key z-map (kbd "o w") 'org-focus-work)
(define-key z-map (kbd "o a") 'org-focus-all-future)
(define-key z-map (kbd "o P") 'org-focus-past)
(define-key z-map (kbd "o A") 'org-focus-all)

;; Emacs config files
(define-key z-map (kbd "E") 'config-custom-visit)
(define-key z-map (kbd "r") 'config-reload)
(define-key z-map (kbd "e") 'config-visit)

;; Timer
(define-key z-map (kbd "k") 'show-msg-after-timer)

;; ORG extra keybinding
;; Store a reference link to an org mode location
(global-set-key (kbd "C-c l") 'org-store-link)

;; Add an extra cursor above or below current cursor
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

;; Remove an extra cursor above or below current cursor
(global-set-key (kbd "C-,") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-.") 'mc/unmark-next-like-this)

;; Skip a spot in adding a new cursor above or below
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)

;; Mark all entries in current selection (useful if you want to rename a variable in the whole file)
(global-set-key (kbd "C-M-,") 'mc/mark-all-like-this)

;; Create cursors on every line in selected area
(global-set-key (kbd "C-M-.") 'mc/edit-lines)

;; Insert numbers with increased index for exery cursor (useful for lists)
(global-set-key (kbd "C-;") 'mc/insert-numbers)

;; Same as numbers but then with letters
(global-set-key (kbd "C-M-;") 'mc/insert-letters)

;; With control shift and a mouse-click add cursor
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Kill whole line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Go anywhere in just a few strokes
(global-set-key (kbd "C-S-s") 'avy-goto-char)

;; Refile within same file
(global-set-key (kbd "C-c w") 'org-refile-in-file)

;;;;;;;;;;;;;;;;;;;;
;;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; replace \n\n with bigskip
(defun my-replace-double-newline (backend)
  "replace multiple blank lines with bigskip"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n\n+" nil t)
    (replace-match "\n#+LATEX: \\par\\vspace{\\baselineskip}\\noindent\n" nil t)
    ;;(replace-match "\n#+LATEX: \\bigskip\\noindent\n" nil t)
    (forward-char 1)))

(add-hook 'org-export-before-processing-hook 'my-replace-double-newline)

;; This setup is tested on Emacs 24.3 & Emacs 24.4 on Linux/OSX
;; org v7 bundled with Emacs 24.3
(setq org-export-odt-preferred-output-format "doc")
;; org v8 bundled with Emacs 24.4
(setq org-odt-preferred-output-format "doc")
;; BTW, you can assign "pdf" in above variables if you prefer PDF format
;; Only OSX need below setup
(defun my-setup-odt-org-convert-process ()
  (setq process-string "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
     (when (and (eq system-type 'darwin) (file-exists-p cmd))
       ;; org v7
       (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
       ;; org v8
       (setq org-odt-convert-processes '(("LibreOffice"  "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
     ))
 (my-setup-odt-org-convert-process)

(eval-after-load "org"
  '(require 'ox-md nil t))

(use-package ox-reveal
:ensure ox-reveal)
(setq org-reveal-mathjax t)
(use-package htmlize :ensure t)

(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

(require 'ox-beamer)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(95 . 95) '(100 . 100)))))

(use-package avy :ensure t)

(require 'multiple-cursors)

;;expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq org-indent-indentation-per-level 1)             ;; Shorten the space on the left side with org headers
(setq org-adapt-indentation nil)                      ;; Adapt indentation to outline node level. Set to nill as it takes up space.
(setq org-hide-emphasis-markers t)                    ;; When making something bold *Hallo*, hide stars. Goes for all emphasis markers.
(setq org-cycle-separator-lines 1)                    ;; Leave a single empty line between headers if there is one. Otherwise leave no room or make the empty lines belong to the previous header.
(setq org-hide-leading-stars 't)                      ;; Hide the extra stars in front of a header (org-bullet displays nicer, but why add extra package)
(customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))   ;; Dont randomly remove newlines below headers

(setq org-image-actual-width 600)

(setq org-priority-highest ?A)
(setq org-priority-default ?C)
(setq org-priority-lowest ?E)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq save-interprogram-paste-before-kill t) ;; Perpetuates system clipboard
(setq scroll-conservatively 1)      ;; Keep from making huge jumps when scrolling
(setq ring-bell-function 'ignore)   ;; Unable annoying sounds
(setq visible-bell 1)               ;; disable annoying windows sound
(setq inhibit-startup-message t)    ;; Hide the startup message
(setq display-time-24hr-format t)   ;; Format clock
(setq-default display-line-numbers 'relative) ;; Setting the line numbers
(when window-system (global-hl-line-mode t)) ;; Get a current line shadow in IDE
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Replace yes questions to y
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; I never want whitespace at the end of lines. Remove it on save.
(setq sentence-end-double-space nil);; Start a new sentence with just a single space instead of 2
(exec-path-from-shell-initialize)   ;; Fixes path issues on mac emacs.app so that same env of terminal is used

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(tool-bar-mode -1)                  ;; Get rid of tool-bar
(menu-bar-mode -1)                  ;; Git rid of menu
(scroll-bar-mode -1)                ;; Get rid of scroll-bar
(global-auto-revert-mode 1)         ;; Make sure that you're always looking at the latest version of a file. Change file when changed on disk
(delete-selection-mode 1)           ;; Remove text from selection instead of just inserting text
(display-time-mode 1)               ;; Set clock on lower right side
;; (electric-pair-mode t)              ;; Enable electric pair mode. It autocompletes certain pairs. E.g., (), {}, [], <>
(global-subword-mode 1)             ;; Cause M-f to move forward per capitalization within a word. E.g., weStopAtEveryCapital
(global-visual-line-mode 1)                ;; Make sure that lines do not disapear at the right side of the screen but wrap around
(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(setq org-todo-keyword-faces
      '(
	("NEXT" . (:foreground "#05d3fc" :weight bold :box (:line-width 2 :style released-button)))
	("WAITING" . (:foreground "#fcca05" :weight bold :box (:line-width 2 :style released-button)))
	("FLEETING" . (:foreground "#f62af9" :weight bold :box (:line-width 2 :style released-button)))
	("PROJ" . (:foreground "#a768f9" :weight bold :box (:line-width 2 :style released-button)))
	("LONGTERM" . (:foreground "#c4013c" :weight bold :box (:line-width 2 :style released-button)))
	("CANCELED" . (:foreground "#fc4205" :weight bold :box (:line-width 2 :style released-button)))
	))

(setq org-todo-keywords
      '((sequence "TODO(t/!)" "NEXT(n/!)" "WAITING(w@/!)" "FLEETING(f)" "PROJ(p)" "LONGTERM(l@/!)" "REPEAT(r)" "|" "CANCELED(c@/!)" "DONE(d/!)")))

(setq org-log-repeat nil)
(setq org-todo-repeat-to-state "REPEAT")

(use-package smartparens
    :config
    (add-hook 'prog-mode-hook 'smartparens-mode))

(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido nil)
(setq org-refile-use-outline-path 't)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(defun org-refile-in-file ()
  "Refile to a target within the current file."
  (interactive)
  (let ((org-refile-targets `(((,(buffer-file-name)) :maxlevel . 6))))
    (call-interactively 'org-refile)))

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

(use-package try
    :ensure t)

(use-package beacon
    :ensure t
    :init
    (beacon-mode 1))

(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(setq doom-modeline-buffer-file-name-style 'truncate-all)

(use-package all-the-icons :ensure t)
