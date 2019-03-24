;; -- optimizations
;; set garbage-collection treshold to a very large number for the purpose of loading init.el
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; --

(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-archive-priorities '(("melpa" . 15) ("gnu" . 10) ("org" . 5) ("melpa-stable" . 0)))

;; for portability to emacs instances without use-package already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t) ; auto install packages

(defun ui-config ()
  (add-to-list 'default-frame-alist '(font . "Menlo 14"))

  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; mac windowing options
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; options (light|dark)

  (setq ring-bell-function 'ignore)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (show-paren-mode 1) ; show matching parenthesis
  ;; (global-display-line-numbers-mode 1) ; native line numbers mode:

  (setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
  ;; (pixel-scroll-mode t)
  ;; (setq pixel-resolution-fine-flag t)
  ;; (setq pixel-dead-time 0)
  )(ui-config)

(defun general-config ()

  (setq-default indent-tabs-mode nil) ; spaces instead of tabs

  (setq mode-require-final-newline nil)

  (electric-pair-mode t) ; smart auto-closing of parens

  (setq scroll-step 1) ; navigate off-screen scroll one line at a time
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil)

  ;; load path from shell (on mac)
  (use-package exec-path-from-shell :config (exec-path-from-shell-initialize))

  (setq load-prefer-newer t) ; prefer loading newer versions of packages and elisp files

  (setq latex-run-command "texi2dvi --pdf --clean --verbose --batch")
  (setq doc-view-resolution 800)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode) ; Auto reload on file changed

  (global-set-key (kbd "s-+") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)

  ;; make emacs put all the custom-settings noise in this file, and then never load it
  (setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))

  ;; terminal stuff
  (defun zsh-panel ()
    "Toggle a terminal in a small pane at the bottom of the screen."
    (interactive)
    (let ((new-name "term panel") (existing-name "*term panel*"))
      (let ((panel-window (get-buffer-window existing-name)))
        (if panel-window
            (delete-window panel-window)
          (evil-window-split)
          (evil-window-decrease-height 16)

          (if (get-buffer existing-name)
              (switch-to-buffer existing-name)
            (ansi-term "/bin/zsh" new-name))))))

  (defun zsh ()
    "Start a terminal in a new buffer without having to provide a name."
    (interactive)
    (ansi-term "/bin/zsh" "term"))

  (global-set-key (kbd "C-x t") 'zsh-panel)

  ;; allow command+v pasting in term mode
  (eval-after-load "term"
    '(define-key term-raw-map (kbd "s-v") 'term-paste))
  )(general-config)

;; ---------------------------
;; [ JS ]
;; ---------------------------

;; (use-package yasnippet
;;   :commands yas-minor-mode
;;   :config
;;   (use-package yasnippet-snippets
;;     :config (yas-reload-all)))

;; (use-package tide
;;   :config
;;   (setq tide-completion-ignore-case t)
;;   ;; (setq tide-completion-detailed t)
;;   (setq tide-filter-out-warning-completions t)
;;   (setq tide-default-mode "JS")
;;   :hook (js-mode . (lambda ()
;;                      (tide-setup)
;;                      ;; (flycheck-mode +1)
;;                      ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;                      (eldoc-mode +1)
;;                      (tide-hl-identifier-mode +1)
;;                      (push '(company-tide :with company-yasnippet) company-backends)))
;;   )

;; ;; tide ligger seg oppå js-mode.. kan også ligge seg oppå js2-mode

;; ;; (use-package js2-mode
;; ;;   :hook js-mode
;; ;;   :config
;; ;;   (setq js2-mode-show-parse-errors nil)
;; ;;   (setq js2-mode-show-strict-warnings nil)
;; ;;   )

;; (defun my-js-mode-hook ()
;;   ;; (use-package npm-mode)
;;   ;; (npm-mode)
;;   ;; (use-package javascript-eslint)
;;   (use-package prettier-js
;;     :config (prettier-js-mode))
;;   (use-package json-mode)
;;   (setq js-indent-level 2)
;;   (yas-minor-mode))

;; (add-hook 'js-mode-hook 'my-js-mode-hook)

;; (use-package rjsx-mode
;;   :defer t)

;; ---------------------------
;; [ Misc. packages ]
;; ---------------------------

(use-package racket-mode
  :mode ("\\.rkt\\'"))

(use-package haskell-mode
  :mode ("\\.hi\\'" "\\.hs\\'"))

(use-package markdown-mode
  :mode "\\.md$")

(use-package magit
  :bind ("C-x m" . magit))

(use-package projectile
  :defer t
  :config (projectile-mode))

(use-package helm
  :demand t
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x y" . helm-show-kill-ring)
  ("C-x k" . helm-buffers-list)
  ("C-x b" . helm-mini)
  ("C-x i" . helm-imenu)
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-M-x-fuzzy-match t)
  ;; For tramp / ssh, makes helm skip checking if files still exists on remote (very painful on slow ssh connections)
  (setq helm-buffer-skip-remote-checking t)
  (helm-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "magit*")
  (helm-autoresize-mode)

  (use-package helm-projectile
    :after (projectile)
    :bind
    ("C-x p" . helm-projectile))
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (setq projectile-switch-project-action 'helm-projectile)
)

(use-package company
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package which-key
  :config (which-key-mode))

(use-package helm-spotify-plus :commands helm-spotify-plus)
(use-package restclient :commands restclient-mode) ; awesome postman like mode
;; (use-package focus :commands focus-mode)

;; ---------------------------
;; [ Evil ]
;; ---------------------------

(use-package evil
  :config
  (evil-mode)
  ;; (setq evil-scroll-count 3)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; initial evil state for these modes
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'special-mode 'motion)
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  ;; bindings
  (global-set-key (kbd "M-j") (lambda() (interactive) (evil-scroll-line-down 3)))
  ;; bind-key* kraftigere, for å fikse M-j scrolling i C++ mode
  (bind-key* "M-j" (lambda() (interactive) (evil-scroll-line-down 3)))
  (global-set-key (kbd "M-k") (lambda() (interactive) (evil-scroll-line-up 3)))
  (define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)
  (define-key evil-motion-state-map (kbd "TAB") nil)

  (use-package evil-commentary
    :config
    (evil-commentary-mode))

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package ace-jump-mode
    :config
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
    (define-key evil-motion-state-map (kbd "SPC") 'ace-jump-mode))
  )

;; ---------------------------
;; [ Org-Mode ]
;; ---------------------------

(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
  (setq org-hierarchical-todo-statistics nil)
  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t)
  (setq org-cycle-separator-lines -2)

  ;; org babel
  (setq org-babel-python-command "python3")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (js . t) (dot . t))
   )

  (add-hook 'org-mode-hook (lambda () (toggle-truncate-lines))) ; truncate lines

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))


  ;; (use-package epresent :commands (epresent-run))
  
  ;; org-tree-slide, different style of presentation than org-present, both are good
  ;; (use-package org-tree-slide :defer t)
  ;; org-present
  (use-package org-present
    :defer t
    :config
    (progn
      (add-hook 'org-present-mode-hook
                (lambda ()
                  (turn-off-evil-mode)
                  (org-present-big)
                  (org-display-inline-images)
                  (org-present-hide-cursor)
                  (org-present-read-only)
                  (set-face-attribute 'org-level-1 nil :height 2.0)
                  (set-frame-parameter nil 'internal-border-width 20)
                  (setq mode-line-format nil)
                  ))
      (add-hook 'org-present-mode-quit-hook
                (lambda ()
                  (turn-on-evil-mode)
                  (org-present-small)
                  (org-present-show-cursor)
                  (org-present-read-write)
                  (set-face-attribute 'org-level-1 nil :height 1.3)
                  (set-frame-parameter nil 'internal-border-width 2)
                  (setq mode-line-format (eval (car (get 'mode-line-format 'standard-value))))
                  ))))
  )

;; ---------------------------
;; [ Dashboard ]
;; ---------------------------

(use-package page-break-lines)

(use-package dashboard
;; (use-package dashboard :load-path "my_packages/dashboard"
  :after page-break-lines
  :config
  (setq dashboard-startup-banner 'official)
  ;; (setq dashboard-startup-banner 1)
  ;; (setq dashboard-banner-logo-title "[ W E L C O M E   T O   E M A C S ]")
  (setq dashboard-banner-logo-title nil)

  (setq dashboard-center-content t)
  (set-face-attribute 'dashboard-text-banner-face nil :foreground "#FF0BAF" :weight 'bold :slant 'italic)

  (add-to-list 'dashboard-items '(custom) t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . t)))

  (defun dashboard-insert-custom (x) (insert (concat "Started in " (emacs-init-time) "")))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)
  (dashboard-setup-startup-hook))

;; ---------------------------
;; [ Themes ]
;; ---------------------------

(use-package dracula-theme :defer t)
(use-package doom-themes :defer t)
(use-package spacemacs-theme :defer t)
(use-package solarized-theme :defer t)
(use-package all-the-icons :defer t)

(setq my-theme 'spacemacs-light)
;; (setq my-theme 'dracula)

;; If emacs started as a deamon, wait until frame exists to apply theme, otherwise apply now
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme my-theme t))))
  (load-theme my-theme t))

;; Custom theming
(set-face-attribute 'helm-source-header nil :height 250)

;; minimalistic, but nice mode line
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-slant-function 'moody-slant-apple-rgb)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (setq powerline-default-separator 'arrow)
;;   ;; (setq powerline-default-separator 'wave)
;;   ;; (setq powerline-default-separator 'utf-8)
;;   ;; (setq powerline-height 16)
;;   (setq powerline-height 24)
;;   ;; (setq powerline-text-scale-factor 1.0)
;;   (setq powerline-image-apple-rgb t)
;;   ;; (spaceline-emacs-theme)
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   (setq evil-normal-state-tag "N")
;;   (setq evil-insert-state-tag "I")
;;   (setq evil-motion-state-tag "M")
;;   (setq evil-emacs-state-tag "E")
;;   (setq evil-visual-state-tag "V")
;;   (spaceline-spacemacs-theme)
;;   (spaceline-helm-mode))

;; -- optimizations
;; resets garbage collection tresholds to default levels
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000
                gc-cons-percentage 0.1)))
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist tmp--file-name-handler-alist)))
