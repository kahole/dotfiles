;; -- optimizations
;; set garbage-collection treshold to a very large number for the purpose of loading init.el
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; ---------------------------
;; [ Package system ]
;; ---------------------------

;; (require 'package)
(when (version< emacs-version "27.0")
    (package-initialize))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-archive-priorities '(("melpa-stable" . 15) ("gnu" . 10) ("org" . 5) ("melpa" . 0)))

;; for portability to emacs instances without use-package already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-always-ensure t) ; auto install packages

;; ---------------------------
;; [ UI preferences ]
;; ---------------------------

(defun ui-config ()
  (add-to-list 'default-frame-alist '(font . "Menlo 14"))

  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; mac window options
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; options (light|dark)

  (setq ring-bell-function 'ignore)
  (setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  
  (show-paren-mode t) ; show matching parenthesis
  (column-number-mode t)

  ;; hmmm
  (add-hook 'prog-mode-hook #'hl-line-mode) ; highlight current line in modes that contain "code"
  (add-hook 'prog-mode-hook #'display-line-numbers-mode) ; native line numbers in modes that contain "code"

  ;; (pixel-scroll-mode)
  ;; (setq pixel-resolution-fine-flag t)
  ;; (setq pixel-dead-time 0)
  )(ui-config)

;; ---------------------------
;; [ General preferences ]
;; ---------------------------

(defun general-config ()

  ;; sane defaults
  (setq mode-require-final-newline nil)
  (setq-default indent-tabs-mode nil) ; spaces instead of tabs
  (setq scroll-step 1) ; navigate off-screen scroll one line at a time
  (setq inhibit-startup-message t) 

  (electric-pair-mode t) ; smart auto-closing of parens

  ;; files
  (setq load-prefer-newer t) ; prefer loading newer versions of packages and elisp files
  (setq vc-follow-symlinks t) ; follow symlinks for git-controlled files into their repos
  (global-auto-revert-mode) ; reload buffer when file changed on disk
  ;; (setq create-lockfiles nil)

  ;; file backups
  (setq
   backup-by-copying t ; dont clobber symlinks
   backup-directory-alist
   '((".*" . "~/.emacs.d/saves/")) ; dont litter the fs tree
   delete-old-versions t
   kept-new-versions 4
   kept-old-versions 2
   version-control t) ; use versioned backups

  (setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory)) ; make emacs put all the custom-settings noise in this file, and then never load it
  ;; (load custom-file)

  (global-set-key (kbd "s-'") 'other-frame)
  (global-set-key (kbd "s-f") 'select-frame-by-name)
  (global-set-key (kbd "s-+") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)

  ;; Trust self signed localhost
  ;; /Users/khol/.private/localhost.pem
  ;;(add-to-list 'gnutls-trustfiles "/Users/khol/.private/localhost.pem")
  ;;(add-to-list 'gnutls-trustfiles "/Users/khol/.private/localhost2.pem")


   ;; mac meta key
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil)

  ;; load path from shell (on mac)
  (if (eq system-type 'darwin)
      (use-package exec-path-from-shell :config (exec-path-from-shell-initialize)))
      ;; manually setting the path.. a lot faster than ^
      ;; (progn
      ;;  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/opt/python/libexec/bin:/Users/khol/.nvm/versions/node/v12.8.0/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:/Library/TeX/texbin:/usr/local/share/dotnet:~/.dotnet/tools"))
      ;;  (setq exec-path (append exec-path (split-string (getenv "PATH") ":")))))


  )(general-config)

;; ---------------------------
;; [ Misc. packages ]
;; ---------------------------

(use-package diminish)
(use-package eldoc :diminish)
(use-package undo-tree :diminish)
;; (use-package page-break-lines
;;   :diminish
;;   :config
;;   (global-page-break-lines-mode))

(use-package magit
  :bind ("C-x g" . magit)
  :config
  (setq magit-diff-hide-trailing-cr-characters t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package projectile
  :config (projectile-mode))

(use-package helm
  :diminish
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x y" . helm-show-kill-ring)
  ("C-x k" . helm-buffers-list)
  ("C-x b" . helm-mini)
  ("C-x C-b" . helm-mini)
  ("C-x i" . helm-imenu)
  ("C-x p" . helm-projectile)
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-M-x-fuzzy-match t)
  ;; For tramp / ssh, makes helm skip checking if files still exists on remote (very painful on slow ssh connections)
  (setq helm-buffer-skip-remote-checking t)
  (helm-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "magit*")
  (helm-autoresize-mode)

  (set-face-attribute 'helm-source-header nil :height 250) ;; big headers in helm

  (use-package helm-projectile
    :after (projectile)
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (setq projectile-switch-project-action 'helm-projectile)))

(use-package company
  ;; :defer t
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package which-key
  :diminish
  :config (which-key-mode))

;; (use-package persp-mode
;;   :bind
;;   ("C-x -" . persp-frame-switch)
;;   ("C-x /" . persp-frame-switch)
;;   :init
;;   (persp-mode t))


(use-package helm-spotify-plus :commands helm-spotify-plus)

(use-package restclient
  ;; :commands restclient-mode
  :mode ("\\.http$" . restclient-mode))

;; ---------------------------
;; [ Evil ]
;; ---------------------------

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode)
  ;; initial evil state for these modes
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'special-mode 'motion)
  ;; (evil-set-initial-state 'term-mode 'emacs)
  ;; (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  ;; bindings
  (global-set-key (kbd "M-j") (lambda() (interactive) (evil-scroll-line-down 3)))
  ;; bind-key* kraftigere, for å fikse M-j scrolling i C++ mode
  (bind-key* "M-j" (lambda() (interactive) (evil-scroll-line-down 3)))
  (global-set-key (kbd "M-k") (lambda() (interactive) (evil-scroll-line-up 3)))
  (bind-key* "M-k" (lambda() (interactive) (evil-scroll-line-up 3)))
  (define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  
  (define-key evil-normal-state-map (kbd "J") 'evil-forward-section-begin)
  (define-key evil-normal-state-map (kbd "K") 'evil-backward-section-begin)

  (use-package evil-commentary
    :diminish
    :config
    (evil-commentary-mode))

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  ;; (use-package ace-jump-mode
  ;;   :config
  ;;   (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
  ;;   (define-key evil-motion-state-map (kbd "SPC") 'ace-jump-mode))
  )

;; ---------------------------
;; [ Org-Mode ]
;; ---------------------------

(use-package org
  ;; :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-hierarchical-todo-statistics nil)
  ;; (setq org-cycle-separator-lines -2)
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (setq org-src-tab-acts-natively t)

  (setq org-agenda-files (list "~/todo.org")) 

  ;; org babel
  (setq org-babel-python-command "python3")

  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t) (js . t) (dot . t)))

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; org-tree-slide, different style of presentation than org-present
  (use-package org-tree-slide
    :commands org-tree-slide-mode
    :config
    (add-hook 'org-tree-slide-mode-hook
              (lambda ()
                (org-tree-slide-slide-in-effect-toggle)
                (org-tree-slide-display-header-toggle)
                )))

  ;; org-present
  (use-package org-present
    :commands org-present
    :config
    (progn
      (add-hook 'org-present-mode-hook
                (lambda ()
                  (turn-off-evil-mode)
                  (org-present-big)
                  (global-set-key (kbd "C-<") (lambda () (interactive)
                                                (turn-on-evil-mode)
                                                (org-present-show-cursor)
                                                (org-present-read-write)))
                  (global-set-key (kbd "C->") (lambda () (interactive)
                                                (org-present-quit)
                                                (org-babel-tangle)
                                                (org-present)
                                                (text-scale-decrease 4)))
                  (org-display-inline-images)
                  (org-present-hide-cursor)
                  (org-present-read-only)
                  (set-face-attribute 'org-level-1 nil :height 2.0)
                  (set-frame-parameter nil 'internal-border-width 20)
                  (setq mode-line-format nil)))
      (add-hook 'org-present-mode-quit-hook
                (lambda ()
                  (turn-on-evil-mode)
                  (org-present-small)
                  (org-present-show-cursor)
                  (org-present-read-write)
                  (set-face-attribute 'org-level-1 nil :height 1.3)
                  (set-frame-parameter nil 'internal-border-width 2)
                  (setq mode-line-format (eval (car (get 'mode-line-format 'standard-value))))))))
  )

;; ---------------------------
;; [ Minimalist dashboard ]
;; ---------------------------

(setq initial-scratch-message "
;; ######## ##     ##    ###     ######   ######
;; ##       ###   ###   ## ##   ##    ## ##    ##
;; ##       #### ####  ##   ##  ##       ##
;; ######   ## ### ## ##     ## ##        ######
;; ##       ##     ## ######### ##             ##
;; ##       ##     ## ##     ## ##    ## ##    ##
;; ######## ##     ## ##     ##  ######   ######

;; ")

(setq initial-scratch-message (concat initial-scratch-message "Started in " (emacs-init-time) "\n\n"))

(add-hook 'emacs-startup-hook
          (lambda ()
              (insert-button ";;  - elkjop/todo.org" 'follow-link t 'action (lambda (x) (find-file "~/elkjop/todo.org")))
              (insert-button "\n;;  - todo.org" 'follow-link t 'action (lambda (x) (find-file "~/todo.org")))
              (insert-button "\n;;  - plan.org" 'follow-link t 'action (lambda (x) (find-file "~/netlight/utviklingsplan/plan.org")))
              (insert-button "\n;;  - init.el" 'follow-link t 'action (lambda (x) (find-file user-init-file)))
              (text-scale-set 3)))

;; ---------------------------
;; [ Themes ]
;; ---------------------------

(use-package dracula-theme :defer t)
(use-package doom-themes :defer t)
(use-package spacemacs-theme :defer t)
(use-package gruvbox-theme :defer t)
;; (use-package all-the-icons :defer t)

;; (setq my-theme nil)
;; (setq my-theme 'spacemacs-light)
;; (setq my-theme 'doom-vibrant)
(setq my-theme 'dracula)
;; (setq my-theme 'gruvbox-dark-hard)

;; If emacs started as a deamon, wait until frame exists to apply theme, otherwise apply now
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme my-theme t))))
  (when my-theme (load-theme my-theme t)))

;; ---------------------------
;; [ Languages ]
;; ---------------------------

;; [ Language server (LSP) ]

;; (use-package lsp-mode
;;   :commands lsp
;;   :init
;;   (use-package company-lsp :commands company-lsp)
;;   (setq lsp-enable-snippet nil)
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-prefer-flymake :none))

;; (use-package lsp-ui :commands lsp-ui-mode)

;; (use-package yasnippet
;;   :commands yas-minor-mode
;;   :config
;;   (use-package yasnippet-snippets
;;     :config (yas-reload-all)))

;; (use-package eglot
;;   :commands eglot-ensure
;;   :config
;;   (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio"))))

;; Emacs 27 support React JSX syntax out of the box
(defun my-js-mode-hook ()
  ;; (use-package npm-mode)
  ;; (npm-mode)
  ;; (use-package javascript-eslint)
  ;; (use-package prettier-js
  ;;   :config (prettier-js-mode))
  ;; (use-package json-mode)
  ;; (lsp)
  ;; (lsp-ui-mode)
  (setq js-indent-level 2))
  ;; (yas-minor-mode))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; [ Misc. languages ]

;; Lisp
(add-hook 'elisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)

;; (use-package haskell-mode
;;   :mode ("\\.hi\\'" "\\.hs\\'"))

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp")))) ;"--args"))))

(use-package csharp-mode
  :mode ("\\.cs\\'"))

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'"))

(use-package go-mode
  :mode ("\\.go\\'"))

(use-package markdown-mode
  :mode "\\.md$")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package vterm
  ;; :load-path "~/.emacs.d/custom_packages/emacs-libvterm"
  :init
  (use-package hide-mode-line)
  (add-hook 'vterm-mode-hook #'hide-mode-line-mode)
  :config
  (setq vterm-shell "/bin/zsh")

  
  (defun vterm-panel ()
    "Toggle a terminal in a small pane at the bottom of the screen."
    (interactive)
    (let ((panel-buffer-name "vterm panel"))
      (let ((panel-window (get-buffer-window panel-buffer-name)))
        (if panel-window (delete-window panel-window)
          (select-window (split-window-below -14))
          (unless (get-buffer panel-buffer-name)
            (let ((buffer (generate-new-buffer panel-buffer-name)))
              (with-current-buffer buffer (vterm-mode))))
          (switch-to-buffer panel-buffer-name)))))

  (global-set-key (kbd "C-x t") 'vterm-panel))

;; -- optimizations
;; resets garbage collection tresholds to default levels
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000
                gc-cons-percentage 0.1)))
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist tmp--file-name-handler-alist)))
