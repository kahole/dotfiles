;(require 'package)
;(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 15)
        ("gnu" . 10)
        ("org" . 5)
        ("melpa" . 0)))

; For portability to emacs versions without use-package already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

; Auto install packages
(setq use-package-always-ensure t)

; Auto update
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

; ---------------------------
; [ Packages ]
; ---------------------------

(use-package magit)
(use-package ace-jump-mode)

(use-package markdown-mode)
(use-package haskell-mode)
(use-package go-mode)

(use-package doom-themes)
(use-package spacemacs-theme :defer t :init (load-theme 'spacemacs-light t))
(use-package moe-theme)
(use-package solarized-theme)
;(use-package all-the-icons)
(use-package all-the-icons-ivy)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

; ---------------------------
; [ General settings ]
; ---------------------------

(add-to-list 'default-frame-alist '(height . 66))
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
; Options (light|dark) dark theme use light, vice versa
(add-to-list 'default-frame-alist '(ns-appearance . light))

(tool-bar-mode -1)
(scroll-bar-mode -1)
; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
;(global-display-line-numbers-mode 1) ;native line numbers mode:
(ido-mode 1)
(set-frame-font "Menlo 14" nil t)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(pixel-scroll-mode t)
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

; ---------------------------
; [ LaTex ]
; ---------------------------

(setq latex-run-command "texi2dvi")

; ---------------------------
; [ Doc-View ]
; ---------------------------

(setq doc-view-resolution 800)
; Auto reload on file changed
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

; ---------------------------
; [ Dashboard ]
; ---------------------------

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "Welcome to Emacs yo!")

  (setq dashboard-items '((recents  . 6)
                          (bookmarks . 6)
                          ;(projects . 5)
                          (agenda . t)))

  (defun dashboard-insert-custom (x) (insert "hmmmmmmm"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t))

; ---------------------------
; [ Org-Mode ]
; ---------------------------

(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))

(setq org-hierarchical-todo-statistics nil)
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-cycle-separator-lines -2)

; org babel
(setq org-babel-python-command "python3")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (js . t) (dot . t))
 )

; truncate lines
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))

; org pretty bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

; org-present
(use-package org-present)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (turn-off-evil-mode)
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (turn-on-evil-mode)
                 (org-present-small)
                 (org-present-show-cursor)
                 (org-present-read-write)))))


; ---------------------------
; [ Evil ]
; ---------------------------

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-scroll-count 3))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

; ---------------------------
; [ Evil Key-Bindings ]
; ---------------------------

(global-set-key (kbd "M-j") (lambda() (interactive) (evil-scroll-line-down 3)))
(global-set-key (kbd "M-k") (lambda() (interactive) (evil-scroll-line-up 3)))

;(setq text-scale-mode-step 1)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

; Ace-jump-mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

; NeoTree
(define-key evil-normal-state-map "\C-n" 'neotree-toggle)
(define-key evil-normal-state-map "gt" 'next-buffer)
(define-key evil-normal-state-map "gT" 'previous-buffer)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)


; ---------------------------
; [ NeoTree ]
; ---------------------------
(use-package neotree
  :config
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


; ---------------------------
; [ Company ]
; ---------------------------

(use-package company
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

