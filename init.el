;;; init.el --- A basic Emacs configuration

;;; Commentary:
;; Fine tuned for development in Python, C/C++ and FORTRAN

;;; Code:

;; Emacs internal options, credits here to Jimmy Aguiar Mena (Ergus)

(setq-default initial-scratch-message ";; Welcome Panadestein!!"
	      ring-bell-function #'ignore
	      user-full-name "Ramón L. Panadés-Barrueta, PhD"
              inhibit-startup-screen t
	      custom-safe-themes t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

;; Keybindings

(global-set-key (kbd "<f9>") 'ranger)

;; Never use scroll bar

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; Line numbers

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers 'relative))

;; Start Emacs maximized in X

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vterm-toggle vterm tramp-term projectile tabbar web-mode raku-mode py-autopep8 jedi ranger yasnippet company highlight-numbers makefile-mode htmlize color-theme-sanityinc-tomorrow magit flycheck-haskell haskell-mode which-key irp-mode shell-pop lsp-mode emmet-mode evil-mc company-lsp gnuplot powerline xclip spacemacs-theme auctex yasnippet-snippets ## elpy gruvbox-theme flycheck evil alect-themes))))

;; Add the GNU ELPA and MELPA archives, and then ensure use-package
;; Allows for using this config in any machine

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
       ("melpa" . "https://melpa.org/packages/"))
      package-quickstart t)

(unless (and (fboundp 'package-installed-p)
       (package-installed-p 'use-package))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

;; Autoupdate packages

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Backup all files here

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; The single most important section of this file (Vim workflow!)

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1))

;; Some eye candy stuff (Sehr Wichtig!)

(use-package spacemacs-theme
  :ensure t
  :disabled
  :init (load-theme 'spacemacs-dark t))

(use-package gruvbox-theme
  :ensure t
  :disabled
  :init (load-theme 'gruvbox-dark-soft t))
  
(use-package afternoon-theme
  :ensure t
  :disabled
  :init (load-theme 'afternoon t))

(use-package blackboard-theme
  :ensure t
  :init (load-theme 'blackboard t))

(use-package dakrone-theme
  :ensure t
  :disabled
  :init (load-theme 'dakrone t))

(use-package ample-theme
  :ensure t
  :disabled
  :init (load-theme 'ample-flat t))

(use-package monokai-theme
  :ensure t
  :disabled
  :init (load-theme 'monokai t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :disabled
  :init (load-theme 'sanityinc-tomorrow-eighties t))

;; Highlight numbers

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

;; Modify face so Emacs is always transparent in terminal

(face-spec-set 'default
  '((((type tty)) :background "unspecified-bg")))

;; Fancy mode line

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Practical settings to make Emacs more ergonomic
;; Avoid enabling the xterm-mouse-mode option, as it
;; introduces an unwanted behaviour in the clipboard

(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

;; Multiple cursors support
;; C-n make and go to next
;; C-p make and go to prev
;; g r q stop all cursors

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

;; Tabs-bar-mode (cannot use without side effects in Evil)
;; C-x t f "filename" to open a new tab
;; C-x t 0 to close current tab

(use-package tabbar
  :ensure t
  :bind (("C-<right>" . tabbar-forward)))

;; Command's information with which-key

(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-secondary-delay 0.01)
  (which-key-dont-use-unicode t)
  :config
  (which-key-mode t))

;; Completion with company and language server protocol

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :ensure t)

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

;; Spell checking with flyspell

(use-package flyspell
  :ensure t
  :hook
  ((prog-mode . flyspell-prog-mode)
   (org-mode . turn-on-flyspell))
  :config
  (flyspell-mode +1))

;; Syntax checking with flycheck

(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode))

;; Snippets

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Terminals, vterm config from Ergus

(use-package shell-pop
  :ensure t
  :disabled
  :bind (("C-c p" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*"
				     (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package vterm
  :ensure t
  :preface
  (defun my/vterm-mode-hook ()
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))
  :hook
  ((vterm-mode . my/vterm-mode-hook)
   (vterm-mode . (lambda () (setq evil-default-state 'emacs))))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  :init
  (which-key-add-key-based-replacements "C-c t" "term")
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
	       '("find-file-other-window" find-file-other-window)))

(use-package vterm-toggle
  :bind (("C-c p" . vterm-toggle-cd)
	 :map vterm-mode-map
	 (("<C-return>" . vterm-toggle-insert-cd)
	  ("C-M-n" . vterm-toggle-forward)
	  ("C-M-p" . vterm-toggle-backward)))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-fullscreen-p nil)
  :config
  ;; Show at bottom
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _)
		   (with-current-buffer bufname
		     (equal major-mode 'vterm-mode)))
                 ;; (display-buffer-reuse-window display-buffer-at-bottom)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))


;; File browser

(use-package ranger
  :ensure t
  :config
  (setq ranger-preview-file t))

(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)

  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Every time when the neotree window is opened, let it find current file and jump to node
  (setq neo-smart-open t)

  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
  (setq projectile-switch-project-action 'neotree-projectile-action)

  ;; show hidden files
  (setq-default neo-show-hidden-files t))

;; SSH with TRAMP

(use-package tramp
  :ensure t)

;; Makefile stuff

(use-package make-mode
  :ensure t)

;; FORTRAN stuff

(use-package f90-mode
  :mode ("\\.f\\'" "\\.f90\\'")
  :hook
  (f90-mode . (lambda () (setq flycheck-gfortran-args "-ffree-form"))))

;; IRPF90 (see the derived mode in ~/.emacs.d/lib/irp-mode.el)

(use-package irp-mode
  :mode ("\\.irp.f\\'")
  :load-path "~/.emacs.d/lib")

;; C/C++ stuff

(setq c-basic-offset 6)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

;; Cuda stuff

   (use-package cuda-mode
       :mode "\\.cu\\'")

;; Julia stuff

(use-package julia-mode
  :mode "\\.jl\\'")

(use-package flycheck-julia
  :hook (julia-mode . flycheck-julia-setup))

;; Haskell stuff

(use-package haskell-mode
  :ensure t
  :custom
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-tags-on-save t))

(use-package flycheck-haskell
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

;; Python stuff

(setenv "PATH" (concat (expand-file-name "~/.local/bin:") (getenv "PATH")))

(use-package elpy
  :ensure t
  :init
  (setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
  (setq elpy-shell-starting-directory 'current-directory)
  (setq elpy-shell-echo-input nil)
  (setq elpy-rpc-python-command "python3")
  (elpy-enable)
  :config
  (add-hook 'elpy-mode-hook (lambda () (elpy-shell-toggle-dedicated-shell 1)))
  (add-to-list 'python-shell-completion-native-disabled-interpreters
	       "jupyter"))

(defvar jedi-config:use-system-python t)
(defun jedi-config:set-python-executable ()
  "Defines some variables to find Python executable."
       (make-local-variable 'jedi:server-command)
       (set 'jedi:server-command
	    (list (executable-find "python3")
		  (cadr default-jedi-server-command))))

(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi-config:setup-server-args)
  (when jedi-config:use-system-python
      (add-hook 'python-mode-hook
                 'jedi-config:set-python-executable))
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t
        jedi:environment-root "jedi"))

(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; Perl stuff

(use-package cperl-mode
  :ensure t
  :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))

;; Raku stuff

(use-package raku-mode
  :ensure t
  :defer t
  :mode "\\.raku\\'")

;; Latex

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t)))


;; Org-mode stuff

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages '((fortran . t)
			       (python . t)
			       (C . t)
			       (gnuplot . t)
			       (awk . t)
			       (latex . t)
			       (shell . t)))
  (setq org-preview-latex-default-process 'imagemagick))

;; Web stuff

(use-package php-mode
  :mode ("\\.php\\'"))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'" "\\.htm\\'" "\\.css\\'")
  :bind (("C-c C-v" . browse-url-of-buffer))
  :hook
  ((web-mode . company-mode)
   (web-mode . emmet-mode)
   (web-mode . (lambda () (flyspell-mode 1)))
   (web-mode . webmd-hooks)))

(defun web-mode-flyspefll-verify ()
  "Make flyspell behave correctly in web mode."
  (let ((f (get-text-property (- (point) 1) 'face)))
    (not (memq f '(web-mode-html-attr-value-face
                   web-mode-html-tag-face
                   web-mode-html-attr-name-face
                   web-mode-doctype-face
                   web-mode-keyword-face
                   web-mode-function-name-face
                   web-mode-variable-name-face
                   web-mode-css-property-name-face
                   web-mode-css-selector-face
                   web-mode-css-color-face
                   web-mode-type-face
                   )
               ))))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspefll-verify)

(use-package emmet-mode
  :ensure t)

(defun webmd-hooks ()
  "Some hooks for web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "red"))

(use-package js-mode :ensure nil
  :mode ("\\.js\\'"))

;; JSON stuff

(use-package json-mode
  :mode "\\.json\\'")

(use-package flymake-json
  :hook (json-mode . flymake-json-load))

;; Cmake stuff

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\(.in\)?\\'")
  :config
  (add-to-list 'company-backends 'company-cmake))

(use-package eldoc-cmake
  :after company
  :hook (cmake-mode . eldoc-cmake-enable))

;; Git stuff

(use-package magit
  :ensure t)

(provide 'init.el)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:foreground "brightcyan"))))
 '(org-table ((t (:foreground "color-69")))))
