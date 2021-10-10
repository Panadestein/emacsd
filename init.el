;;; init.el --- A basic Emacs configuration

;;; Commentary:
;; Fine tuned for development in Python, C/C++ and FORTRAN

;;; Code:

;; Emacs internal options

(setq-default initial-scratch-message ";; Welcome Panadestein!!"
	      ring-bell-function #'ignore
	      user-full-name "Ramón L. Panadés-Barrueta, PhD"
              inhibit-startup-screen t
	      custom-safe-themes t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(fringe-mode '(0 . 0))

;; Only highlight programming and text buffers

(use-package hl-line
  :config
  (global-hl-line-mode +1)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

;; Prevent custom from messing with my config file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Never use scroll bar

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; Line numbers

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (setq display-line-numbers 'relative))

;; Start Emacs maximized in X

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Backup directory

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; The single most important section of this file (Vim workflow!)

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

;; Emacs’s native undo system is a mistake

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;; Fonts (emacsclient requires the add-to-list)

(add-to-list 'default-frame-alist '(font . "Fira Code-18"))

;; Themes

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package gruvbox-theme
  :ensure t
  :disabled
  :init (load-theme 'gruvbox-dark-soft t))
  
(use-package blackboard-theme
  :ensure t
  :disabled
  :init (load-theme 'blackboard t))

;; Highlight numbers

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

;; Modify face so Emacs is always transparent in terminal

(face-spec-set 'default
  '((((type tty)) :background "unspecified-bg")))

;; Fancy mode line

(use-package all-the-icons
  ;; Needs a manual `M-x all-the-icons-install-fonts`
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-line-numbers-style 'relative)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t))

;; Practical settings to make Emacs more ergonomic
;; I avoid enabling the xterm-mouse-mode option, as it
;; introduces an unwanted behaviour in the clipboard

(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

;; Make ESC close prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Completion in Emacs with ivy

(use-package ivy
  :ensure t
  :diminish
  :bind
  (("C-s" . swiper))
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package swiper :ensure t)

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
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 3
	company-selection-wrap-around t
	company-tooltip-limit 20
	company-tooltip-minimum-width 15
	company-tooltip-align-annotations t))

(use-package lsp-mode
  :ensure t
  :defer t)

;; Spell checking with flyspell

(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
   (org-mode . turn-on-flyspell))
  :config
  (flyspell-mode +1))

;; Syntax checking with flycheck

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

;; Snippets

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Terminals, vterm config from Ergus

(use-package vterm
  :ensure t
  :preface
  (defun my/vterm-mode-hook ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))
  :hook
  ((vterm-mode . my/vterm-mode-hook))
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
  :ensure t
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
  :disabled
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
  (setq neo-smart-open t))

;; SSH with TRAMP

(use-package tramp
  :ensure t)

;; Makefile stuff

(use-package make-mode)

;; FORTRAN stuff

(use-package f90-mode
  :mode ("\\.f90\\'")
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

(use-package python
  :ensure t
  :config
  ;; Use IPython when available
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))
  (defun run-buffer ()
    "This is not the Emacs way, I know"
    (interactive)
    (shell-command (concat "python " buffer-file-name)))
  (global-set-key (kbd "<f9>") 'run-buffer))

(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package anaconda-mode
  :ensure t
  :bind ("C-c C-d" . anaconda-mode-show-doc))

;; Perl stuff

(use-package cperl-mode
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

(use-package org-superstar  ;; Fancy bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
  
(setq org-startup-indented t)           ;; Indent according to section
(setq org-startup-with-inline-images t) ;; Display images in-buffer by default

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
                   web-mode-type-face)
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

;; YAML stuff

(use-package yaml-mode
  :ensure t
  :mode "\.ya?ml\'")

;; Markdown stuff

(use-package markdown-mode
  :ensure t
  :init
  (setq-default markdown-hide-markup t))

;; reStructuredText stuff

(use-package rst
  :mode ("\\.rst\\'" . rst-mode)
  :bind (:map rst-mode-map
              ("M-a" . rst-backward-section)
              ("M-e" . rst-forward-section))
  :init
  (setq rst-indent-width 2))

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
