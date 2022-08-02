(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
             :custom (straight-use-package-by-default t))

(use-package emacs
  :straight nil
  :config
  ;; Emacs internal options
  (setq-default initial-scratch-message ";; Welcome Panadestein!!"
                ring-bell-function #'ignore
                user-full-name "Ramón L. Panadés-Barrueta, PhD"
                inhibit-startup-screen t
                custom-safe-themes t)
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups"))))
  (if init-file-debug
    (setq warning-minimum-level :debug)
    (setq warning-minimum-level :emergency))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (show-paren-mode 1)
  (fringe-mode '(0 . 0))
  ;; Make ESC close prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; No scroll bar, always full screen
  (add-to-list 'default-frame-alist
               '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Font
  (add-to-list 'default-frame-alist '(font . "Fira Code-18"))
  ;; Terminal transparency
  (face-spec-set 'default
                 '((((type tty)) :background "unspecified-bg")))
  ;; Line numbers
  (when (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  ;; Remember line number
  (if (fboundp #'save-place-mode)
      (save-place-mode +1)
    (setq-default save-place t)))

(let
    ((customization-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p customization-file)
    (setq custom-file customization-file)
    (load custom-file 'noerror)))

(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode +1)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

(use-package evil
  :straight t
  :demand t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  ;; This is the cleanest solution for vterm
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package doom-themes
  :straight t
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
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package gruvbox-theme
  :straight t
  :disabled
  :init (load-theme 'gruvbox-dark-soft t))

(use-package blackboard-theme
  :straight t
  :disabled
  :init (load-theme 'blackboard t))

;; Highlight numbers

(use-package highlight-numbers
  :straight t
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package so-long
  :straight nil
  :hook
  (after-init-hook . global-so-long-mode))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package minimap
  :straight t
  :config
  (setq minimap-window-location 'right)
  (setq minimap-minimum-width 10)
  (setq minimap-dedicated-window t)
  (setq minimap-hide-cursor t)
  (setq minimap-hide-scroll-bar t)
  (setq minimap-hide-fringes t))

(use-package all-the-icons
  ;; Needs a manual `M-x all-the-icons-install-fonts`
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-line-numbers-style 'relative)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-major-mode-color-icon t))

(use-package evil-mc
  :straight t
  :config
  (global-evil-mc-mode 1))

(use-package tabbar
  :straight t
  :bind (("C-<right>" . tabbar-forward)))

(use-package htmlize
  :straight t)

(use-package ivy
  :straight t
  :diminish
  :bind
  (("C-s" . swiper))
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package swiper
  :straight t)

(use-package counsel
  :straight t
  :after ivy
  :hook
  (after-init . counsel-mode)
  :config (counsel-mode)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
  ("C-M-l" . counsel-imenu)
  ("C-x C-f" . counsel-find-file)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> f" . counsel-descbinds-function))

(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package ivy-rich
  :straight t
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators
                       (:width 4 :face error :align right))
                      (ivy-rich-switch-buffer-major-mode
                       (:width 12 :face warning))
                      (ivy-rich-switch-buffer-project
                       (:width 15 :face success))
                      (ivy-rich-switch-buffer-path
                       (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
                                            x (ivy-rich-minibuffer-width 0.3))))))))))

(use-package all-the-icons-ivy
  :straight t
  :demand t)

(use-package which-key
  :straight t
  :diminish
  :custom
  (which-key-idle-secondary-delay 0.01)
  (which-key-dont-use-unicode t)
  :config
  (which-key-mode t))

(use-package helpful
  :straight t
  :custom
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package gtags-mode
  :straight t
  :hook ((emacs-startup . gtags-mode)))

(use-package counsel-etags
  :straight t
  :disabled
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package company
  :straight t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-tooltip-limit 20
        company-tooltip-minimum-width 15
        company-tooltip-align-annotations t))

(use-package lsp-mode
  :straight t
  :init
  (defun my-lsp-hook ()                      
    "Do not use lsp-mode with tramp"         
    (unless (file-remote-p default-directory)
      (lsp)))                                
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" nil nil)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ;; Disable duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . my-lsp-hook)
   (f90-mode . my-lsp-hook)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0.5
        lsp-ui-doc-delay 5
        lsp-ui-sideline-ignore-duplicates t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode)

  (use-package lsp-treemacs
    :after treemacs lsp
    :straight t)

(use-package eglot
  :straight t
  :defer t)

(use-package flyspell
  :straight nil
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . turn-on-flyspell))
  :config
  (flyspell-mode +1))

(use-package langtool
  :straight t
  :init
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(python-mypy))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable))
  (setq flycheck-scheme-chicken-executable "csc")
  :hook
  (after-init . global-flycheck-mode))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package vterm
  :straight t
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
  (vterm-tramp-shells '(("ssh" "/bin/bash")))
  :init
  (which-key-add-key-based-replacements "C-c t" "term")
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" find-file-other-window)))

(use-package vterm-toggle
  :straight t
  :bind (("C-`" . vterm-toggle-cd)
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

(use-package eshell
  :straight nil
  :preface
  (defun my-eshell-prompt ()
    "A nice prompt"
    (concat
      "(" (user-login-name) " . "
      (car (reverse (split-string (eshell/pwd) "/"))) ") "))
  :hook
  ((eshell-mode . (lambda () 
                    (add-to-list 'eshell-visual-commands "ssh")
                    (add-to-list 'eshell-visual-commands "tail")
                    (add-to-list 'eshell-visual-commands "top")
                    (eshell/alias "e" "find-file-other-window $1")
                    (eshell/alias "d" "dired-other-window $1") 
                    (eshell/alias "mgu" "magit-diff-unstaged")
                    (eshell/alias "mg" "magit-diff-staged"))))
  :custom
  (eshell-banner-message (shell-command-to-string "figlet Eshell"))
  (eshell-history-size 10000)
  (eshell-hist-ignore-dups t)
  (eshell-buffer-maximum-lines 10000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prompt-regexp "^[^\)]*[\)] ")
  (eshell-prompt-function #'my-eshell-prompt)
  :config
  (setenv "PAGER" "cat"))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root nil)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("C-c p" . eshell-toggle))

(use-package ranger
  :straight t
  :disabled
  :config
  (setq ranger-preview-file t))

(use-package neotree
  :straight t
  :disabled
  :bind ("<f9>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)
  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))

(use-package treemacs
  :straight t
  :defer t
  :bind
  (:map global-map
	      ("<f8>" . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package all-the-icons-dired
  :straight t)

(use-package dired
  ;; TIP: use ( to hide file information
  :straight nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :config
  (evil-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :straight t)

(use-package pdf-tools
  :straight t
  ;:pin manual
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install))

(use-package tramp                                        
  :straight nil                                             
  :custom                                                 
  (tramp-default-method "ssh")                           
  :config
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp               
        (format "%s\\|%s"                  
                vc-ignore-dir-regexp       
                tramp-file-name-regexp))   
  (setq tramp-verbose 1))                  

(use-package tramp-sh                                     
  :straight nil                                             
  :config                                                 
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package slime
  :straight t
  :mode ("\\.lisp\\'" . lisp-mode)
  :init
  (setq slime-net-coding-system 'utf-8-unix
        inferior-lisp-program "sbcl")
  (add-to-list 'slime-contribs 'slime-fancy)
  (add-to-list 'slime-contribs 'slime-repl))

(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(chez)))

(use-package geiser-chez
  :straight t
  :init
  (setq geiser-chez-binary "scheme"))

(use-package racket-mode
  :straight t)

(use-package hy-mode
  :straight t
  :mode "\\.hy\\'"
  :commands (hy-mode org-babel-execute:hy)
  :interpreter "hy"
  :hook
  (hy-mode . company-mode)
  (hy-mode . (lambda () (lispy-mode 1)))
  :config
  (add-hook 'hy-mode-hook #'paredit-mode)
  (add-hook 'hy-mode-hook #'rainbow-delimiters-mode))

(use-package macrostep
  :straight t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package f90-mode
  :straight nil
  :mode ("\\.f90\\'")
  :hook
  (f90-mode . (lambda () (setq flycheck-gfortran-args "-ffree-form"))))

(use-package irp-mode
  :straight nil
  :mode ("\\.irp.f\\'")
  :load-path "~/.emacs.d/lib")

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq c-basic-offset 6)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux"))))

(use-package python
  :straight nil
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

(use-package pyvenv
  :straight t
  :demand t
  :config
  (setq pyvenv-workon "emacs")
  (pyvenv-tracking-mode 1))

(use-package py-autopep8
  :straight t
  :hook ((python-mode) . py-autopep8-mode))

(use-package jupyter
  :straight t)

(use-package cuda-mode
    :mode "\\.cu\\'")

(use-package julia-mode
  :mode "\\.jl\\'")

(use-package flycheck-julia
  :hook (julia-mode . flycheck-julia-setup))

(use-package haskell-mode
  :straight t
  :custom
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-tags-on-save t))

(use-package flycheck-haskell
  :straight t
  :config
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

(use-package make-mode
   :straight nil)

(use-package cperl-mode
  :straight nil
  :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))

(use-package raku-mode
  :straight t
  :defer t
  :mode "\\.raku\\'")

(use-package auctex
  :straight t
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

(use-package org
  :straight nil
  :hook
  (org-mode . (lambda () (add-hook 'after-save-hook #'org-babel-tangle :append :local)))
  :config
  (require 'ox-beamer)
  (require 'ol-bibtex)
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages '((fortran . t)
                               (python . t)
                               (jupyter . t)
                               (scheme . t)
                               (lisp . t)
                               (emacs-lisp . t)
                               (C . t)
                               (org . t)
                               (gnuplot . t)
                               (awk . t)
                               (latex . t)
                               (shell . t)))
  (setq org-latex-pdf-process '("latexmk -shell-escape -pdf -outdir=%o %f"))
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-file-apps '(("\\.pdf\\'" . "evince %s")))
  (setq org-startup-indented t)
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted")))
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "BROKEN(b)" "RUNNING(r)" "VERIFY(v)" "URGENT(u)" "PARTIAL(p)"
                    "|" "DONE(d)" "OPTIONAL(o)" "DELEGATED(e)" "IRRELEVANT(i)")))
  (setq org-todo-keyword-faces
              '(("BROKEN" . "red") ("RUNNING" . "yellow")
                ("VERIFY" . "light goldenrod") ("URGENT" . "orange") ("PARTIAL" . "burlywood")
                ("OPTIONAL" . "green") ("IRRELEVANT" . "LightBlue1")
                ("DELEGATED" . "aquamarine3")))
  (plist-put org-format-latex-options :scale 3.0)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package ox-reveal
  :straight t
  :after org
  :custom
  (org-reveal-root "file:///home/loren/bin/reveal.js")
  :config
  (setq org-reveal-mathjax t))

(use-package ob-racket
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	      #'ob-racket-raco-make-runtime-library)
  :straight (ob-racket
	       :type git :host github :repo "hasu/emacs-ob-racket"
	       :files ("*.el" "*.rkt")))

(use-package php-mode
  :mode ("\\.php\\'"))

(use-package web-mode
  :straight t
  :defer t
  :mode ("\\.html\\'" "\\.htm\\'" "\\.css\\'")
  :bind (("C-c C-v" . browse-url-of-buffer))
  :init
  (defun webmd-hooks-mine ()
    "Some hooks for web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-current-element-highlight t)
    (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "red"))

  (defun web-mode-flyspell-verify ()
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
  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)
  :hook
  ((web-mode . company-mode)
   (web-mode . emmet-mode)
   (web-mode . (lambda () (flyspell-mode 1)))
   (web-mode . webmd-hooks-mine)))

(use-package emmet-mode
  :straight t)

(use-package js-mode :straight nil
  :mode ("\\.js\\'"))

(use-package simple-httpd
  :straight t)

(use-package yaml-mode
  :straight t
  :mode "\.ya?ml\'")

(use-package markdown-mode
  :straight t
  :init
  (setq-default markdown-hide-markup t))

(use-package rst
  :straight nil
  :mode ("\\.rst\\'" . rst-mode)
  :bind (:map rst-mode-map
              ("M-a" . rst-backward-section)
              ("M-e" . rst-forward-section))
  :init
  (setq rst-indent-width 2))

(use-package lua-mode
  :straight t)

(use-package json-mode
  :mode "\\.json\\'")

(use-package flymake-json
  :hook (json-mode . flymake-json-load))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\(.in\)?\\'")
  :config
  (add-to-list 'company-backends 'company-cmake))

(use-package eldoc-cmake
  :after company
  :hook (cmake-mode . eldoc-cmake-enable))

(use-package cmake-font-lock
  :straight t
  :preface
  (defun my-cmake-font-lock ()
    (let ((auto-refresh-defaults (boundp 'font-lock-keywords)))
      (cmake-font-lock-activate)
      (when auto-refresh-defaults
  (font-lock-refresh-defaults))))
  :init
  (add-hook 'cmake-mode-hook #'my-cmake-font-lock))

(use-package magit
  :straight t)

(use-package git-modes
  :straight t
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
   ("\\.gitconfig\\'" . gitconfig-mode)
   ("\\.gitignore\\'" . gitignore-mode)))

(use-package vimrc-mode
  :straight t)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")
