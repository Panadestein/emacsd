;;; init.el --- A basic Emacs configuration

;;; Commentary:
;; Fine tuned for development in Python, C/C++ and FORTRAN

;;; Code:

;; Emacs internal options (credits here to Jimmy Aguiar Mena)

(setq-default initial-scratch-message ";; Welcome Panadestein!!"
	      ring-bell-function #'ignore
	      user-full-name "Ramón L. Panadés-Barrueta, PhD"
              inhibit-startup-screen t
	      custom-safe-themes t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(show-paren-mode 1)

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
  :defer t
  :init (load-theme 'spacemacs-dark t))

(if (display-graphic-p)
    nil
  (set-face-attribute 'default nil :background "unspecified-bg"))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Practical settings to make Emacs more ergonomic
;; Avoid enabling the xterm-mouse-mode option, as it
;; introduces an unwanted behaviour in the clipboard

(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

;; Tabs-bar-mode (cannot use without side effects in Evil)
;; C-x t f "filename" to open a new tab
;; C-x t 0 to close current tab

(use-package tab-bar
  :ensure nil
  :bind (("C-<right>" . tab-next)))

;; Command's information with which-key

(use-package which-key
  :defer t
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
  :ensure t
  :disabled t)

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
   (org-mode . turn-on-flyspell)
   (LaTeX-mode-hook  . turn-on-flyspell))
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
  :after yasnippet)

;; SSH with TRAMP

(use-package tramp
  :defer t)

;; FORTRAN stuff

(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))

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

;; Python stuff

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; Raku stuff

(use-package raku-mode
  :ensure t
  :defer t
  :mode "\\.raku\\'")

;; Latex

(use-package tex
  :ensure auctex
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-save-query nil))

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
  :hook
  ((web-mode . company-mode)
   (web-mode . webmd-hooks)))

(defun webmd-hooks ()
  "Some hooks for web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "red"))

(use-package js-mode :ensure nil
  :mode ("\\.js\\'"))

(use-package company-web
  :preface
  (defun my/company-web ()
    (add-to-list (make-local-variable 'company-backends) '(company-web-html)))
  :hook (web-mode . my/company-web))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-lsp gnuplot powerline xclip spacemacs-theme auctex yasnippet-snippets ## elpy gruvbox-theme flycheck evil alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
