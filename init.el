;;; init.el --- A basic Emacs configuration

;;; Commentary:
;; Fine tuned for development in Python, C/C++ and FORTRAN

;;; Code:

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

(setq custom-safe-themes t)

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

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(show-paren-mode 1)

;; Tabs-bar-mode (cannot use without side effects in Evil)
;; C-x t f "filename" to open a new tab
;; C-x t 0 to close current tab

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

;; FORTRAN stuff

(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))

;; C/C++ stuff

(setq c-basic-offset 6)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

;; Python stuff

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

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
