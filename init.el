;;; init.el --- A basic Emacs config

;;; Commentary:
;; Fine tuned for development in Python, C/C++ and FORTRAN

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Backup all files here

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; The single most important section of this file (USE EVIL)

(require 'evil)
(evil-mode 1)

;; Some eye candy stuff (Sehr Wichtig!)

(setq custom-safe-themes t)
(load-theme 'gruvbox-dark-hard t)
(if (display-graphic-p)
    nil
(set-face-attribute 'default nil :background "unspecified-bg"))
(xterm-mouse-mode 1)
(show-paren-mode 1)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(setq inhibit-startup-screen t)

;; Spell checking with flyspell

(require 'flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(flyspell-mode +1)

;; Syntax checking with flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; FORTRAN specific stuff

(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))

;; Python stuff

(elpy-enable)

;; C stuff

(setq c-basic-offset 6)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

;; Snippets

(require 'yasnippet)
(yas-global-mode 1)

;; Latex

(setq TeX-PDF-mode t)

(provide 'init.el)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex yasnippet-snippets ## elpy gruvbox-theme flycheck evil alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
