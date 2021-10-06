;;; custom.el --- Reduce entropy of init.el

;;; Commentary:
;; Here will come all the mess from customize interface

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-pyright python doom-modeline powerline-evil evil-matchit yaml-mode cperl-mode make-mode flyspell neotree vterm-toggle vterm tramp-term projectile tabbar web-mode raku-mode py-autopep8 jedi ranger yasnippet company highlight-numbers makefile-mode htmlize color-theme-sanityinc-tomorrow magit flycheck-haskell haskell-mode which-key irp-mode shell-pop lsp-mode emmet-mode evil-mc company-lsp gnuplot powerline xclip spacemacs-theme auctex yasnippet-snippets ## elpy gruvbox-theme flycheck evil alect-themes)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-5-face ((t (:foreground "red" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "brightcyan"))))
 '(org-table ((t (:foreground "color-69")))))

(provide 'custom.el)
;;; custom.el ends here
