;;; package --- Build Org website

;;; Commentary:
;; Build website from Org-mode source files

;;; Code:

;; Set a package installation directory to avoid conflicts

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install htmlize to get color highlighting

(package-install 'htmlize)

(require 'ox-publish)
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

;; Define the project to be published

(setq org-publish-project-alist
      (list
       (list "emacsd"
	     :recursive t
	     :base-directory "./content"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html
	     :with-author t
	     :with-creator nil
	     :with-toc t
	     :setion-numbers nil
	     :time-stamp-file nil)))

;; Generate site

(org-publish-all t)

(message "Build completed")

(provide 'build-site)
;;; build-site.el ends here
