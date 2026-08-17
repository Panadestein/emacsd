;;; build-site.el --- Build the Org website -*- lexical-binding: t; -*-

;;; Commentary:
;; Export the website with bundled Org and a vendored htmlize dependency.

;;; Code:

(require 'ox-publish)

(defconst emacsd-site-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the emacsd website sources.")

(defconst emacsd-site-content-directory
  (expand-file-name "content" emacsd-site-root))

(defconst emacsd-site-output-directory
  (expand-file-name "public" emacsd-site-root))

(add-to-list 'load-path (expand-file-name "vendor" emacsd-site-root))
(require 'htmlize)

(setq org-export-use-babel nil
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type 'css
      org-html-validation-link nil
      org-publish-timestamp-directory
      (expand-file-name ".org-timestamps/" emacsd-site-root)
      org-publish-use-timestamps-flag nil
      org-src-fontify-natively t)

(setq org-publish-project-alist
      `(("emacsd-pages"
         :base-directory ,emacsd-site-content-directory
         :base-extension "org"
         :publishing-directory ,emacsd-site-output-directory
         :publishing-function org-html-publish-to-html
         :recursive t
         :with-author t
         :with-creator nil
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil
         :html-head-extra "<link rel=\"stylesheet\" href=\"site.css\">\n<script src=\"site.js\" defer></script>"
         :html-preamble "<a class=\"source-link\" href=\"https://github.com/Panadestein/emacsd\">Source on GitHub</a>"
         :html-postamble nil)
        ("emacsd-static"
         :base-directory ,emacsd-site-content-directory
         :base-extension "css\\|js"
         :publishing-directory ,emacsd-site-output-directory
         :publishing-function org-publish-attachment
         :recursive t)
        ("emacsd-site" :components ("emacsd-pages" "emacsd-static"))))

(org-publish "emacsd-site" t)

(message "Website built in %s" emacsd-site-output-directory)

(provide 'build-site)
;;; build-site.el ends here
