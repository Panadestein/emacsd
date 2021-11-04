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

(require 'htmlize)
(require 'ox-publish)
(require 'font-lock)

(global-font-lock-mode t)
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-include-default-style nil
      org-src-fontify-natively t)

;; Courtesy of https://emacs.stackexchange.com/questions/38437/org-mode-batch-export-missing-syntax-highlighting

(require 'subr-x) ;; for `when-let'
(unless (boundp 'maximal-integer)
  (defconst maximal-integer (lsh -1 -1)
    "Maximal integer value representable natively in emacs lisp."))

(defun face-spec-default (spec)
  "Get list containing at most the default entry of face SPEC.
Return nil if SPEC has no default entry."
  (let* ((first (car-safe spec))
	 (display (car-safe first)))
    (when (eq display 'default)
      (list (car-safe spec)))))

(defun face-spec-min-color (display-atts)
  "Get min-color entry of DISPLAY-ATTS pair from face spec."
  (let* ((display (car-safe display-atts)))
    (or (car-safe (cdr (assoc 'min-colors display)))
    maximal-integer)))

(defun face-spec-highest-color (spec)
  "Search face SPEC for highest color. That means the DISPLAY entry of SPEC
   with class 'color and highest min-color value."
  (let ((color-list (cl-remove-if-not
		     (lambda (display-atts)
		       (when-let ((display (car-safe display-atts))
				  (class (and (listp display)
					      (assoc 'class display)))
				  (background (assoc 'background display)))
			 (and (member 'light (cdr background))
			      (member 'color (cdr class)))))
		     spec)))
    (cl-reduce (lambda (display-atts1 display-atts2)
		 (if (> (face-spec-min-color display-atts1)
			(face-spec-min-color display-atts2))
		     display-atts1
		   display-atts2))
               (cdr color-list)
               :initial-value (car color-list))))

(defun face-spec-t (spec)
  "Search face SPEC for fall back."
  (cl-find-if (lambda (display-atts)
        (eq (car-safe display-atts) t))
          spec))

(defun my-face-attribute (face attribute &optional frame inherit)
  "Get FACE ATTRIBUTE from `face-user-default-spec' and not from `face-attribute'."
  (let* ((face-spec (face-user-default-spec face))
     (display-attr (or (face-spec-highest-color face-spec)
               (face-spec-t face-spec)))
     (attr (cdr display-attr))
     (val (or (plist-get attr attribute) (car-safe (cdr (assoc attribute attr))))))
    ;; (message "attribute: %S" attribute) ;; for debugging
    (when (and (null (eq attribute :inherit))
           (null val))
      (let ((inherited-face (my-face-attribute face :inherit)))
    (when (and inherited-face
           (null (eq inherited-face 'unspecified)))
      (setq val (my-face-attribute inherited-face attribute)))))
     ;;(message "face: %S attribute: %S display-attr: %S, val: %S" face attribute display-attr val) ;; for debugging
    (or val 'unspecified)))

(advice-add 'face-attribute :override #'my-face-attribute)

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
