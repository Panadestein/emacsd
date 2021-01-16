;;; irp-mode.el --- A major mode for dealing with IRPF90 files

;;; Commentary:
;; An attempt to support Scemama's IRPF90 in Emacs

;;; Code:

;; Define IRPF90 extended FORTRAN syntax

(defvar irp-font-lock-keywords)

(setq irp-font-lock-keywords
      (let* (
             ;; Define different keywords
	     (x-keywords '("BEGIN_PROVIDER" "END_PROVIDER" "ASSERT"
			    "FREE" "PROVIDE" "BEGIN_TEMPLATE"
			    "END_TEMPLATE" "BEGIN_SHELL"
			    "END_SHELL" "IRP_IF" "IRP_ELSE" "TOUCH"
			    "SOFT_TOUCH"))
             (x-types '("double precision" "integer"))
	     (x-comments '("BEGIN_DOC" "END_DOC"))

             ;; Generate regex
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-comments-regexp (regexp-opt x-comments 'words)))

	`(
	  (,x-types-regexp . font-lock-type-face)
          (,x-keywords-regexp . font-lock-preprocessor-face)
          (,x-comments-regexp . font-lock-comment-face)
          )))

;;;###autoload
(define-derived-mode irp-mode f90-mode "irp mode"
  "Major mode for editing IRPF90 files."
  :syntax-table nil
  :abbrev-table nil
  (font-lock-add-keywords nil irp-font-lock-keywords))

(provide 'irp-mode)
;;; irp-mode.el ends here
