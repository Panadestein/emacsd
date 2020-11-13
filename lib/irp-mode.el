;;; irpmode.el --- A major mode for dealing with IRPF90 files

;;; Commentary:
;; An attempt to support Scemama's IRPF90 in Emacs
;; Package-Requires: ((dash "2.17.0"))

;;; Code:

;(defconst irp-mode-syntax-table
;  (-let [table (copy-syntax-table f90-mode-syntax-table)]
;    ;; Additional syntax
;    table)
;  "IRPF90 syntax table.")

(defconst irp--kwds-constants
  '("BEGIN_PROVIDER" "END_PROVIDER" "BEGIN_DOC"
    "END_DOC" "double precision")
  "IRPF90 constant keywords.")

(defconst irp--font-lock-kwds-constants
  (list
   (rx-to-string
    `(: (or ,@irp--kwds-constants)))
   '(0 font-lock-constant-face))
  "IRPF90 constant keywords.")

(defconst irp-font-lock-kwds
  (list irp--font-lock-kwds-constants)
  "All IRPF90 font lock keywords.")

(defun irp-mode ()
  "Major mode for editing IRPF90 files."
  (interactive)
  (kill-all-local-variables)
  ;(set-syntax-table irp-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(irp-font-lock-kwds))
  (setq major-mode 'irp-mode)
  (setq mode-name "IRPF90"))

(provide 'irp-mode)
;;; irp-mode.el ends here
