;;; idris-eldoc.el --- ElDoc support for Idris -*- lexical-binding: t -*-


;;; Commentary:
;;

;;; Code:

(defun idris-eldoc-lookup ()
  "Return Eldoc string associated with the thing at point."
  (if (>=-protocol-version 2 1)
      ;; Idris2 using :doc-for command
      (let* ((thing (idris-name-at-point))
             (ty (idris-eval (list :docs-for thing) t))
             (result (car ty))
             (formatting (cdr ty))
             (result-colour (with-temp-buffer
                              ;; (insert result)
                              (idris-propertize-spans (idris-repl-semantic-text-props formatting)
                                (insert result))
                              (buffer-string)))
             )
        ;; result
        (message "-t- error: %s" result)
        result)
    ;; Idris 1 using :doc-overview semantic properties
    (get-char-property (point) 'idris-eldoc)))

(defun idris-semantic-properties-eldoc (props)
  "Compute an Eldoc string from Idris semantic properties (PROPS)."
  (let* ((name (assoc :name props))
         (namespace (assoc :namespace props))
         (source-file (assoc :source-file props))
         (type (pcase (assoc :type props)
                 (`(:type ,ty)
                  (concat " : " ty))
                 (_ "")))
         (doc-overview (pcase (assoc :doc-overview props)
                         (`(:doc-overview ,docs)
                          (if (string-match "[^ ]" docs)
                              (concat "\n"
                                      ;; Emacs will do its own line-wrapping in Eldoc
                                      (replace-regexp-in-string "\\\n" " " docs))
                            ""))
                         (_ ""))))
    (cond (name (list 'idris-eldoc
                      (concat (cadr name)
                              ;; Emacs will do its own line-wrapping in Eldoc
                              (replace-regexp-in-string "\\\n" " " type)
                              doc-overview)))
          ((and namespace source-file)
           (list 'idris-eldoc
                 (file-relative-name (cadr source-file))))
          (namespace (list 'idris-eldoc
                           (cadr namespace)))
          (t nil)))
  )

(provide 'idris-eldoc)

;;; idris-eldoc.el ends here
