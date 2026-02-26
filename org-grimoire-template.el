;;; org-grimoire-template.el --- Template engine for org-grimoire -*- lexical-binding: t -*-

;;; Commentary:
;; Simple template engine supporting {{placeholder}} substitution,
;; {{include file.html}} directives, and base template wrapping.
;; Zero external dependencies.

;;; Code:

;; --- Template loading ---

(defun grimoire--load-template (templates-dir name)
  "Load template file NAME.html from TEMPLATES-DIR."
  (let ((path (expand-file-name (concat name ".html") templates-dir)))
    (if (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      (error "Template not found: %s" path))))

;; --- Include processing ---

(defun grimoire--process-includes (template templates-dir)
  "Replace {{include file.html}} with file contents from TEMPLATES-DIR."
  (replace-regexp-in-string
   "{{include \\([^}]+\\)}}"
   (lambda (match)
     (let* ((filename (match-string 1 match))
            (path (expand-file-name filename templates-dir)))
       (if (file-exists-p path)
           (with-temp-buffer
             (insert-file-contents path)
             (buffer-string))
         (error "Include not found: %s" filename))))
   template))

;; --- Placeholder substitution ---

(defun grimoire--render-template (template vars templates-dir)
  "Replace {{key}} placeholders in TEMPLATE with values from VARS plist.
Processes {{include}} directives first using TEMPLATES-DIR."
  (let ((result (grimoire--process-includes template templates-dir)))
    (cl-loop for (key value) on vars by #'cddr do
      (setq result
            (replace-regexp-in-string
             (concat "{{" (substring (symbol-name key) 1) "}}")
             (or value "")
             result t t)))
    result))

;; --- Base wrapping ---

(defvar grimoire--current-templates-dir nil
  "Current templates directory, set during build.")

(defun grimoire--wrap-base (content title templates-dir)
  "Wrap CONTENT in base.html template from TEMPLATES-DIR."
  (let ((base (grimoire--load-template templates-dir "base")))
    (grimoire--render-template base
      (list :title title
            :content content)
      templates-dir)))

(provide 'org-grimoire-template)
;;; org-grimoire-template.el ends here
