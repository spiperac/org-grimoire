;;; org-grimoire-template.el --- Template engine for org-grimoire -*- lexical-binding: t -*-

;;; Commentary:
;; Simple template engine supporting {{placeholder}} substitution,
;; {{include file.html}} directives, and theme system with fallback
;; to built-in default theme.

;;; Code:

(defvar grimoire--package-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                           buffer-file-name))
  "Directory where org-grimoire is installed.")

(defun grimoire--default-theme-dir ()
  "Return path to built-in default theme."
  (expand-file-name "themes/default/" grimoire--package-dir))

;; --- Template loading ---

(defun grimoire--load-template (name theme-dir)
  "Load template NAME.html from THEME-DIR with fallback to default theme."
  (let* ((filename     (concat name ".html"))
         (user-path    (when theme-dir
                         (expand-file-name filename theme-dir)))
         (default-path (expand-file-name filename (grimoire--default-theme-dir)))
         (path         (cond
                        ((and user-path (file-exists-p user-path)) user-path)
                        ((file-exists-p default-path) default-path)
                        (t (error "Template not found: %s" name)))))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;; --- Include processing ---

(defun grimoire--process-includes (template theme-dir)
  "Replace {{include file.html}} in TEMPLATE with file contents.
Checks THEME-DIR first then default theme."
  (replace-regexp-in-string
   "{{include \\([^}]+\\)}}"
   (lambda (match)
     (let* ((filename     (match-string 1 match))
            (user-path    (when theme-dir
                            (expand-file-name filename theme-dir)))
            (default-path (expand-file-name filename (grimoire--default-theme-dir)))
            (path (cond
                   ((and user-path (file-exists-p user-path)) user-path)
                   ((file-exists-p default-path) default-path)
                   (t (error "Include not found: %s" filename)))))
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))
   template))

;; --- Placeholder substitution ---

(defun grimoire--render-template (template vars theme-dir)
  "Replace {{key}} placeholders in TEMPLATE with values from VARS plist.
Processes {{include}} directives first using THEME-DIR."
  (let ((result (grimoire--process-includes template theme-dir)))
    (cl-loop for (key value) on vars by #'cddr do
      (setq result
            (replace-regexp-in-string
             (concat "{{" (substring (symbol-name key) 1) "}}")
             (or value "")
             result t t)))
    result))

;; --- Copy Theme Static Files ---

(defun grimoire--copy-theme-static (output-dir theme-dir)
  "Copy static files from THEME-DIR to OUTPUT-DIR if present."
  (let* ((theme    (or theme-dir (grimoire--default-theme-dir)))
         (theme-static (expand-file-name "static" theme)))
    (when (file-exists-p theme-static)
      (grimoire--copy-static theme-static (expand-file-name "static" output-dir)))))

;; --- Base wrapping ---

(defun grimoire--wrap-base (content title theme-dir)
  "Wrap CONTENT in base.html template from THEME-DIR."
  (let ((base (grimoire--load-template "base" theme-dir)))
    (grimoire--render-template base
      (list :title   title
            :content content)
      theme-dir)))

(provide 'org-grimoire-template)
;;; org-grimoire-template.el ends here
