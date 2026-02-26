;;; org-grimoire.el --- Emacs-native static site generator -*- lexical-binding: t -*-

;; Author: your name
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: org, blog, static-site-generator
;; URL: https://github.com/you/org-grimoire

;;; Commentary:
;; org-grimoire is a static site generator for Emacs.
;; Configured entirely through your Emacs config.

;;; Code:

(require 'org-grimoire-collect)
(require 'org-grimoire-render)
(require 'org-grimoire-index)
(require 'org-grimoire-tags)
(require 'org-grimoire-feed)

;; --- State ---

(defvar grimoire--types (make-hash-table :test 'equal)
  "Hash table of defined content types.")

(defvar grimoire--config nil
  "Global grimoire configuration plist.")

;; --- Public API ---

(defun grimoire-setup (&rest args)
  "Configure grimoire with ARGS.
Keys: :source :output :templates"
  (setq grimoire--config args))

(defun grimoire-define-type (name &rest args)
  "Define a content type NAME with ARGS.
Keys: :listing :pagination :per-page :feed"
  (puthash name args grimoire--types))

(defun grimoire--copy-static (static-dir output-dir)
  "Copy all files from STATIC-DIR to OUTPUT-DIR recursively."
  (when (and static-dir (file-exists-p static-dir))
    (let ((files (directory-files-recursively static-dir ".*")))
      (dolist (file files)
        (let* ((relative (file-relative-name file static-dir))
               (dest     (expand-file-name relative output-dir)))
          (make-directory (file-name-directory dest) t)
          (copy-file file dest t)
          (message "Copied: %s" dest))))))

(defun grimoire-build ()
  "Build the site."
  (interactive)
  (let ((source      (plist-get grimoire--config :source))
        (output      (plist-get grimoire--config :output))
        (templates   (plist-get grimoire--config :templates))
        (static      (plist-get grimoire--config :static))
        (base-url    (plist-get grimoire--config :base-url))
        (title       (plist-get grimoire--config :title))
        (description (plist-get grimoire--config :description)))
    (message "=== org-grimoire build started ===")
    (message "Source:    %s" source)
    (message "Output:    %s" output)
    (message "Templates: %s" templates)
    (condition-case err
        (let ((posts (grimoire-collect source output)))
          (message "Collected %d posts." (length posts))
          (setq grimoire--current-templates-dir templates)
          (grimoire--copy-static static (expand-file-name "static" output))
          (grimoire-render posts templates)
          (maphash (lambda (type config)
                     (when (plist-get config :listing)
                       (grimoire-generate-index posts type templates output
                                               nil
                                               (or (plist-get config :per-page) 10))))
                   grimoire--types)
          (grimoire-generate-tags posts templates output)
          (grimoire-generate-feeds posts output base-url title description)
          (message "=== org-grimoire build complete ==="))
      (error (message "ERROR: Build failed: %s"
                      (error-message-string err))))))

(provide 'org-grimoire)
;;; org-grimoire.el ends here

