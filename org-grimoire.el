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

;; --- State ---

(defvar grimoire--sites nil
  "Hash table of named site configurations.")
(unless grimoire--sites
  (setq grimoire--sites (make-hash-table :test 'equal)))

;; --- Requires ---

(require 'org-grimoire-collect)
(require 'org-grimoire-render)
(require 'org-grimoire-index)
(require 'org-grimoire-tags)
(require 'org-grimoire-feed)

;; --- Public API ---

(defun grimoire--resolve-config (args)
  "Derive :source :output :static :theme from :base-dir unless explicitly set."
  (let ((base  (plist-get args :base-dir))
        (theme (plist-get args :theme)))
    (if (not base)
        args
      (let ((base (expand-file-name base)))
        (append
         (list
          :source (or (plist-get args :source)
                      (expand-file-name "content" base))
          :output (or (plist-get args :output)
                      (expand-file-name "public_html" base))
          :static (or (plist-get args :static)
                      (expand-file-name "static" base))
          :theme  (or (and theme
                           (expand-file-name theme
                                             (expand-file-name "themes" base)))
                      (plist-get args :theme)))
         args)))))

(defun grimoire-setup (name &rest args)
  "Register a site configuration NAME with ARGS.
Keys: :base-dir :base-url :title :description :theme :per-page
Optional overrides: :source :output :static"
  (puthash name (grimoire--resolve-config args) grimoire--sites))

(defun grimoire-build (name)
  "Build the site registered as NAME."
  (interactive "sSite name: ")
  (let* ((config      (or (gethash name grimoire--sites)
                          (error "No site configured with name: %s" name)))
         (source      (plist-get config :source))
         (output      (plist-get config :output))
         (static      (plist-get config :static))
         (theme-dir   (plist-get config :theme))
         (base-url    (plist-get config :base-url))
         (title       (plist-get config :title))
         (description (plist-get config :description))
         (per-page    (or (plist-get config :per-page) 10)))
    (message "=== org-grimoire build started ===")
    (message "Source: %s" source)
    (message "Output: %s" output)
    (condition-case err
        (let ((posts (grimoire-collect source output)))
          (message "Collected %d posts." (length posts))
          (grimoire--copy-static static (expand-file-name "static" output))
          (grimoire--copy-theme-static output theme-dir)
          (grimoire-render posts theme-dir)
          (grimoire-generate-index posts output per-page theme-dir title)
          (grimoire-generate-tags posts output theme-dir)
          (grimoire-generate-feeds posts output base-url title description)
          (message "=== org-grimoire build complete ==="))
      (error (message "ERROR: Build failed: %s"
                      (error-message-string err))))))

(provide 'org-grimoire)
;;; org-grimoire.el ends here
