;;; org-grimoire-render.el --- Render org files to HTML -*- lexical-binding: t -*-

;;; Commentary:
;; Takes post plists from grimoire-collect and renders them to HTML
;; by exporting org content and injecting into HTML templates.

;;; Code:

(require 'org)
(require 'ox-html)
(require 'org-grimoire-collect)
(require 'org-grimoire-template)

;; --- Org export ---

(defun grimoire--org-to-html (filepath)
  "Export org file at FILEPATH to HTML string, body only."
  (with-temp-buffer
    (insert-file-contents filepath)
    (org-mode)
    (org-export-as 'html nil nil t
                   '(:with-toc nil
                     :with-title nil
                     :section-numbers nil))))

;; --- Main render ---

(defun grimoire--render-post (post templates-dir)
  "Render a single POST plist to HTML string."
  (let* ((type     (or (plist-get post :type) "page"))
         (title    (or (plist-get post :title) ""))
         (template (grimoire--load-template templates-dir type))
         (content  (grimoire--org-to-html (plist-get post :source)))
         (date     (or (plist-get post :date) ""))
         (tags     (plist-get post :tags))
         (inner    (grimoire--render-template template
                     (list :title   title
                           :content content
                           :date    date
                           :tags    (if tags (string-join tags ", ") "")
                           :slug    (plist-get post :slug))
                     templates-dir)))
    (grimoire--wrap-base inner title templates-dir)))

(defun grimoire--copy-assets (assets source-file output-file)
  "Copy ASSETS to output dir, mirroring structure relative to SOURCE-FILE."
  (let ((source-dir (file-name-directory source-file))
        (output-dir (file-name-directory output-file)))
    (dolist (asset assets)
      (let* ((relative (file-relative-name asset source-dir))
             (dest     (expand-file-name relative output-dir)))
        (make-directory (file-name-directory dest) t)
        (copy-file asset dest t)
        (message "Copied asset: %s" dest)))))

(defun grimoire--write-post (post templates-dir)
  "Render POST and write to its output path."
  (let* ((output (plist-get post :output))
         (dir    (file-name-directory output))
         (html   (grimoire--render-post post templates-dir))
         (assets (plist-get post :assets)))
    (make-directory dir t)
    (write-region html nil output)
    (when assets
      (grimoire--copy-assets assets (plist-get post :source) output))
    (message "Rendered: %s" output)))

(defun grimoire-render (posts templates-dir)
  "Render all POSTS to their output paths using TEMPLATES-DIR."
  (dolist (post posts)
    (condition-case err
        (grimoire--write-post post templates-dir)
      (error (message "WARNING: Failed to render %s: %s"
                      (plist-get post :source)
                      (error-message-string err))))))

(provide 'org-grimoire-render)
;;; org-grimoire-render.el ends here
