;;; org-grimoire-tags.el --- Generate tag pages -*- lexical-binding: t -*-

;;; Commentary:
;; Collects tags from all posts and generates per-tag listing pages
;; and a tags index page.

;;; Code:

(require 'org-grimoire-collect)
(require 'org-grimoire-render)
(require 'org-grimoire-index)
(require 'org-grimoire-template)

;; --- Tag collection ---

(defun grimoire--collect-tags (posts)
  "Return a hash table of tag -> list of posts from POSTS."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (post posts)
      (dolist (tag (plist-get post :tags))
        (puthash tag (cons post (gethash tag tags '())) tags)))
    tags))

(defun grimoire--tag-to-slug (tag)
  "Convert TAG to a URL safe slug."
  (replace-regexp-in-string "[^a-z0-9]" "-" (downcase tag)))

;; --- Tag pages ---

(defun grimoire--write-tag-page (tag posts templates-dir output-dir)
  "Write a listing page for TAG with its POSTS."
  (let* ((slug     (grimoire--tag-to-slug tag))
         (dir      (expand-file-name "tags" output-dir))
         (output   (expand-file-name (concat slug ".html") dir))
         (template (grimoire--load-template templates-dir "index"))
         (sorted   (grimoire--sort-posts-by-date posts))
         (inner    (grimoire--render-template template
                     (list :title      (concat "Tag: " tag)
                           :posts      (grimoire--render-post-list sorted output-dir)
                           :pagination "")
                     templates-dir))
         (html     (grimoire--wrap-base inner (concat "Tag: " tag) templates-dir)))
    (make-directory dir t)
    (write-region html nil output)
    (message "Rendered tag page: %s" output)))

;; --- Tag index ---

(defun grimoire--write-tags-index (tags-table templates-dir output-dir)
  "Write an index page listing all tags."
  (let* ((dir      (expand-file-name "tags" output-dir))
         (output   (expand-file-name "index.html" dir))
         (template (grimoire--load-template templates-dir "tags"))
         (items    (let (result)
                     (maphash (lambda (tag posts)
                                (push (format "<li><a href=\"/tags/%s.html\">%s</a> (%d)</li>"
                                              (grimoire--tag-to-slug tag)
                                              tag
                                              (length posts))
                                      result))
                              tags-table)
                     (string-join (sort result #'string<) "\n")))
         (inner    (grimoire--render-template template
                     (list :title "Tags"
                           :tags  items)
                     templates-dir))
         (html     (grimoire--wrap-base inner "Tags" templates-dir)))
    (make-directory dir t)
    (write-region html nil output)
    (message "Rendered tags index: %s" output)))

;; --- Public API ---

(defun grimoire-generate-tags (posts templates-dir output-dir)
  "Generate all tag pages and tags index from POSTS."
  (condition-case err
      (let ((tags (grimoire--collect-tags posts)))
        (maphash (lambda (tag tag-posts)
                   (condition-case err
                       (grimoire--write-tag-page tag tag-posts templates-dir output-dir)
                     (error (message "WARNING: Failed to render tag page '%s': %s"
                                     tag (error-message-string err)))))
                 tags)
        (grimoire--write-tags-index tags templates-dir output-dir)
        (message "Generated %d tag pages." (hash-table-count tags)))
    (error (message "WARNING: Failed to generate tags: %s"
                    (error-message-string err)))))

(provide 'org-grimoire-tags)
;;; org-grimoire-tags.el ends here
