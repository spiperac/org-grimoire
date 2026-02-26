;;; org-grimoire-tags.el --- Generate tag pages -*- lexical-binding: t -*-

;;; Commentary:
;; Collects tags from all posts and generates per-tag listing pages
;; and a tags index page.

;;; Code:

(require 'org-grimoire-collect)
(require 'org-grimoire-template)
(require 'org-grimoire-index)

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
  (replace-regexp-in-string "-+" "-"
    (replace-regexp-in-string "[^a-z0-9]" "-" (downcase tag))))

;; --- Tag item rendering ---

(defun grimoire--render-tag-item (tag count theme-dir)
  "Render a single TAG as HTML using tag-item partial."
  (grimoire--render-template
   (grimoire--load-template "partials/tag-item" theme-dir)
   (list :name  tag
         :slug  (grimoire--tag-to-slug tag)
         :count (number-to-string count))
   theme-dir))

;; --- Tag pages ---

(defun grimoire--write-tag-page (tag posts output-dir theme-dir)
  "Write a listing page for TAG with its POSTS."
  (let* ((slug     (grimoire--tag-to-slug tag))
         (dir      (expand-file-name "tags" output-dir))
         (output   (expand-file-name (concat slug ".html") dir))
         (sorted   (grimoire--sort-posts-by-date posts))
         (template (grimoire--load-template "index" theme-dir))
         (inner    (grimoire--render-template template
                     (list :title      (concat "Tag: " tag)
                           :posts      (grimoire--render-post-list sorted output-dir theme-dir)
                           :pagination "")
                     theme-dir))
         (html     (grimoire--wrap-base inner (concat "Tag: " tag) theme-dir)))
    (make-directory dir t)
    (write-region html nil output)
    (message "Rendered tag page: %s" output)))

;; --- Tags index ---

(defun grimoire--write-tags-index (tags-table output-dir theme-dir)
  "Write an index page listing all tags."
  (let* ((dir         (expand-file-name "tags" output-dir))
         (output      (expand-file-name "index.html" dir))
         (template    (grimoire--load-template "tags" theme-dir))
         (sorted-tags (sort (hash-table-keys tags-table) #'string<))
         (items       (mapconcat
                       (lambda (tag)
                         (grimoire--render-tag-item
                          tag (length (gethash tag tags-table)) theme-dir))
                       sorted-tags "\n"))
         (inner       (grimoire--render-template template
                        (list :title "Tags"
                              :tags  items)
                        theme-dir))
         (html        (grimoire--wrap-base inner "Tags" theme-dir)))
    (make-directory dir t)
    (write-region html nil output)
    (message "Rendered tags index: %s" output)))

;; --- Public API ---

(defun grimoire-generate-tags (posts output-dir theme-dir)
  "Generate all tag pages and tags index from POSTS."
  (condition-case err
      (let ((tags (grimoire--collect-tags posts)))
        (maphash (lambda (tag tag-posts)
                   (condition-case err
                       (grimoire--write-tag-page tag tag-posts output-dir theme-dir)
                     (error (message "WARNING: Failed to render tag page '%s': %s"
                                     tag (error-message-string err)))))
                 tags)
        (grimoire--write-tags-index tags output-dir theme-dir)
        (message "Generated %d tag pages." (hash-table-count tags)))
    (error (message "WARNING: Failed to generate tags: %s"
                    (error-message-string err)))))

(provide 'org-grimoire-tags)
;;; org-grimoire-tags.el ends here
