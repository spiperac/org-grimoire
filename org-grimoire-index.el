;;; org-grimoire-index.el --- Generate index and listing pages -*- lexical-binding: t -*-

;;; Commentary:
;; Generates index pages, tag pages, and pagination for post listings.

;;; Code:

(require 'org-grimoire-collect)
(require 'org-grimoire-template)

;; --- Sorting ---

(defun grimoire--sort-posts-by-date (posts)
  "Sort POSTS by date, newest first."
  (sort (copy-sequence posts)
        (lambda (a b)
          (string> (or (plist-get a :date) "")
                   (or (plist-get b :date) "")))))

;; --- Post list rendering ---

(defun grimoire--render-post-item (post output-dir theme-dir)
  "Render a single POST as HTML using post-item partial."
  (let* ((title  (or (plist-get post :title) "Untitled"))
         (date   (or (plist-get post :date) ""))
         (tags   (plist-get post :tags))
         (output (plist-get post :output))
         (url    (concat "/" (file-relative-name output output-dir))))
    (grimoire--render-template
     (grimoire--load-template "partials/post-item" theme-dir)
     (list :title title
           :url   url
           :date  date
           :tags  (if tags (string-join tags ", ") ""))
     theme-dir)))

(defun grimoire--render-post-list (posts output-dir theme-dir)
  "Render all POSTS as HTML list items string."
  (mapconcat (lambda (p) (grimoire--render-post-item p output-dir theme-dir))
             posts "\n"))

;; --- Pagination ---

(defun grimoire--paginate (posts per-page)
  "Split POSTS into pages of PER-PAGE posts each."
  (let (pages current)
    (dolist (post posts)
      (push post current)
      (when (= (length current) per-page)
        (push (nreverse current) pages)
        (setq current nil)))
    (when current
      (push (nreverse current) pages))
    (nreverse pages)))

(defun grimoire--pagination-html (current-page total-pages theme-dir)
  "Generate pagination HTML for CURRENT-PAGE of TOTAL-PAGES."
  (let* ((prev-url  (when (> current-page 1)
                      (if (= current-page 2)
                          "/index.html"
                        (format "/page-%d.html" (1- current-page)))))
         (next-url  (when (< current-page total-pages)
                      (format "/page-%d.html" (1+ current-page))))
         (template  (grimoire--load-template "partials/pagination" theme-dir))
         (prev-html (if prev-url
                        (format "<a href=\"%s\">&larr; Newer</a>" prev-url)
                      ""))
         (next-html (if next-url
                        (format "<a href=\"%s\">Older &rarr;</a>" next-url)
                      "")))
    (grimoire--render-template template
      (list :prev prev-html
            :next next-html)
      theme-dir)))

;; --- Index generation ---

(defun grimoire--write-index-page (posts page-num total-pages
                                         output-dir title theme-dir)
  "Write a single index page PAGE-NUM to OUTPUT-DIR."
  (let* ((filename (if (= page-num 1) "index.html"
                     (format "page-%d.html" page-num)))
         (output   (expand-file-name filename output-dir))
         (template (grimoire--load-template "index" theme-dir))
         (inner    (grimoire--render-template template
                     (list :title      title
                           :posts      (grimoire--render-post-list posts output-dir theme-dir)
                           :pagination (grimoire--pagination-html page-num total-pages theme-dir))
                     theme-dir))
         (html     (grimoire--wrap-base inner title theme-dir)))
    (make-directory output-dir t)
    (write-region html nil output)
    (message "Rendered index: %s" output)))

(defun grimoire-generate-index (all-posts output-dir per-page theme-dir &optional title)
  "Generate index pages for all listed posts."
  (condition-case err
      (let* ((title  (or title "Posts"))
             (posts  (grimoire--sort-posts-by-date
                      (cl-remove-if-not
                       (lambda (p) (plist-get p :listed))
                       all-posts)))
             (pages  (grimoire--paginate posts per-page))
             (total  (length pages)))
        (if (null posts)
            (message "WARNING: No listed posts found.")
          (cl-loop for page-posts in pages
                   for i from 1
                   do (grimoire--write-index-page
                       page-posts i total output-dir title theme-dir))))
    (error (message "WARNING: Failed to generate index: %s"
                    (error-message-string err)))))

(provide 'org-grimoire-index)
;;; org-grimoire-index.el ends here
