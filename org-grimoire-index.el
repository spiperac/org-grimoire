;;; org-grimoire-index.el --- Generate index and listing pages -*- lexical-binding: t -*-

;;; Commentary:
;; Generates index pages, tag pages, and pagination for post listings.

;;; Code:

(require 'org-grimoire-collect)

;; --- Sorting ---

(defun grimoire--sort-posts-by-date (posts)
  "Sort POSTS by date, newest first."
  (sort posts
        (lambda (a b)
          (string> (or (plist-get a :date) "")
                   (or (plist-get b :date) "")))))

;; --- Post list HTML ---

(defun grimoire--post-to-list-item (post output-dir)
  "Render a single POST as an HTML list item."
  (let* ((title  (or (plist-get post :title) "Untitled"))
         (date   (or (plist-get post :date) ""))
         (tags   (plist-get post :tags))
         (output (plist-get post :output))
         (url    (concat "/" (file-relative-name output output-dir))))
    (format "<li><a href=\"%s\">%s</a> <time>%s</time>%s</li>"
            url
            title
            date
            (if tags
                (format " <span class=\"tags\">%s</span>"
                        (string-join tags ", "))
              ""))))

(defun grimoire--render-post-list (posts output-dir)
  "Render all POSTS as HTML list items string."
  (mapconcat (lambda (p) (grimoire--post-to-list-item p output-dir))
             posts
             "\n"))

;; --- Pagination ---

(defun grimoire--paginate (posts per-page)
  "Split POSTS into pages of PER-PAGE posts each.
Returns list of pages, each page is a list of posts."
  (let (pages current)
    (dolist (post posts)
      (push post current)
      (when (= (length current) per-page)
        (push (nreverse current) pages)
        (setq current nil)))
    (when current
      (push (nreverse current) pages))
    (nreverse pages)))

(defun grimoire--pagination-html (current-page total-pages output-dir type)
  "Generate pagination HTML for CURRENT-PAGE of TOTAL-PAGES."
  (let ((prev-url (when (> current-page 1)
                    (if (= current-page 2)
                        "/index.html"
                      (format "/page-%d.html" (1- current-page)))))
        (next-url (when (< current-page total-pages)
                    (format "/page-%d.html" (1+ current-page)))))
    (concat
     "<nav class=\"pagination\">"
     (if prev-url (format "<a href=\"%s\">&larr; Newer</a> " prev-url) "")
     (if next-url (format "<a href=\"%s\">Older &rarr;</a>" next-url) "")
     "</nav>")))

;; --- Index generation ---

(defun grimoire--write-index-page (posts page-num total-pages template
                                         output-dir type title per-page)
  "Write a single index page PAGE-NUM to OUTPUT-DIR."
  (let* ((filename (if (= page-num 1) "index.html"
                     (format "page-%d.html" page-num)))
         (output   (expand-file-name filename output-dir))
         (inner    (grimoire--render-template template
                     (list :title      title
                           :posts      (grimoire--render-post-list posts output-dir)
                           :pagination (grimoire--pagination-html
                                        page-num total-pages output-dir type))
                     grimoire--current-templates-dir))
         (html     (grimoire--wrap-base inner title grimoire--current-templates-dir)))
    (make-directory output-dir t)
    (write-region html nil output)
    (message "Rendered index: %s" output)))

(defun grimoire-generate-index (all-posts type template-dir output-dir
                                           &optional title per-page)
  "Generate index pages for posts of TYPE.
ALL-POSTS is the full list, TYPE filters which posts to list.
TITLE defaults to type name, PER-PAGE defaults to 10."
  (let* ((title    (or title (capitalize type)))
         (per-page (or per-page 10))
         (posts    (grimoire--sort-posts-by-date
                    (cl-remove-if-not
                     (lambda (p) (string= (plist-get p :type) type))
                     all-posts)))
         (template (grimoire--load-template template-dir "index"))
         (pages    (grimoire--paginate posts per-page))
         (total    (length pages)))
    (if (null posts)
        (message "No posts of type '%s' found." type)
      (cl-loop for page-posts in pages
               for i from 1
               do (grimoire--write-index-page
                   page-posts i total template
                   output-dir type title per-page)))))

(provide 'org-grimoire-index)
;;; org-grimoire-index.el ends here
