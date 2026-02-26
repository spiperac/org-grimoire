;;; org-grimoire.el --- Emacs-native static site generator -*- lexical-binding: t -*-

;; Copyright (C) 2026 Strahinja Piperac <sp@spiperac.dev>
;;
;; Author: Strahinja Piperac <sp@spiperac.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2") (org "9.7.11"))
;; Keywords: files, hypermedia, outlines, text
;; URL: https://github.com/you/org-grimoire

;; License: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; org-grimoire is a static site generator for Emacs and org mode.
;; Configured and ran entirely through your Emacs config file.
;;
;; Usage:
;;   (org-grimoire-setup "my-site"
;;     :base-dir "~/my-site"
;;     :base-url "https://example.com"
;;     :title    "My Site"
;;     :description "A personal site"
;;     :theme "mytheme")
;;
;;   (org-grimoire-build "my-site")
;;
;; A post is a plist:
;; (:title "My Post"
;;  :date "2026-01-15"
;;  :type "post"
;;  :tags ("emacs" "lisp")
;;  :slug "my-post"
;;  :source "/path/to/file.org"
;;  :output "/path/to/output/my-post.html"
;;  :assets ("/path/to/images/screenshot.png" ...))

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'ox-html)

;;; ---------------------------------------------------------------------------
;;; Internal State
;;; ---------------------------------------------------------------------------

(defvar org-grimoire--sites nil
  "Hash table of named site configurations.")
(unless org-grimoire--sites
  (setq org-grimoire--sites (make-hash-table :test 'equal)))

(defvar org-grimoire--package-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                           buffer-file-name))
  "Directory where org-grimoire.el is installed.")

;;; ---------------------------------------------------------------------------
;;; Build Logger
;;; ---------------------------------------------------------------------------

(defvar org-grimoire--log nil
  "Accumulated log entries for the current build.  List of (level . message).")

(defun org-grimoire--log-reset ()
  "Clear the build log."
  (setq org-grimoire--log nil))

(defun org-grimoire--log (level msg)
  "Record a log entry with LEVEL (:info :warn :error) and MSG.
Also prints immediately via `message'."
  (push (cons level msg) org-grimoire--log)
  (message "[%s] %s"
           (pcase level
             (:info  "INFO ")
             (:warn  "WARN ")
             (:error "ERROR"))
           msg))

(defun org-grimoire--log-summary ()
  "Print a summary of warnings and errors from the current build log."
  (let* ((entries  (nreverse org-grimoire--log))
         (warnings (cl-remove-if-not (lambda (e) (eq (car e) :warn))  entries))
         (errors   (cl-remove-if-not (lambda (e) (eq (car e) :error)) entries)))
    (if (and (null warnings) (null errors))
        (message "Build completed with no warnings or errors.")
      (message "--- Build Summary ---")
      (dolist (e (append warnings errors))
        (message "  [%s] %s"
                 (if (eq (car e) :warn) "WARN " "ERROR")
                 (cdr e)))
      (message "  %d warning(s), %d error(s)."
               (length warnings) (length errors)))))

;;; ---------------------------------------------------------------------------
;;; Template Engine
;;; ---------------------------------------------------------------------------

(defun org-grimoire--default-theme-dir ()
  "Return path to built-in default theme."
  (expand-file-name "themes/default/" org-grimoire--package-dir))

(defun org-grimoire--load-template (name theme-dir)
  "Load template NAME.html from THEME-DIR with fallback to default theme."
  (let* ((filename     (concat name ".html"))
         (user-path    (when theme-dir
                         (expand-file-name filename theme-dir)))
         (default-path (expand-file-name filename (org-grimoire--default-theme-dir)))
         (path         (cond
                        ((and user-path (file-exists-p user-path)) user-path)
                        ((file-exists-p default-path) default-path)
                        (t (user-error "Template not found: %s" name)))))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun org-grimoire--process-includes (template theme-dir)
  "Replace {{include file.html}} in TEMPLATE with file contents.
Checks THEME-DIR first then default theme."
  (replace-regexp-in-string
   "{{include \\([^}]+\\)}}"
   (lambda (match)
     (let* ((filename     (match-string 1 match))
            (user-path    (when theme-dir
                            (expand-file-name filename theme-dir)))
            (default-path (expand-file-name filename (org-grimoire--default-theme-dir)))
            (path         (cond
                           ((and user-path (file-exists-p user-path)) user-path)
                           ((file-exists-p default-path) default-path)
                           (t (user-error "Include not found: %s" filename)))))
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))
   template))

(defun org-grimoire--render-template (template vars theme-dir)
  "Replace {{key}} placeholders in TEMPLATE with values from VARS plist.
Processes {{include}} directives first using THEME-DIR."
  (let ((result (org-grimoire--process-includes template theme-dir)))
    (cl-loop for (key value) on vars by #'cddr do
      (setq result
            (replace-regexp-in-string
             (concat "{{" (substring (symbol-name key) 1) "}}")
             (or value "")
             result t t)))
    result))

(defun org-grimoire--wrap-base (content title theme-dir)
  "Wrap CONTENT in base.html template from THEME-DIR, substituting TITLE."
  (let ((base (org-grimoire--load-template "base" theme-dir)))
    (org-grimoire--render-template base
      (list :title   title
            :content content)
      theme-dir)))

;;; ---------------------------------------------------------------------------
;;; File Utilities
;;; ---------------------------------------------------------------------------

(defun org-grimoire--copy-static (static-dir output-dir)
  "Copy all files from STATIC-DIR to OUTPUT-DIR recursively."
  (when (and static-dir (file-exists-p static-dir))
    (dolist (file (directory-files-recursively static-dir ".*"))
      (let* ((relative (file-relative-name file static-dir))
             (dest     (expand-file-name relative output-dir)))
        (make-directory (file-name-directory dest) t)
        (copy-file file dest t)
        (org-grimoire--log :info (format "Copied: %s" dest))))))

(defun org-grimoire--copy-theme-static (output-dir theme-dir)
  "Copy static files from THEME-DIR into OUTPUT-DIR/static/ if present."
  (let* ((theme        (or theme-dir (org-grimoire--default-theme-dir)))
         (theme-static (expand-file-name "static" theme)))
    (when (file-exists-p theme-static)
      (org-grimoire--copy-static theme-static (expand-file-name "static" output-dir)))))

;;; ---------------------------------------------------------------------------
;;; Collect
;;; ---------------------------------------------------------------------------

(defun org-grimoire--extract-keyword (ast keyword)
  "Extract value of KEYWORD from org AST."
  (org-element-map ast 'keyword
    (lambda (el)
      (when (string= (org-element-property :key el) keyword)
        (org-element-property :value el)))
    nil t))

(defun org-grimoire--file-to-slug (filepath)
  "Convert FILEPATH to a URL slug."
  (file-name-sans-extension
   (file-name-nondirectory filepath)))

(defun org-grimoire--parse-tags (tags-string)
  "Parse TAGS-STRING into a list of tags."
  (when tags-string
    (split-string tags-string "[ ,]+" t "[ \t]+")))

(defun org-grimoire--collect-assets (ast source-file)
  "Extract all file: links from AST, resolve relative to SOURCE-FILE.
Returns list of absolute paths that actually exist on disk."
  (let ((source-dir (file-name-directory source-file)))
    (delq nil
          (org-element-map ast 'link
            (lambda (el)
              (when (string= (org-element-property :type el) "file")
                (let* ((path     (org-element-property :path el))
                       (absolute (expand-file-name path source-dir)))
                  (when (file-exists-p absolute)
                    absolute))))))))

(defun org-grimoire--normalize-boolean (str &optional default)
  "Normalize STR to boolean.
If STR is nil, return DEFAULT.
Treat \"t\", \"true\", \"yes\" as t.
Treat \"nil\", \"false\", \"no\" as nil."
  (if (null str)
      default
    (member (downcase str) '("t" "true" "yes"))))

(defun org-grimoire--collect-file (filepath source-dir output-dir)
  "Parse a single org FILEPATH and return a post plist.
SOURCE-DIR and OUTPUT-DIR used to compute output path."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let* ((ast      (org-element-parse-buffer))
           (title    (org-grimoire--extract-keyword ast "TITLE"))
           (date     (org-grimoire--extract-keyword ast "DATE"))
           (type     (org-grimoire--extract-keyword ast "TYPE"))
           (draft  (org-grimoire--normalize-boolean (org-grimoire--extract-keyword ast "DRAFT")))
           (listed (org-grimoire--normalize-boolean (org-grimoire--extract-keyword ast "LISTED") t))
           (tags     (org-grimoire--parse-tags
                      (org-grimoire--extract-keyword ast "TAGS")))
           (slug     (org-grimoire--file-to-slug filepath))
           (relative (file-relative-name filepath source-dir))
           (output   (expand-file-name
                      (concat (file-name-sans-extension relative) ".html")
                      output-dir))
           (assets   (org-grimoire--collect-assets ast filepath)))
      (unless type
        (org-grimoire--log :warning (format "WARNING: No #+TYPE in %s" filepath)))
      (list :title  title
            :date   date
            :type   type
            :draft  draft
            :listed listed
            :tags   tags
            :slug   slug
            :source filepath
            :output output
            :assets assets))))

(defun org-grimoire--collect (source-dir output-dir)
  "Scan SOURCE-DIR recursively and return list of post plists for OUTPUT-DIR.
Skips files with #+DRAFT: t or missing #+TYPE:."
  (delq nil
        (mapcar (lambda (f)
                  (let ((post (org-grimoire--collect-file f source-dir output-dir)))
                    (cond
                     ((plist-get post :draft)
                      (org-grimoire--log :info (format "Skipping draft: %s" f))
                      nil)
                     ((null (plist-get post :type))
                      (org-grimoire--log :warn (format "No #+TYPE, skipping: %s" f))
                      nil)
                     (t post))))
                (directory-files-recursively source-dir "\\.org$"))))

(defun org-grimoire--sort-posts-by-date (posts)
  "Sort POSTS by date, newest first."
  (sort (copy-sequence posts)
        (lambda (a b)
          (string> (or (plist-get a :date) "")
                   (or (plist-get b :date) "")))))

;;; ---------------------------------------------------------------------------
;;; Render
;;; ---------------------------------------------------------------------------

(defun org-grimoire--org-to-html (filepath)
  "Export org file at FILEPATH to HTML string, body only."
  (with-temp-buffer
    (insert-file-contents filepath)
    (org-mode)
    (org-export-as 'html nil nil t
                   '(:with-toc nil
                     :with-title nil
                     :section-numbers nil))))

(defun org-grimoire--render-post (post theme-dir)
  "Render a single POST plist to HTML string using THEME-DIR."
  (let* ((type     (or (plist-get post :type) "page"))
         (title    (or (plist-get post :title) ""))
         (template (org-grimoire--load-template type theme-dir))
         (content  (org-grimoire--org-to-html (plist-get post :source)))
         (date     (or (plist-get post :date) ""))
         (tags     (plist-get post :tags))
         (inner    (org-grimoire--render-template template
                     (list :title   title
                           :content content
                           :date    date
                           :tags    (if tags (string-join tags ", ") "")
                           :slug    (plist-get post :slug))
                     theme-dir)))
    (org-grimoire--wrap-base inner title theme-dir)))

(defun org-grimoire--copy-assets (assets source-file output-file)
  "Copy ASSETS mirroring structure relative to SOURCE-FILE into dir of OUTPUT-FILE."
  (let ((source-dir (file-name-directory source-file))
        (output-dir (file-name-directory output-file)))
    (dolist (asset assets)
      (let* ((relative (file-relative-name asset source-dir))
             (dest     (expand-file-name relative output-dir)))
        (make-directory (file-name-directory dest) t)
        (copy-file asset dest t)
        (org-grimoire--log :info (format "Copied asset: %s" dest))))))

(defun org-grimoire--write-post (post theme-dir)
  "Render POST and write to its output path using THEME-DIR."
  (let* ((output (plist-get post :output))
         (dir    (file-name-directory output))
         (html   (org-grimoire--render-post post theme-dir))
         (assets (plist-get post :assets)))
    (make-directory dir t)
    (write-region html nil output)
    (when assets
      (org-grimoire--copy-assets assets (plist-get post :source) output))
    (org-grimoire--log :info (format "Rendered: %s" output))))

(defun org-grimoire--render-all (posts theme-dir)
  "Render all POSTS to their output paths using THEME-DIR."
  (dolist (post posts)
    (condition-case err
        (org-grimoire--write-post post theme-dir)
      (error (org-grimoire--log :warning (format "WARNING: Failed to render %s: %s"
                      (plist-get post :source)
                      (error-message-string err)))))))

;;; ---------------------------------------------------------------------------
;;; Index and Pagination
;;; ---------------------------------------------------------------------------

(defun org-grimoire--render-post-item (post output-dir theme-dir)
  "Render POST as HTML using post-item partial from THEME-DIR.
URL is computed relative to OUTPUT-DIR."
  (let* ((title  (or (plist-get post :title) "Untitled"))
         (date   (or (plist-get post :date) ""))
         (tags   (plist-get post :tags))
         (output (plist-get post :output))
         (url    (concat "/" (file-relative-name output output-dir))))
    (org-grimoire--render-template
     (org-grimoire--load-template "partials/post-item" theme-dir)
     (list :title title
           :url   url
           :date  date
           :tags  (if tags (string-join tags ", ") ""))
     theme-dir)))

(defun org-grimoire--render-post-list (posts output-dir theme-dir)
  "Render POSTS as concatenated HTML list items using THEME-DIR.
URLs are computed relative to OUTPUT-DIR."
  (mapconcat (lambda (p) (org-grimoire--render-post-item p output-dir theme-dir))
             posts "\n"))

(defun org-grimoire--paginate (posts per-page)
  "Split POSTS into pages of PER-PAGE posts each."
  (let (pages current)
    (let ((count 0))
      (dolist (post posts)
        (push post current)
        (setq count (1+ count))
        (when (= count per-page)
          (push (nreverse current) pages)
          (setq current nil count 0))))
    (when current
      (push (nreverse current) pages))
    (nreverse pages)))

(defun org-grimoire--pagination-html (current-page total-pages theme-dir)
  "Generate pagination HTML for CURRENT-PAGE of TOTAL-PAGES using THEME-DIR."
  (let* ((prev-url  (when (> current-page 1)
                      (if (= current-page 2)
                          "/index.html"
                        (format "/page-%d.html" (1- current-page)))))
         (next-url  (when (< current-page total-pages)
                      (format "/page-%d.html" (1+ current-page))))
         (template  (org-grimoire--load-template "partials/pagination" theme-dir))
         (prev-html (if prev-url
                        (format "<a href=\"%s\">&larr; Newer</a>" prev-url)
                      ""))
         (next-html (if next-url
                        (format "<a href=\"%s\">Older &rarr;</a>" next-url)
                      "")))
    (org-grimoire--render-template template
      (list :prev prev-html
            :next next-html)
      theme-dir)))

(defun org-grimoire--write-index-page (posts page-num total-pages
                                             output-dir title theme-dir)
  "Write index page PAGE-NUM of TOTAL-PAGES to OUTPUT-DIR.
POSTS are rendered using THEME-DIR templates with page TITLE."
  (let* ((filename (if (= page-num 1) "index.html"
                     (format "page-%d.html" page-num)))
         (output   (expand-file-name filename output-dir))
         (template (org-grimoire--load-template "index" theme-dir))
         (inner    (org-grimoire--render-template template
                     (list :title      title
                           :posts      (org-grimoire--render-post-list posts output-dir theme-dir)
                           :pagination (org-grimoire--pagination-html page-num total-pages theme-dir))
                     theme-dir))
         (html     (org-grimoire--wrap-base inner title theme-dir)))
    (make-directory output-dir t)
    (write-region html nil output)
    (org-grimoire--log :info (format "Rendered index: %s" output))))

(defun org-grimoire--generate-index (all-posts output-dir per-page theme-dir &optional title)
"Generate paginated index pages from listed posts in ALL-POSTS.
Write PER-PAGE posts per page to OUTPUT-DIR using THEME-DIR templates.
Optional TITLE defaults to \"Posts\"."
  (condition-case err
      (let* ((title (or title "Posts"))
             (posts (org-grimoire--sort-posts-by-date
                     (cl-remove-if-not
                      (lambda (p) (plist-get p :listed))
                      all-posts)))
             (pages (org-grimoire--paginate posts per-page))
             (total (length pages)))
        (if (null posts)
            (org-grimoire--log :warning (format "WARNING: No listed posts found.")
          (cl-loop for page-posts in pages
                   for i from 1
                   do (org-grimoire--write-index-page
                       page-posts i total output-dir title theme-dir)))))
    (error (org-grimoire--log :warning (format "WARNING: Failed to generate index: %s"
                    (error-message-string err))))))

;;; ---------------------------------------------------------------------------
;;; Tags
;;; ---------------------------------------------------------------------------

(defun org-grimoire--collect-tags (posts)
  "Return a hash table of tag -> list of posts from POSTS."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (post posts)
      (dolist (tag (plist-get post :tags))
        (puthash tag (cons post (gethash tag tags '())) tags)))
    tags))

(defun org-grimoire--tag-to-slug (tag)
  "Convert TAG to a URL safe slug."
  (replace-regexp-in-string "-+" "-"
    (replace-regexp-in-string "[^a-z0-9]" "-" (downcase tag))))

(defun org-grimoire--render-tag-item (tag count theme-dir)
  "Render TAG with post COUNT as HTML using tag-item partial from THEME-DIR."
  (org-grimoire--render-template
   (org-grimoire--load-template "partials/tag-item" theme-dir)
   (list :name  tag
         :slug  (org-grimoire--tag-to-slug tag)
         :count (number-to-string count))
   theme-dir))

(defun org-grimoire--write-tag-page (tag posts output-dir theme-dir)
  "Write a listing page for TAG with its POSTS to OUTPUT-DIR using THEME-DIR."
  (let* ((slug     (org-grimoire--tag-to-slug tag))
         (dir      (expand-file-name "tags" output-dir))
         (output   (expand-file-name (concat slug ".html") dir))
         (sorted   (org-grimoire--sort-posts-by-date posts))
         (template (org-grimoire--load-template "index" theme-dir))
         (inner    (org-grimoire--render-template template
                     (list :title      (concat "Tag: " tag)
                           :posts      (org-grimoire--render-post-list sorted output-dir theme-dir)
                           :pagination "")
                     theme-dir))
         (html     (org-grimoire--wrap-base inner (concat "Tag: " tag) theme-dir)))
    (make-directory dir t)
    (write-region html nil output)
    (org-grimoire--log :info (format "Rendered tag page: %s" output))))

(defun org-grimoire--write-tags-index (tags-table output-dir theme-dir)
  "Write index page listing all tags from TAGS-TABLE to OUTPUT-DIR.
Using THEME-DIR for templates."
  (let* ((dir         (expand-file-name "tags" output-dir))
         (output      (expand-file-name "index.html" dir))
         (template    (org-grimoire--load-template "tags" theme-dir))
         (sorted-tags (sort (hash-table-keys tags-table) #'string<))
         (items       (mapconcat
                       (lambda (tag)
                         (org-grimoire--render-tag-item
                          tag (length (gethash tag tags-table)) theme-dir))
                       sorted-tags "\n"))
         (inner       (org-grimoire--render-template template
                        (list :title "Tags"
                              :tags  items)
                        theme-dir))
         (html        (org-grimoire--wrap-base inner "Tags" theme-dir)))
    (make-directory dir t)
    (write-region html nil output)
    (org-grimoire--log :info (format "Rendered tags index: %s" output))))

(defun org-grimoire--generate-tags (posts output-dir theme-dir)
  "Generate all tag pages and tags index from POSTS to OUTPUT-DIR using THEME-DIR."
  (condition-case err
      (let ((tags (org-grimoire--collect-tags posts)))
        (maphash (lambda (tag tag-posts)
                   (condition-case err
                       (org-grimoire--write-tag-page tag tag-posts output-dir theme-dir)
                     (error (org-grimoire--log :warn (format "Failed to render tag page '%s': %s"
                                                             tag (error-message-string err))))))
                 tags)
        (org-grimoire--write-tags-index tags output-dir theme-dir)
        (org-grimoire--log :info (format "Generated %d tag pages." (hash-table-count tags))))
    (error (org-grimoire--log :error (format "Failed to generate tags: %s"
                                             (error-message-string err))))))

;;; ---------------------------------------------------------------------------
;;; Feeds
;;; ---------------------------------------------------------------------------

(defun org-grimoire--rss-date (date-string)
  "Convert DATE-STRING (yyyy-mm-dd) to RFC 822 format for RSS."
  (when date-string
    (let ((time (date-to-time (concat date-string " 00:00:00"))))
      (format-time-string "%a, %d %b %Y %H:%M:%S +0000" time t))))

(defun org-grimoire--atom-date (date-string)
  "Convert DATE-STRING (yyyy-mm-dd) to RFC 3339 format for Atom."
  (when date-string
    (let ((time (date-to-time (concat date-string " 00:00:00"))))
      (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))))

(defun org-grimoire--escape-xml (str)
  "Escape special XML characters in STR."
  (when str
    (replace-regexp-in-string
     "&" "&amp;"
     (replace-regexp-in-string
      "<" "&lt;"
      (replace-regexp-in-string
       ">" "&gt;"
       (replace-regexp-in-string
        "\"" "&quot;" str))))))

(defun org-grimoire--post-url (post base-url output-dir)
  "Return full URL for POST given BASE-URL and OUTPUT-DIR."
  (let* ((output   (plist-get post :output))
         (relative (file-relative-name output output-dir)))
    (concat (string-trim-right base-url "/") "/" relative)))

(defun org-grimoire--rss-item (post base-url output-dir)
  "Return RSS 2.0 item XML string for POST using BASE-URL and OUTPUT-DIR."
  (let ((title (org-grimoire--escape-xml (plist-get post :title)))
        (url   (org-grimoire--post-url post base-url output-dir))
        (date  (org-grimoire--rss-date (plist-get post :date)))
        (tags  (plist-get post :tags)))
    (concat
     "  <item>\n"
     (format "    <title>%s</title>\n" (or title "Untitled"))
     (format "    <link>%s</link>\n" url)
     (format "    <guid>%s</guid>\n" url)
     (when date (format "    <pubDate>%s</pubDate>\n" date))
     (when tags
       (mapconcat (lambda (tag)
                    (format "    <category>%s</category>\n"
                            (org-grimoire--escape-xml tag)))
                  tags ""))
     "  </item>\n")))

(defun org-grimoire--generate-rss (posts base-url output-dir site-title site-description)
  "Return RSS 2.0 feed XML string for POSTS.
Uses BASE-URL and OUTPUT-DIR to build URLs.
   SITE-TITLE and SITE-DESCRIPTION for channel metadata."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<rss version=\"2.0\">\n"
   "<channel>\n"
   (format "  <title>%s</title>\n" (org-grimoire--escape-xml site-title))
   (format "  <link>%s</link>\n" base-url)
   (format "  <description>%s</description>\n"
           (org-grimoire--escape-xml site-description))
   (format "  <lastBuildDate>%s</lastBuildDate>\n"
           (format-time-string "%a, %d %b %Y %H:%M:%S +0000" nil t))
   (mapconcat (lambda (p) (org-grimoire--rss-item p base-url output-dir))
              posts "")
   "</channel>\n"
   "</rss>\n"))

(defun org-grimoire--atom-entry (post base-url output-dir)
  "Return Atom entry XML string for POST using BASE-URL and OUTPUT-DIR."
  (let ((title (org-grimoire--escape-xml (plist-get post :title)))
        (url   (org-grimoire--post-url post base-url output-dir))
        (date  (org-grimoire--atom-date (plist-get post :date))))
    (concat
     "  <entry>\n"
     (format "    <title>%s</title>\n" (or title "Untitled"))
     (format "    <link href=\"%s\"/>\n" url)
     (format "    <id>%s</id>\n" url)
     (when date (format "    <updated>%s</updated>\n" date))
     "  </entry>\n")))

(defun org-grimoire--generate-atom (posts base-url output-dir site-title)
  "Return Atom feed XML string for POSTS.
Uses BASE-URL and OUTPUT-DIR to build URLs, SITE-TITLE for feed metadata."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
   (format "  <title>%s</title>\n" (org-grimoire--escape-xml site-title))
   (format "  <link href=\"%s\"/>\n" base-url)
   (format "  <id>%s</id>\n" base-url)
   (format "  <updated>%s</updated>\n"
           (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
   (mapconcat (lambda (p) (org-grimoire--atom-entry p base-url output-dir))
              posts "")
   "</feed>\n"))

(defun org-grimoire--generate-feeds (posts output-dir base-url site-title site-description)
  "Write rss.xml and atom.xml to OUTPUT-DIR from POSTS.
Uses BASE-URL, SITE-TITLE and SITE-DESCRIPTION for feed metadata."
  (condition-case err
      (let* ((feed-posts (org-grimoire--sort-posts-by-date posts))
             (rss-path   (expand-file-name "rss.xml" output-dir))
             (atom-path  (expand-file-name "atom.xml" output-dir)))
        (write-region
         (org-grimoire--generate-rss feed-posts base-url output-dir site-title site-description)
         nil rss-path)
        (write-region
         (org-grimoire--generate-atom feed-posts base-url output-dir site-title)
         nil atom-path)
        (org-grimoire--log :info "Generated feeds: rss.xml, atom.xml"))
    (error (org-grimoire--log :error (format "Failed to generate feeds: %s"
                                             (error-message-string err))))))
;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun org-grimoire--resolve-config (args)
  "Derive :source :output :static :theme from :base-dir in ARGS.
User-specified :theme string is resolved relative to base-dir/themes."
  (let ((base (plist-get args :base-dir))
        (theme (plist-get args :theme))
        (final args))
    (when base
      (setq base (expand-file-name base))
      ;; Always compute theme path if user supplied one
      (when theme
        (setq final (plist-put final :theme
                               (expand-file-name theme (expand-file-name "themes" base)))))

      ;; Only fill in missing :source, :output, :static
      (dolist (key '(:source :output :static))
        (unless (plist-member final key)
          (setq final
                (plist-put final key
                           (pcase key
                             (:source (expand-file-name "content" base))
                             (:output (expand-file-name "public_html" base))
                             (:static (expand-file-name "static" base))))))))
    final))

(defun org-grimoire--validate-config (config)
  "Validate required org-grimoire CONFIG.  Signal error if invalid."
  (let ((source   (plist-get config :source))
        (output   (plist-get config :output))
        (base-url (plist-get config :base-url))
        (title    (plist-get config :title))
        (per-page (or (plist-get config :per-page) 10)))
    (unless (and source (file-directory-p source))
      (user-error "Invalid or missing :source directory"))
    (unless output
      (user-error "Missing :output directory"))
    (unless (and base-url (string-match-p "\\`https?://" base-url))
      (user-error "Missing or invalid :base-url (must start with http/https)"))
    (unless (and title (not (string-empty-p title)))
      (user-error "Missing or empty :title"))
    (unless (and (integerp per-page) (> per-page 0))
      (user-error ":per-page must be a positive integer"))))

;;;###autoload
(defun org-grimoire-setup (name &rest args)
  "Register a site configuration NAME with ARGS.
Keys: :base-dir :base-url :title :description :theme :per-page
Optional overrides: :source :output :static"
  (puthash name (org-grimoire--resolve-config args) org-grimoire--sites))

;;;###autoload
(defun org-grimoire-build (name)
  "Build the site registered as NAME."
  (interactive "sSite name: ")
  (org-grimoire--log-reset)
  (let* ((config      (or (gethash name org-grimoire--sites)
                          (user-error "No site configured with name: %s" name)))
         (source      (plist-get config :source))
         (output      (plist-get config :output))
         (static      (plist-get config :static))
         (theme-dir   (plist-get config :theme))
         (base-url    (plist-get config :base-url))
         (title       (plist-get config :title))
         (description (plist-get config :description))
         (per-page    (or (plist-get config :per-page) 10)))
    (org-grimoire--validate-config config)
    (org-grimoire--log :info "Build started")
    (org-grimoire--log :info (format "Source: %s" source))
    (org-grimoire--log :info (format "Output: %s" output))
    (condition-case err
        (let ((posts (org-grimoire--collect source output)))
          (org-grimoire--log :info (format "Collected %d posts." (length posts)))
          (org-grimoire--copy-static static (expand-file-name "static" output))
          (org-grimoire--copy-theme-static output theme-dir)
          (org-grimoire--render-all posts theme-dir)
          (org-grimoire--generate-index posts output per-page theme-dir title)
          (org-grimoire--generate-tags posts output theme-dir)
          (org-grimoire--generate-feeds posts output base-url title description)
          (org-grimoire--log :info "Build complete.")
          (org-grimoire--log-summary))
      (error
       (org-grimoire--log :error (error-message-string err))
       (org-grimoire--log-summary)))))

(provide 'org-grimoire)
;;; org-grimoire.el ends here
