;;; org-grimoire.el --- Emacs-native static site generator -*- lexical-binding: t -*-

;; Copyright (C) 2026 Strahinja Piperac <sp@spiperac.dev>
;;
;; Author: Strahinja Piperac <sp@spiperac.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2") (org "9.7.11"))
;; Keywords: files, hypermedia, outlines, text
;; URL: https://github.com/spiperac/org-grimoire

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
;; org-grimoire is a static site generator for Emacs and Org mode.
;; Configured and run entirely through your Emacs init file.
;;
;; Usage:
;;   (org-grimoire-setup "my-site"
;;     :base-dir    "~/my-site"
;;     :base-url    "https://example.com"
;;     :site-title  "My Site"
;;     :description "A personal site"
;;     :theme       "mytheme")
;;
;;   (org-grimoire-build "my-site")
;;
;; :site-title is the global name of your site, used in the <title> element,
;; feeds, and navigation.  The per-page :title placeholder in templates is
;; filled with each post's own #+TITLE keyword.
;;
;; A post is represented as a plist:
;;   (:title        "My Post"
;;    :date         "2026-01-15"
;;    :tags         ("emacs" "lisp")
;;    :slug         "my-post"
;;    :source       "/path/to/file.org"
;;    :output       "/path/to/output/my-post.html"
;;    :assets       ("/path/to/images/screenshot.png"))

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'ox-html)

(declare-function httpd-start "simple-httpd")
(defvar httpd-root)
(defvar httpd-port)

;;; Internal State:

(defvar org-grimoire--sites (make-hash-table :test 'equal)
  "Hash table of named site configurations.")

(defvar org-grimoire--current-site nil
  "Name of the site currently being built.
Bound dynamically by `org-grimoire-build'; do not set this directly.")

(defvar org-grimoire--package-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                           buffer-file-name))
  "Directory containing org-grimoire.el.")

(defun org-grimoire--config-get (key)
  "Return the value of KEY from the current site configuration."
  (plist-get (gethash org-grimoire--current-site org-grimoire--sites) key))

;;; Build Logger:

(defvar org-grimoire--log nil
  "Accumulated log entries for the current build.
Each entry is a cons cell (LEVEL . MESSAGE) where LEVEL is one of
:info, :warn, or :error.")

(defun org-grimoire--log-reset ()
  "Clear the build log."
  (setq org-grimoire--log nil))

(defun org-grimoire--log (level msg)
  "Append a log entry with LEVEL and MSG, and echo it immediately.
LEVEL must be :info, :warn, or :error."
  (push (cons level msg) org-grimoire--log)
  (message "[%s] %s"
           (pcase level
             (:info  "INFO ")
             (:warn  "WARN ")
             (:error "ERROR"))
           msg))

(defun org-grimoire--log-summary ()
  "Print a summary of warnings and errors from the current build log."
  (let* ((entries  (reverse org-grimoire--log))
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

;;; Template Engine:

(defun org-grimoire--default-theme-dir ()
  "Return the path to the built-in default theme."
  (expand-file-name "themes/default/" org-grimoire--package-dir))

(defun org-grimoire--resolve-theme-file (filename theme-dir)
  "Return the absolute path to FILENAME, searching THEME-DIR.
Then the default theme.
 Return nil when FILENAME is not found in either location."
  (let ((user-path    (when theme-dir (expand-file-name filename theme-dir)))
        (default-path (expand-file-name filename
                                        (org-grimoire--default-theme-dir))))
    (cond
     ((and user-path (file-exists-p user-path)) user-path)
     ((file-exists-p default-path) default-path))))

(defun org-grimoire--load-template (name theme-dir)
  "Return the contents of template NAME.html, resolved via THEME-DIR.
Fall back to the built-in default theme.  Signal a user error when not found."
  (let* ((resolved (org-grimoire--resolve-theme-file
                    (concat name ".html") theme-dir))
       (path     (or resolved (user-error "Template not found: %s" name))))
  (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun org-grimoire--process-includes (template theme-dir)
  "Replace {{include file.html}} directives in TEMPLATE with file contents.
Search THEME-DIR first, then fall back to the built-in default theme."
  (replace-regexp-in-string
   "{{include \\([^}]+\\)}}"
   (lambda (match)
     (let* ((filename (match-string 1 match))
            (path     (or (org-grimoire--resolve-theme-file filename theme-dir)
                          (user-error "Include not found: %s" filename))))
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))
   template))

(defun org-grimoire--render-template (template vars theme-dir)
  "Return TEMPLATE with {{key}} placeholders replaced by values from VARS plist.
Process {{include}} directives first using THEME-DIR."
  (let ((result (org-grimoire--process-includes template theme-dir)))
    (cl-loop for (key value) on vars by #'cddr do
      (setq result
            (replace-regexp-in-string
             (concat "{{" (substring (symbol-name key) 1) "}}")
             (or value "")
             result t t)))
    result))

(defun org-grimoire--wrap-base (content title &optional url)
  "Return CONTENT wrapped in the base template, substituting TITLE.
URL is used to fill the {{url}} placeholder; it defaults to an empty string."
  (let* ((theme-dir (org-grimoire--config-get :theme))
         (base      (org-grimoire--load-template "base" theme-dir)))
    (org-grimoire--render-template base
      (list :title       title
            :site-title  (org-grimoire--config-get :site-title)
            :description (org-grimoire--config-get :description)
            :author      (org-grimoire--config-get :author)
            :base-url    (org-grimoire--config-get :base-url)
            :url         (or url "")
            :content     content)
      theme-dir)))

;;; File Utilities:

(defun org-grimoire--copy-static (static-dir output-dir)
  "Copy all files from STATIC-DIR to OUTPUT-DIR recursively."
  (when (and static-dir (file-exists-p static-dir))
    (let ((count 0))
      (dolist (file (directory-files-recursively static-dir ".*"))
        (let* ((relative (file-relative-name file static-dir))
               (dest     (expand-file-name relative output-dir)))
          (make-directory (file-name-directory dest) t)
          (copy-file file dest t)
          (setq count (1+ count))))
      (org-grimoire--log :info
                         (format "Copied %d static file(s) to %s." count output-dir)))))

(defun org-grimoire--copy-theme-static (output-dir theme-dir)
  "Copy static files from THEME-DIR into OUTPUT-DIR/static/ if present."
  (let* ((theme        (or theme-dir (org-grimoire--default-theme-dir)))
         (theme-static (expand-file-name "static" theme)))
    (when (file-exists-p theme-static)
      (org-grimoire--copy-static theme-static (expand-file-name "static" output-dir)))))

;;; Collect:

(defun org-grimoire--extract-keyword (ast keyword)
  "Return the value of KEYWORD from the Org AST."
  (org-element-map ast 'keyword
    (lambda (el)
      (when (string= (org-element-property :key el) keyword)
        (org-element-property :value el)))
    nil t))

(defun org-grimoire--reading-time-from-ast (ast &optional wpm)
  "Return an estimated reading-time string computed from AST.
WPM is the words-per-minute rate; it defaults to 200."
  (let* ((text    (org-element-interpret-data ast))
         (clean   (replace-regexp-in-string "^#\\+[A-Z_]+:.*$" "" text))
         (words   (length (split-string clean "\\W+" t)))
         (minutes (max 1 (round (/ (float words) (or wpm 200))))))
    (format "%d min read" minutes)))

(defun org-grimoire--file-to-slug (filepath)
  "Return a URL slug derived from FILEPATH."
  (file-name-sans-extension (file-name-nondirectory filepath)))

(defun org-grimoire--parse-tags (tags-string)
  "Return a list of tags parsed from TAGS-STRING."
  (when tags-string
    (split-string tags-string "[ ,]+" t "[ \t]+")))

(defun org-grimoire--collect-assets (ast source-file)
  "Return a list of absolute paths for all file: links found in AST.
Paths are resolved relative to SOURCE-FILE and filtered to those that exist."
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
  "Return the boolean interpretation of STR.
Return DEFAULT when STR is nil.
Treat \"t\", \"true\", and \"yes\" as t; anything else as nil."
  (if (null str)
      default
    (if (member (downcase str) '("t" "true" "yes"))
        t
      nil)))

(defun org-grimoire--infer-type (filepath source-dir)
  "Return the post type inferred from FILEPATH relative to SOURCE-DIR.
The type is the name of the immediate subdirectory of SOURCE-DIR."
  (let* ((relative (file-relative-name filepath source-dir))
         (parts    (split-string relative "/")))
    (when (> (length parts) 1)
      (car parts))))

(defun org-grimoire--collect-file (filepath source-dir output-dir)
  "Return a post plist by parsing the Org file at FILEPATH.
SOURCE-DIR and OUTPUT-DIR are used to compute the output path and post type."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let* ((ast      (org-element-parse-buffer))
           (title    (org-grimoire--extract-keyword ast "TITLE"))
           (date     (org-grimoire--extract-keyword ast "DATE"))
           (type     (org-grimoire--infer-type filepath source-dir))
           (draft    (org-grimoire--normalize-boolean
                      (org-grimoire--extract-keyword ast "DRAFT")))
           (listed   (org-grimoire--normalize-boolean
                      (org-grimoire--extract-keyword ast "LISTED") t))
           (tags     (org-grimoire--parse-tags
                      (org-grimoire--extract-keyword ast "TAGS")))
           (slug     (org-grimoire--file-to-slug filepath))
           (relative (file-relative-name filepath source-dir))
           (output   (expand-file-name
                      (concat (file-name-sans-extension relative) ".html")
                      output-dir))
           (assets   (org-grimoire--collect-assets ast filepath)))
      (list :title        title
            :date         date
            :type         type
            :draft        draft
            :listed       listed
            :tags         tags
            :slug         slug
            :source       filepath
            :reading-time (when (org-grimoire--config-get :reading-time)
                            (org-grimoire--reading-time-from-ast ast))
            :output       output
            :assets       assets))))

(defun org-grimoire--collect (source-dir output-dir)
  "Return a list of post plists by scanning SOURCE-DIR recursively.
OUTPUT-DIR is used to compute output paths.  Draft posts and files placed
directly in SOURCE-DIR with no type subdirectory are skipped."
  (delq nil
        (mapcar (lambda (f)
                  (let ((post (org-grimoire--collect-file f source-dir output-dir)))
                    (cond
                     ((plist-get post :draft)
                      (org-grimoire--log :info (format "Skipping draft: %s" f))
                      nil)
                     ((null (plist-get post :type))
                      (org-grimoire--log :warn (format "No type directory, skipping: %s" f))
                      nil)
                     (t post))))
                (directory-files-recursively source-dir "\\.org$"))))

(defun org-grimoire--sort-posts-by-date (posts)
  "Return POSTS sorted by date, newest first."
  (sort (copy-sequence posts)
        (lambda (a b)
          (string> (or (plist-get a :date) "")
                   (or (plist-get b :date) "")))))

;;; Render:

(defun org-grimoire--org-to-html (filepath)
  "Return the HTML body string produced by exporting the Org file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath)
    (setq-local tab-width 8)
    (org-mode)
    (org-export-as 'html nil nil t
                   '(:with-toc nil
                     :with-title nil
                     :section-numbers nil))))

(defun org-grimoire--tags-html (tags)
  "Return an HTML string representing TAGS as a linked grimoire-tags div."
  (if tags
      (concat "<div class=\"grimoire-tags\">"
              (mapconcat
               (lambda (tag)
                 (format "<a class=\"grimoire-tag\" href=\"/tags/%s.html\">%s</a>"
                         (org-grimoire--tag-to-slug tag) tag))
               tags " ")
              "</div>")
    ""))

(defun org-grimoire--post-site-url (post)
  "Return the root-relative URL for POST."
  (concat "/"
          (file-relative-name (plist-get post :output)
                              (org-grimoire--config-get :output))))

(defun org-grimoire--render-post (post)
  "Return the full HTML string for POST rendered with its type template."
  (let* ((theme-dir (org-grimoire--config-get :theme))
         (type      (or (plist-get post :type) "page"))
         (title     (or (plist-get post :title) ""))
         (template  (org-grimoire--load-template type theme-dir))
         (content   (org-grimoire--org-to-html (plist-get post :source)))
         (date      (or (plist-get post :date) ""))
         (tags      (plist-get post :tags))
         (url       (org-grimoire--post-site-url post))
         (inner     (org-grimoire--render-template template
                      (list :title        title
                            :content      content
                            :date         date
                            :tags         (org-grimoire--tags-html tags)
                            :reading-time (or (plist-get post :reading-time) "")
                            :slug         (plist-get post :slug))
                      theme-dir)))
    (org-grimoire--wrap-base inner title url)))

(defun org-grimoire--copy-assets (assets source-file output-file)
  "Copy ASSETS into the directory of OUTPUT-FILE.
Asset paths are resolved relative to SOURCE-FILE and mirrored in OUTPUT-FILE."
  (let ((source-dir (file-name-directory source-file))
        (output-dir (file-name-directory output-file)))
    (dolist (asset assets)
      (let* ((relative (file-relative-name asset source-dir))
             (dest     (expand-file-name relative output-dir)))
        (make-directory (file-name-directory dest) t)
        (copy-file asset dest t)))))

(defun org-grimoire--write-post (post)
  "Render POST and write it to its output path."
  (let* ((output (plist-get post :output))
         (html   (org-grimoire--render-post post))
         (assets (plist-get post :assets)))
    (make-directory (file-name-directory output) t)
    (write-region html nil output)
    (when assets
      (org-grimoire--copy-assets assets (plist-get post :source) output))
    (org-grimoire--log :info (format "Rendered: %s" output))))

(defun org-grimoire--render-all (posts)
  "Render all POSTS to their output paths."
  (dolist (post posts)
    (condition-case err
        (org-grimoire--write-post post)
      (error (org-grimoire--log :warn (format "Failed to render %s: %s"
                                              (plist-get post :source)
                                              (error-message-string err)))))))

;;; Index and Pagination:

(defun org-grimoire--render-post-item (post theme-dir)
  "Return the HTML string for POST rendered as a list item using THEME-DIR."
  (let* ((title (or (plist-get post :title) "Untitled"))
         (date  (or (plist-get post :date) ""))
         (tags  (plist-get post :tags))
         (url   (org-grimoire--post-site-url post)))
    (org-grimoire--render-template
     (org-grimoire--load-template "partials/post-item" theme-dir)
     (list :title title
           :url   url
           :date  date
           :tags  (if tags (string-join tags ", ") ""))
     theme-dir)))

(defun org-grimoire--render-post-list (posts theme-dir)
  "Return an HTML string of concatenated list items for POSTS using THEME-DIR."
  (mapconcat (lambda (p) (org-grimoire--render-post-item p theme-dir))
             posts "\n"))

(defun org-grimoire--paginate (posts per-page)
  "Return POSTS split into a list of pages of PER-PAGE items each."
  (let (pages current (count 0))
    (dolist (post posts)
      (push post current)
      (setq count (1+ count))
      (when (= count per-page)
        (push (nreverse current) pages)
        (setq current nil count 0)))
    (when current
      (push (nreverse current) pages))
    (nreverse pages)))

(defun org-grimoire--pagination-html (current-page total-pages theme-dir)
  "Return pagination HTML for CURRENT-PAGE of TOTAL-PAGES using THEME-DIR."
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

(defun org-grimoire--write-index-page (posts page-num total-pages output-dir)
  "Write index page PAGE-NUM of TOTAL-PAGES to OUTPUT-DIR.
POSTS is the list of posts to render on this page."
  (let* ((theme-dir (org-grimoire--config-get :theme))
         (filename  (if (= page-num 1) "index.html"
                      (format "page-%d.html" page-num)))
         (output    (expand-file-name filename output-dir))
         (template  (org-grimoire--load-template "index" theme-dir))
         (inner     (org-grimoire--render-template template
                      (list :site-title  (org-grimoire--config-get :site-title)
                            :description (org-grimoire--config-get :description)
                            :posts       (org-grimoire--render-post-list posts theme-dir)
                            :pagination  (org-grimoire--pagination-html
                                          page-num total-pages theme-dir))
                      theme-dir))
         (html      (org-grimoire--wrap-base inner (org-grimoire--config-get :site-title))))
    (make-directory output-dir t)
    (write-region html nil output)
    (org-grimoire--log :info (format "Rendered index: %s" output))))

(defun org-grimoire--generate-index (all-posts output-dir)
  "Generate paginated index pages from listed posts in ALL-POSTS to OUTPUT-DIR."
  (condition-case err
      (let* ((per-page (or (org-grimoire--config-get :per-page) 10))
             (posts    (org-grimoire--sort-posts-by-date
                        (cl-remove-if-not
                         (lambda (p) (plist-get p :listed)) all-posts)))
             (pages    (org-grimoire--paginate posts per-page))
             (total    (length pages)))
        (if (null posts)
            (org-grimoire--log :warn "No listed posts found.")
          (cl-loop for page-posts in pages
                   for i from 1
                   do (org-grimoire--write-index-page
                       page-posts i total output-dir))))
    (error (org-grimoire--log :warn (format "Failed to generate index: %s"
                                            (error-message-string err))))))

;;; Tags:

(defun org-grimoire--collect-tags (posts)
  "Return a hash table mapping each tag string to its list of posts from POSTS."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (post posts)
      (dolist (tag (plist-get post :tags))
        (puthash tag (cons post (gethash tag tags '())) tags)))
    tags))

(defun org-grimoire--tag-to-slug (tag)
  "Return a URL-safe slug for TAG."
  (replace-regexp-in-string "-+" "-"
    (replace-regexp-in-string "[^a-z0-9]" "-" (downcase tag))))

(defun org-grimoire--render-tag-item (tag count theme-dir)
  "Return HTML for TAG with post COUNT using tag-item partial from THEME-DIR."
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
         (template (org-grimoire--load-template "partials/tag-index" theme-dir))
         (inner    (org-grimoire--render-template template
                     (list :title      (concat "Tag: " tag)
                           :posts      (org-grimoire--render-post-list
                                        sorted theme-dir)
                           :pagination "")
                     theme-dir))
         (html     (org-grimoire--wrap-base inner (concat "Tag: " tag))))
    (make-directory dir t)
    (write-region html nil output)
    (org-grimoire--log :info (format "Rendered tag page: %s" output))))

(defun org-grimoire--write-tags-index (tags-table output-dir theme-dir)
  "Write the tags index page from TAGS-TABLE to OUTPUT-DIR using THEME-DIR."
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
         (html        (org-grimoire--wrap-base inner "Tags")))
    (make-directory dir t)
    (write-region html nil output)
    (org-grimoire--log :info (format "Rendered tags index: %s" output))))

(defun org-grimoire--generate-tags (posts output-dir)
  "Generate all tag pages and the tags index from POSTS to OUTPUT-DIR."
  (condition-case err
      (let ((tags      (org-grimoire--collect-tags posts))
            (theme-dir (org-grimoire--config-get :theme)))
        (maphash (lambda (tag tag-posts)
                   (condition-case tag-err
                       (org-grimoire--write-tag-page
                        tag tag-posts output-dir theme-dir)
                     (error (org-grimoire--log
                             :warn
                             (format "Failed to render tag page '%s': %s"
                                     tag (error-message-string tag-err))))))
                 tags)
        (org-grimoire--write-tags-index tags output-dir theme-dir)
        (org-grimoire--log :info (format "Generated %d tag page(s)."
                                         (hash-table-count tags))))
    (error (org-grimoire--log :error (format "Failed to generate tags: %s"
                                             (error-message-string err))))))

;;; Feeds:

(defun org-grimoire--parse-date (date-string)
  "Return an internal time value parsed from DATE-STRING (yyyy-mm-dd format)."
  (when date-string
    (date-to-time (concat date-string " 00:00:00"))))

(defun org-grimoire--rss-date (date-string)
  "Return an RFC 822 date string derived from DATE-STRING (yyyy-mm-dd format)."
  (when-let ((time (org-grimoire--parse-date date-string)))
    (format-time-string "%a, %d %b %Y %T +0000" time t)))

(defun org-grimoire--atom-date (date-string)
  "Return an RFC 3339 date string derived from DATE-STRING (yyyy-mm-dd format)."
  (when-let ((time (org-grimoire--parse-date date-string)))
    (format-time-string "%FT%TZ" time t)))

(defun org-grimoire--escape-xml (str)
  "Return STR with XML special characters escaped.
Escapes &, <, >, and \" in that order to avoid double-escaping."
  (when str
    (replace-regexp-in-string
     "\"" "&quot;"
     (replace-regexp-in-string
      ">" "&gt;"
      (replace-regexp-in-string
       "<" "&lt;"
       (replace-regexp-in-string "&" "&amp;" str))))))

(defun org-grimoire--post-url (post base-url output-dir)
  "Return the full URL for POST given BASE-URL and OUTPUT-DIR."
  (let* ((output   (plist-get post :output))
         (relative (file-relative-name output output-dir)))
    (concat (string-trim-right base-url "/") "/" relative)))

(defun org-grimoire--rss-item (post base-url output-dir)
  "Return an RSS 2.0 <item> XML string for POST using BASE-URL and OUTPUT-DIR."
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
  "Return an RSS 2.0 feed XML string for POSTS.
BASE-URL and OUTPUT-DIR are used to build item URLs.
SITE-TITLE and SITE-DESCRIPTION supply the channel metadata."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<rss version=\"2.0\">\n"
   "<channel>\n"
   (format "  <title>%s</title>\n" (org-grimoire--escape-xml site-title))
   (format "  <link>%s</link>\n" base-url)
   (format "  <description>%s</description>\n"
           (org-grimoire--escape-xml site-description))
   (format "  <lastBuildDate>%s</lastBuildDate>\n"
           (format-time-string "%a, %d %b %Y %T +0000" nil t))
   (mapconcat (lambda (p) (org-grimoire--rss-item p base-url output-dir))
              posts "")
   "</channel>\n"
   "</rss>\n"))

(defun org-grimoire--atom-entry (post base-url output-dir)
  "Return an Atom feed <entry> XML string for POST using BASE-URL and OUTPUT-DIR."
  (let ((title  (org-grimoire--escape-xml (plist-get post :title)))
        (url    (org-grimoire--post-url post base-url output-dir))
        (date   (org-grimoire--atom-date (plist-get post :date)))
        (author (org-grimoire--config-get :author)))
    (concat
     "  <entry>\n"
     (format "    <title>%s</title>\n" (or title "Untitled"))
     (format "    <link href=\"%s\"/>\n" url)
     (format "    <id>%s</id>\n" url)
     (when date   (format "    <updated>%s</updated>\n" date))
     (when author (format "    <author><name>%s</name></author>\n"
                          (org-grimoire--escape-xml author)))
     "  </entry>\n")))

(defun org-grimoire--generate-atom (posts base-url output-dir site-title)
  "Return an Atom feed XML string for POSTS.
BASE-URL and OUTPUT-DIR are used to build entry URLs.
SITE-TITLE supplies the feed title."
  (let ((author (org-grimoire--config-get :author)))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
     (format "  <title>%s</title>\n" (org-grimoire--escape-xml site-title))
     (format "  <link href=\"%s\"/>\n" base-url)
     (format "  <link rel=\"self\" href=\"%s/atom.xml\"/>\n"
             (string-trim-right base-url "/"))
     (format "  <id>%s/</id>\n" (string-trim-right base-url "/"))
     (format "  <updated>%s</updated>\n"
             (format-time-string "%FT%TZ" nil t))
     (when author (format "  <author><name>%s</name></author>\n"
                          (org-grimoire--escape-xml author)))
     (mapconcat (lambda (p) (org-grimoire--atom-entry p base-url output-dir))
                posts "")
     "</feed>\n")))

(defun org-grimoire--generate-feeds (posts output-dir)
  "Write rss.xml and atom.xml to OUTPUT-DIR generated from POSTS."
  (condition-case err
      (let* ((base-url    (org-grimoire--config-get :base-url))
             (site-title  (org-grimoire--config-get :site-title))
             (description (org-grimoire--config-get :description))
             (feed-posts  (org-grimoire--sort-posts-by-date posts))
             (rss-path    (expand-file-name "rss.xml" output-dir))
             (atom-path   (expand-file-name "atom.xml" output-dir)))
        (write-region
         (org-grimoire--generate-rss
          feed-posts base-url output-dir site-title description)
         nil rss-path)
        (write-region
         (org-grimoire--generate-atom feed-posts base-url output-dir site-title)
         nil atom-path)
        (org-grimoire--log :info "Generated feeds: rss.xml, atom.xml"))
    (error (org-grimoire--log :error (format "Failed to generate feeds: %s"
                                             (error-message-string err))))))

(defun org-grimoire--generate-sitemap (posts output-dir)
  "Write sitemap.xml to OUTPUT-DIR generated from POSTS."
  (condition-case err
      (let* ((base-url (org-grimoire--config-get :base-url))
             (output   (expand-file-name "sitemap.xml" output-dir))
             (urls     (mapconcat
                        (lambda (p)
                          (format
                           "  <url>\n    <loc>%s</loc>\n    <lastmod>%s</lastmod>\n  </url>"
                           (org-grimoire--post-url p base-url output-dir)
                           (or (plist-get p :date) "")))
                        posts "\n")))
        (write-region
         (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                 "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
                 urls "\n"
                 "</urlset>\n")
         nil output)
        (org-grimoire--log :info "Generated sitemap.xml"))
    (error (org-grimoire--log :error (format "Failed to generate sitemap: %s"
                                             (error-message-string err))))))

;;; Public API:

(defun org-grimoire--resolve-config (args)
  "Return a resolved configuration plist derived from ARGS.
Expand :base-dir and derive :source, :output, and :static when absent.
Resolve :theme relative to the themes/ subdirectory of :base-dir."
  (let ((base  (plist-get args :base-dir))
        (theme (plist-get args :theme))
        (final args))
    (when base
      (setq base (expand-file-name base))
      (when theme
        (setq final (plist-put final :theme
                               (expand-file-name theme
                                            (expand-file-name "themes" base)))))
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
  "Validate the required fields in CONFIG, signaling a user error if invalid."
  (let ((source     (plist-get config :source))
        (output     (plist-get config :output))
        (base-url   (plist-get config :base-url))
        (site-title (plist-get config :site-title))
        (per-page   (or (plist-get config :per-page) 10)))
    (unless (and source (file-directory-p source))
      (user-error "Invalid or missing :source directory"))
    (unless output
      (user-error "Missing :output directory"))
    (unless (and base-url (string-match-p "\\`https?://" base-url))
      (user-error "Missing or invalid :base-url (must start with http/https)"))
    (unless (and site-title (not (string-empty-p site-title)))
      (user-error "Missing or empty :site-title"))
    (unless (and (integerp per-page) (> per-page 0))
      (user-error ":per-page must be a positive integer"))))

;;;###autoload
(defun org-grimoire-setup (name &rest args)
  "Register a site configuration NAME with ARGS.
Required keys: :base-dir, :base-url, :site-title.
Optional keys: :description, :author, :theme, :per-page, :reading-time.
Optional path overrides: :source, :output, :static."
  (puthash name (org-grimoire--resolve-config args) org-grimoire--sites))

;;;###autoload
(defun org-grimoire-build (name)
  "Build the site registered as NAME."
  (interactive
   (list (completing-read "Site: " (hash-table-keys org-grimoire--sites) nil t)))
  (org-grimoire--log-reset)
  (unless (gethash name org-grimoire--sites)
    (user-error "No site configured with name: %s" name))
  (let* ((org-grimoire--current-site name)
         (source    (org-grimoire--config-get :source))
         (output    (org-grimoire--config-get :output))
         (static    (org-grimoire--config-get :static))
         (theme-dir (org-grimoire--config-get :theme)))
    (org-grimoire--validate-config (gethash name org-grimoire--sites))
    (org-grimoire--log :info "Build started")
    (org-grimoire--log :info (format "Source: %s" source))
    (org-grimoire--log :info (format "Output: %s" output))
    (condition-case err
        (let ((posts (org-grimoire--collect source output)))
          (org-grimoire--log :info (format "Collected %d post(s)." (length posts)))
          (org-grimoire--copy-static static (expand-file-name "static" output))
          (org-grimoire--copy-theme-static output theme-dir)
          (org-grimoire--render-all posts)
          (org-grimoire--generate-index posts output)
          (org-grimoire--generate-tags posts output)
          (org-grimoire--generate-feeds posts output)
          (org-grimoire--generate-sitemap posts output)
          (org-grimoire--log :info "Build complete.")
          (org-grimoire--log-summary))
      (error
       (org-grimoire--log :error (error-message-string err))
       (org-grimoire--log-summary)))))

;;;###autoload
(defun org-grimoire-new (name)
  "Interactively create a new post for the site registered as NAME."
  (interactive "sSite name: ")
  (let* ((config (or (gethash name org-grimoire--sites)
                     (user-error "No site configured with name: %s" name)))
         (source (plist-get config :source))
         (dirs   (cl-remove-if-not #'file-directory-p
                                   (directory-files source t "^[^.]")))
         (types  (mapcar #'file-name-nondirectory dirs))
         (type   (completing-read "Type: " types nil t))
         (title  (read-string "Title: "))
         (tags   (read-string "Tags (comma separated): "))
         (date   (format-time-string "%F"))
         (slug   (replace-regexp-in-string "[^a-z0-9]" "-" (downcase title)))
         (dir    (expand-file-name type source))
         (file   (expand-file-name (concat slug ".org") dir)))
    (make-directory dir t)
    (write-region
     (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: %s\n#+DRAFT: t\n\n"
             title date tags)
     nil file)
    (find-file file)))

;;;###autoload
(defun org-grimoire-init (name base-dir base-url)
  "Initialize a new org-grimoire site NAME at BASE-DIR with BASE-URL."
  (interactive
   (list (read-string "Site name: ")
         (read-directory-name "Base directory: ")
         (read-string "Base URL (e.g. https://example.com): ")))
  (let* ((base    (expand-file-name base-dir))
         (content (expand-file-name "content" base))
         (posts   (expand-file-name "post" content))
         (pages   (expand-file-name "page" content))
         (static  (expand-file-name "static" base)))
    (dolist (dir (list base content posts pages static))
      (make-directory dir t))
        (write-region
        (concat "#+TITLE: My First Post\n"
                "#+DATE: " (format-time-string "%F") "\n"
                "#+TAGS: emacs\n"
                "#+DRAFT: false\n\n"
                "Hello, world!\n")
        nil (expand-file-name "my-first-post.org" posts))
    (write-region
     "#+TITLE: About\n#+LISTED: false\n\nThis is the about page.\n"
     nil (expand-file-name "about.org" pages))
        (message "Done! Add this to your Emacs config:\n\n%s"
                (format
                 "(org-grimoire-setup \"%s\"\n  :base-dir    \"%s\"\n  :base-url    \"%s\"\n  :site-title  \"%s\"\n  :description \"My site\")\n"
                 name base base-url name))))

;;;###autoload
(defun org-grimoire-serve (name)
  "Serve the built site for NAME using simple-httpd if available."
  (interactive
   (list (completing-read "Site:" (hash-table-keys org-grimoire--sites) nil t)))
  (if (not (require 'simple-httpd nil t))
      (user-error "Simple-httpd is not installed")
    (let* ((config (or (gethash name org-grimoire--sites)
                       (user-error "No site configured with name: %s" name)))
           (output (plist-get config :output)))
      (setq httpd-root output)
      (httpd-start)
      (message "Serving %s at http://localhost:%d" output httpd-port))))
(provide 'org-grimoire)
;;; org-grimoire.el ends here
