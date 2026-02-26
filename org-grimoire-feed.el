;;; org-grimoire-feed.el --- RSS/Atom feed generation -*- lexical-binding: t -*-

;;; Commentary:
;; Generates RSS 2.0 and Atom feeds from collected posts.

;;; Code:

(require 'org-grimoire-collect)

;; --- Helpers ---

(defun grimoire--rss-date (date-string)
  "Convert DATE-STRING (yyyy-mm-dd) to RFC 822 format for RSS."
  (when date-string
    (let ((time (date-to-time (concat date-string " 00:00:00"))))
      (format-time-string "%a, %d %b %Y %H:%M:%S +0000" time))))

(defun grimoire--atom-date (date-string)
  "Convert DATE-STRING (yyyy-mm-dd) to RFC 3339 format for Atom."
  (when date-string
    (let ((time (date-to-time (concat date-string " 00:00:00"))))
      (format-time-string "%Y-%m-%dT%H:%M:%SZ" time))))

(defun grimoire--escape-xml (str)
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

(defun grimoire--post-url (post base-url output-dir)
  "Generate full URL for POST given BASE-URL and OUTPUT-DIR."
  (let* ((output   (plist-get post :output))
         (relative (file-relative-name output output-dir)))
    (concat (string-trim-right base-url "/") "/" relative)))

;; --- RSS 2.0 ---

(defun grimoire--rss-item (post base-url output-dir)
  "Generate an RSS item XML string for POST."
  (let ((title (grimoire--escape-xml (plist-get post :title)))
        (url   (grimoire--post-url post base-url output-dir))
        (date  (grimoire--rss-date (plist-get post :date)))
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
                            (grimoire--escape-xml tag)))
                  tags ""))
     "  </item>\n")))

(defun grimoire--generate-rss (posts base-url output-dir site-title site-description)
  "Generate RSS 2.0 feed XML string from POSTS."
  (let ((sorted (grimoire--sort-posts-by-date posts)))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<rss version=\"2.0\">\n"
     "<channel>\n"
     (format "  <title>%s</title>\n" (grimoire--escape-xml site-title))
     (format "  <link>%s</link>\n" base-url)
     (format "  <description>%s</description>\n"
             (grimoire--escape-xml site-description))
     (format "  <lastBuildDate>%s</lastBuildDate>\n"
             (format-time-string "%a, %d %b %Y %H:%M:%S +0000"))
     (mapconcat (lambda (p) (grimoire--rss-item p base-url output-dir))
                sorted "")
     "</channel>\n"
     "</rss>\n")))

;; --- Atom ---

(defun grimoire--atom-entry (post base-url output-dir)
  "Generate an Atom entry XML string for POST."
  (let ((title (grimoire--escape-xml (plist-get post :title)))
        (url   (grimoire--post-url post base-url output-dir))
        (date  (grimoire--atom-date (plist-get post :date))))
    (concat
     "  <entry>\n"
     (format "    <title>%s</title>\n" (or title "Untitled"))
     (format "    <link href=\"%s\"/>\n" url)
     (format "    <id>%s</id>\n" url)
     (when date (format "    <updated>%s</updated>\n" date))
     "  </entry>\n")))

(defun grimoire--generate-atom (posts base-url output-dir site-title)
  "Generate Atom feed XML string from POSTS."
  (let ((sorted (grimoire--sort-posts-by-date posts)))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
     (format "  <title>%s</title>\n" (grimoire--escape-xml site-title))
     (format "  <link href=\"%s\"/>\n" base-url)
     (format "  <id>%s</id>\n" base-url)
     (format "  <updated>%s</updated>\n"
             (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
     (mapconcat (lambda (p) (grimoire--atom-entry p base-url output-dir))
                sorted "")
     "</feed>\n")))

;; --- Public API ---

(defun grimoire-generate-feeds (posts output-dir base-url site-title site-description)
  "Generate RSS and Atom feeds from POSTS to OUTPUT-DIR."
  (condition-case err
      (let* ((feed-posts (grimoire--sort-posts-by-date
                          (cl-remove-if-not
                           (lambda (p)
                             (let ((type (plist-get p :type)))
                               (when type
                                 (let ((config (gethash type grimoire--types)))
                                   (plist-get config :feed)))))
                           posts)))
             (rss-path  (expand-file-name "rss.xml" output-dir))
             (atom-path (expand-file-name "atom.xml" output-dir)))
        (write-region
         (grimoire--generate-rss feed-posts base-url output-dir site-title site-description)
         nil rss-path)
        (write-region
         (grimoire--generate-atom feed-posts base-url output-dir site-title)
         nil atom-path)
        (message "Generated feeds: rss.xml, atom.xml"))
    (error (message "WARNING: Failed to generate feeds: %s"
                    (error-message-string err)))))

(provide 'org-grimoire-feed)
;;; org-grimoire-feed.el ends here
