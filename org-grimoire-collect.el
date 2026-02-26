;;; org-grimoire-collect.el --- Collect and parse org files -*- lexical-binding: t -*-

;;; Commentary:
;; Scans source directory for .org files and extracts metadata.

;;; Code:
(require 'org)
(require 'org-element)

;; --- Data model ---
;; A post is a plist:
;; (:title "My Post"
;;  :date "2024-01-15"
;;  :type "post"
;;  :tags ("emacs" "lisp")
;;  :slug "my-post"
;;  :source "/path/to/file.org"
;;  :output "/path/to/output/my-post.html"
;;  :assets ("/path/to/images/screenshot.png" ...))


(defun grimoire--extract-keyword (ast keyword)
  "Extract value of KEYWORD from org AST."
  (org-element-map ast 'keyword
    (lambda (el)
      (when (string= (org-element-property :key el) keyword)
        (org-element-property :value el)))
    nil t))

(defun grimoire--file-to-slug (filepath)
  "Convert FILEPATH to a URL slug."
  (file-name-sans-extension
   (file-name-nondirectory filepath)))

(defun grimoire--parse-tags (tags-string)
  "Parse TAGS-STRING into a list of tags."
  (when tags-string
    (split-string tags-string "[ ,]+" t "[ \t]+")))

(defun grimoire--collect-assets (ast source-file)
  "Extract all file: links from AST, resolve relative to SOURCE-FILE.
Returns list of absolute paths that actually exist on disk."
  (let ((source-dir (file-name-directory source-file)))
    (delq nil
          (org-element-map ast 'link
            (lambda (el)
              (when (string= (org-element-property :type el) "file")
                (let* ((path (org-element-property :path el))
                       (absolute (expand-file-name path source-dir)))
                  (when (file-exists-p absolute)
                    absolute))))))))

(defun grimoire--collect-file (filepath source-dir output-dir)
  "Parse a single org FILEPATH and return a post plist.
SOURCE-DIR and OUTPUT-DIR used to compute output path."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let* ((ast    (org-element-parse-buffer))
           (title  (grimoire--extract-keyword ast "TITLE"))
           (date   (grimoire--extract-keyword ast "DATE"))
           (type   (grimoire--extract-keyword ast "TYPE"))
           (draft  (grimoire--extract-keyword ast "DRAFT"))
           (listed (let ((v (grimoire--extract-keyword ast "LISTED")))
                     (not (equal v "nil"))))
           (tags   (grimoire--parse-tags
                    (grimoire--extract-keyword ast "TAGS")))
           (slug   (grimoire--file-to-slug filepath))
           (relative (file-relative-name filepath source-dir))
           (output (expand-file-name
                    (concat (file-name-sans-extension relative) ".html")
                    output-dir))
           (assets (grimoire--collect-assets ast filepath)))
      (unless type
        (message "WARNING: No #+TYPE in %s" filepath))
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

(defun grimoire-collect (source-dir output-dir)
  "Scan SOURCE-DIR recursively, return list of post plists.
Skips files with #+DRAFT: t or missing #+TYPE:."
  (let ((org-files (directory-files-recursively source-dir "\\.org$")))
    (delq nil
          (mapcar (lambda (f)
                    (let ((post (grimoire--collect-file f source-dir output-dir)))
                      (unless (or (equal (plist-get post :draft) "t")
                                  (null (plist-get post :type)))
                        post)))
                  org-files))))

(provide 'org-grimoire-collect)
;;; org-grimoire-collect.el ends here
