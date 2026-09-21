;;; org-grimoire-test.el --- Tests for org-grimoire -*- lexical-binding: t; -*-

;; Copyright (C) 2026 spiperac

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; ERT suite for org-grimoire.  Run from the repository root with:
;;
;;   emacs -Q --batch -l ert -l org-grimoire.el \
;;         -l test/org-grimoire-test.el -f ert-run-tests-batch-and-exit
;;
;; Each test builds a throwaway site in its own temporary directory,
;; because builds never clean their output directory and results would
;; otherwise leak between tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-grimoire)

(defvar org-grimoire-test--counter 0
  "Counter used to give each fixture build a unique site name.")

(defun org-grimoire-test--slurp (file)
  "Return the contents of FILE, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun org-grimoire-test--make-site ()
  "Create a fixture site in a fresh temporary directory.
Return the base directory.  The site has three published posts:
alpha (tagged emacs), bravo (tagged ctf pwn) and charlie (tagged CTF),
the last of which exercises case-insensitive tag matching."
  (let* ((base (make-temp-file "org-grimoire-test-" t))
         (post (expand-file-name "content/post" base)))
    (make-directory post t)
    (dolist (spec '(("alpha"   "2026-01-01" "emacs")
                    ("bravo"   "2026-01-02" "ctf pwn")
                    ("charlie" "2026-01-03" "CTF")))
      (cl-destructuring-bind (slug date tags) spec
        (write-region
         (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: %s\n#+DRAFT: false\n\nBody text.\n"
                 slug date tags)
         nil (expand-file-name (concat slug ".org") post))))
    base))

(defun org-grimoire-test--build (base &rest args)
  "Build the fixture site at BASE with ARGS added to its configuration.
Return the output directory."
  (let* ((name   (format "test-%d" (cl-incf org-grimoire-test--counter)))
         (output (expand-file-name (concat "out-" name) base)))
    (apply #'org-grimoire-setup name
           :base-dir   base
           :base-url   "https://example.com"
           :site-title "Fixture"
           :output     output
           args)
    (org-grimoire-build name)
    output))

(defun org-grimoire-test--index (output)
  "Return the concatenated text of every index page under OUTPUT."
  (mapconcat #'org-grimoire-test--slurp
             (append (list (expand-file-name "index.html" output))
                     (file-expand-wildcards
                      (expand-file-name "page-*.html" output)))
             "\n"))

(defun org-grimoire-test--indexed-p (output slug)
  "Return non-nil when SLUG is linked from any index page under OUTPUT."
  (string-match-p (regexp-quote (concat "/post/" slug ".html"))
                  (or (org-grimoire-test--index output) "")))

(defun org-grimoire-test--warned-p (pattern)
  "Return non-nil when the last build logged a warning matching PATTERN."
  (cl-some (lambda (entry)
             (and (eq (car entry) :warn)
                  (string-match-p pattern (cdr entry))))
           org-grimoire--log))


;;; :index-exclude-tags

(ert-deftest org-grimoire-test-index-lists-everything-by-default ()
  "Without :index-exclude-tags every published post is listed."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base)))
    (dolist (slug '("alpha" "bravo" "charlie"))
      (should (org-grimoire-test--indexed-p out slug)))))

(ert-deftest org-grimoire-test-index-exclude-tags-list ()
  "A list of tags keeps matching posts off the index."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base :index-exclude-tags '("ctf"))))
    (should (org-grimoire-test--indexed-p out "alpha"))
    (should-not (org-grimoire-test--indexed-p out "bravo"))
    (should-not (org-grimoire-test--indexed-p out "charlie"))))

(ert-deftest org-grimoire-test-index-exclude-tags-accepts-bare-string ()
  "A bare string behaves like a single-element list."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base :index-exclude-tags "ctf")))
    (should (org-grimoire-test--indexed-p out "alpha"))
    (should-not (org-grimoire-test--indexed-p out "bravo"))))

(ert-deftest org-grimoire-test-index-exclude-tags-ignores-case ()
  "Tag matching is case-insensitive in both directions."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base :index-exclude-tags '("CTF"))))
    (should (org-grimoire-test--indexed-p out "alpha"))
    (should-not (org-grimoire-test--indexed-p out "bravo"))
    (should-not (org-grimoire-test--indexed-p out "charlie"))))

(ert-deftest org-grimoire-test-excluded-posts-are-still-rendered ()
  "Excluded posts keep their own pages, tag pages and feed entries."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base :index-exclude-tags '("ctf"))))
    (should (file-exists-p (expand-file-name "post/bravo.html" out)))
    (should (string-match-p "bravo"
                            (or (org-grimoire-test--slurp
                                 (expand-file-name "tags/ctf.html" out)) "")))
    (should (string-match-p "bravo"
                            (or (org-grimoire-test--slurp
                                 (expand-file-name "rss.xml" out)) "")))))

(ert-deftest org-grimoire-test-index-exclude-tags-repaginates ()
  "Pagination reflects the filtered post count, not the raw one."
  (let* ((base (org-grimoire-test--make-site))
         (all  (org-grimoire-test--build base :per-page 2))
         (some (org-grimoire-test--build base :per-page 2
                                         :index-exclude-tags '("ctf"))))
    (should (file-exists-p (expand-file-name "page-2.html" all)))
    (should-not (file-exists-p (expand-file-name "page-2.html" some)))))

(ert-deftest org-grimoire-test-excluding-every-tag-writes-no-index ()
  "Excluding every tag warns instead of writing an empty index."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build
                base :index-exclude-tags '("emacs" "ctf" "pwn"))))
    (should (org-grimoire-test--warned-p "excluded by :index-exclude-tags"))
    (should-not (file-exists-p (expand-file-name "index.html" out)))))

(ert-deftest org-grimoire-test-invalid-exclude-tags-is-rejected ()
  "A non-string entry is refused before anything is written."
  (let ((base (org-grimoire-test--make-site)))
    (should-error (org-grimoire-test--build base :index-exclude-tags '(ctf))
                  :type 'user-error)))


;;; Tags

(ert-deftest org-grimoire-test-tags-differing-only-in-case-merge ()
  "Tags differing only in case share one page instead of overwriting it.
The fixture tags bravo with ctf and charlie with CTF, both of which slug
to tags/ctf.html."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base))
         (page (org-grimoire-test--slurp
                (expand-file-name "tags/ctf.html" out))))
    (should (string-match-p "bravo" (or page "")))
    (should (string-match-p "charlie" (or page "")))))

(ert-deftest org-grimoire-test-tags-index-lists-one-entry-per-slug ()
  "The tags index lists a case-folded tag once, not once per spelling."
  (let* ((base  (org-grimoire-test--make-site))
         (out   (org-grimoire-test--build base))
         (index (or (org-grimoire-test--slurp
                     (expand-file-name "tags/index.html" out)) ""))
         (hits  0)
         (start 0))
    (while (string-match "/tags/ctf\\.html" index start)
      (setq hits  (1+ hits)
            start (match-end 0)))
    (should (= hits 1))))


;;; Theme resolution

(ert-deftest org-grimoire-test-theme-static-falls-back-to-default ()
  "A theme directory without static files falls back to the default theme."
  (let* ((base (org-grimoire-test--make-site))
         (out  (org-grimoire-test--build base :theme "nope")))
    (should (file-exists-p (expand-file-name "static/css/style.css" out)))))

(ert-deftest org-grimoire-test-missing-theme-warns ()
  "Pointing :theme at a directory that does not exist logs a warning."
  (let ((base (org-grimoire-test--make-site)))
    (org-grimoire-test--build base :theme "nope")
    (should (org-grimoire-test--warned-p "Theme directory not found"))))

(ert-deftest org-grimoire-test-theme-without-static-keeps-site-files ()
  "A theme that exists but ships no static files overwrites nothing.
Site-level static files must survive, since the theme contributes none."
  (let* ((base (org-grimoire-test--make-site))
         (css  (expand-file-name "static/css/style.css" base)))
    (make-directory (expand-file-name "themes/bare" base) t)
    (make-directory (file-name-directory css) t)
    (write-region "/* SITE */" nil css)
    (let ((out (org-grimoire-test--build base :theme "bare")))
      (should (equal "/* SITE */"
                     (org-grimoire-test--slurp
                      (expand-file-name "static/css/style.css" out)))))))

(ert-deftest org-grimoire-test-custom-theme-static-wins ()
  "A theme carrying its own static files is used verbatim."
  (let* ((base  (org-grimoire-test--make-site))
         (theme (expand-file-name "themes/mine/static" base)))
    (make-directory theme t)
    (write-region "body{}" nil (expand-file-name "mine.css" theme))
    (let ((out (org-grimoire-test--build base :theme "mine")))
      (should (file-exists-p (expand-file-name "static/mine.css" out)))
      (should-not (file-exists-p (expand-file-name "static/css/style.css" out))))))


;;; Scaffolding

(ert-deftest org-grimoire-test-init-copies-default-theme ()
  "`org-grimoire-init' leaves an editable copy of the default theme."
  (let ((base (make-temp-file "org-grimoire-init-" t)))
    (org-grimoire-init "demo" base "https://example.com")
    (should (file-directory-p (expand-file-name "themes/default" base)))
    (should (file-exists-p (expand-file-name "themes/default/base.html" base)))
    (should (file-exists-p
             (expand-file-name "themes/default/static/css/style.css" base)))))

;;; Open Graph metadata

(defun org-grimoire-test--make-meta-site (body)
  "Create a fixture site whose single post has BODY after its keywords.
Return the base directory."
  (let* ((base (make-temp-file "org-grimoire-test-" t))
         (post (expand-file-name "content/post" base)))
    (make-directory post t)
    (write-region (concat "#+TITLE: Solo\n#+DATE: 2026-01-01\n#+TAGS: emacs\n\n"
                          body)
                  nil (expand-file-name "solo.org" post))
    base))

(defun org-grimoire-test--meta (html property)
  "Return the content of the meta tag named PROPERTY in HTML."
  (when (string-match
         (format "<meta[^>]*\\(?:property\\|name\\)=\"%s\"[^>]*content=\"\\([^\"]*\\)\""
                 (regexp-quote property))
         (or html ""))
    (match-string 1 html)))

(defun org-grimoire-test--solo (base)
  "Return the rendered HTML of the solo post built under BASE."
  (let ((out (org-grimoire-test--build base)))
    (org-grimoire-test--slurp
     (expand-file-name "post/solo.html" out))))

(ert-deftest org-grimoire-test-description-keyword-wins ()
  "#+DESCRIPTION: is preferred over the first paragraph."
  (let* ((base (org-grimoire-test--make-meta-site
                "#+DESCRIPTION: Chosen by hand.\n\nThe opening paragraph.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "Chosen by hand."
                   (org-grimoire-test--meta html "og:description")))))

(ert-deftest org-grimoire-test-description-falls-back-to-first-paragraph ()
  "Without the keyword the first paragraph becomes the description."
  (let* ((base (org-grimoire-test--make-meta-site "The opening paragraph.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "The opening paragraph."
                   (org-grimoire-test--meta html "og:description")))))

(ert-deftest org-grimoire-test-description-strips-org-links ()
  "Link syntax in the first paragraph is reduced to its visible text."
  (let* ((base (org-grimoire-test--make-meta-site
                "Text with [[https://example.com][a link]] inside.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "Text with a link inside."
                   (org-grimoire-test--meta html "og:description")))))

(ert-deftest org-grimoire-test-description-strips-footnotes ()
  "A footnote reference is removed from the description."
  (let* ((base (org-grimoire-test--make-meta-site
                "I got mine on AliExpress[fn:1].\n\n[fn:1] A shop.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "I got mine on AliExpress."
                   (org-grimoire-test--meta html "og:description")))))

(ert-deftest org-grimoire-test-description-is-escaped ()
  "A quote in the description cannot break out of the attribute."
  (let* ((base (org-grimoire-test--make-meta-site
                "#+DESCRIPTION: He said \"hello\" & left.\n\nBody.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "He said &quot;hello&quot; &amp; left."
                   (org-grimoire-test--meta html "og:description")))))

(ert-deftest org-grimoire-test-description-is-truncated ()
  "A long first paragraph is cut on a word boundary."
  (let* ((base (org-grimoire-test--make-meta-site
                (concat (mapconcat #'identity
                                   (make-list 60 "word") " ")
                        ".\n")))
         (html (org-grimoire-test--solo base))
         (text (org-grimoire-test--meta html "og:description")))
    (should (<= (length text) 163))
    (should (string-suffix-p "..." text))
    (should-not (string-match-p "wor\\.\\.\\." text))))

(ert-deftest org-grimoire-test-image-keyword-wins ()
  "#+IMAGE: is preferred over the first image in the body."
  (let* ((base (org-grimoire-test--make-meta-site
                "#+IMAGE: ./images/chosen.png\n\n[[./images/first.png]]\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "https://example.com/post/images/chosen.png"
                   (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-image-falls-back-to-first-in-post ()
  "Without the keyword the first image in the post is used."
  (let* ((base (org-grimoire-test--make-meta-site
                "Intro.\n\n[[./images/first.png]]\n\n[[./images/second.png]]\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "https://example.com/post/images/first.png"
                   (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-image-ignores-non-image-links ()
  "A link to a non-image file is not chosen as the preview image."
  (let* ((base (org-grimoire-test--make-meta-site
                "[[./notes.org]]\n\n[[./images/real.jpg]]\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "https://example.com/post/images/real.jpg"
                   (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-image-accepts-absolute-forms ()
  "Root-relative and full URLs are passed through correctly."
  (let* ((base (org-grimoire-test--make-meta-site
                "#+IMAGE: /static/card.png\n\nBody.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "https://example.com/static/card.png"
                   (org-grimoire-test--meta html "og:image"))))
  (let* ((base (org-grimoire-test--make-meta-site
                "#+IMAGE: https://cdn.example.org/card.png\n\nBody.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "https://cdn.example.org/card.png"
                   (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-post-without-image-has-empty-og-image ()
  "With no site image configured, a post linking none leaves og:image empty."
  (let* ((base (org-grimoire-test--make-meta-site "Just words.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "" (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-site-image-is-the-fallback ()
  "Pages supplying no image of their own fall back to :og-image."
  (let* ((base  (org-grimoire-test--make-meta-site "Just words.\n"))
         (out   (org-grimoire-test--build base :og-image "/static/avatar.webp"))
         (post  (org-grimoire-test--slurp
                 (expand-file-name "post/solo.html" out)))
         (index (org-grimoire-test--slurp
                 (expand-file-name "index.html" out))))
    (should (equal "https://example.com/static/avatar.webp"
                   (org-grimoire-test--meta post "og:image")))
    (should (equal "https://example.com/static/avatar.webp"
                   (org-grimoire-test--meta index "og:image")))))

(ert-deftest org-grimoire-test-post-image-beats-site-image ()
  "A post with its own image is not overridden by :og-image."
  (let* ((base (org-grimoire-test--make-meta-site "[[./images/own.png]]\n"))
         (out  (org-grimoire-test--build base :og-image "/static/avatar.webp"))
         (html (org-grimoire-test--slurp
                (expand-file-name "post/solo.html" out))))
    (should (equal "https://example.com/post/images/own.png"
                   (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-site-image-accepts-full-url ()
  "A full URL in :og-image is used as it is."
  (let* ((base (org-grimoire-test--make-meta-site "Just words.\n"))
         (out  (org-grimoire-test--build
                base :og-image "https://cdn.example.org/a.png"))
         (html (org-grimoire-test--slurp
                (expand-file-name "post/solo.html" out))))
    (should (equal "https://cdn.example.org/a.png"
                   (org-grimoire-test--meta html "og:image")))))

(ert-deftest org-grimoire-test-og-type-differs-by-page ()
  "Posts are articles; the index is a website."
  (let* ((base  (org-grimoire-test--make-meta-site "Body.\n"))
         (out   (org-grimoire-test--build base))
         (post  (org-grimoire-test--slurp
                 (expand-file-name "post/solo.html" out)))
         (index (org-grimoire-test--slurp
                 (expand-file-name "index.html" out))))
    (should (equal "article" (org-grimoire-test--meta post "og:type")))
    (should (equal "website" (org-grimoire-test--meta index "og:type")))))

(ert-deftest org-grimoire-test-og-url-is-per-page ()
  "Each post carries its own canonical URL."
  (let* ((base (org-grimoire-test--make-meta-site "Body.\n"))
         (html (org-grimoire-test--solo base)))
    (should (equal "https://example.com/post/solo.html"
                   (org-grimoire-test--meta html "og:url")))))

(ert-deftest org-grimoire-test-wrap-base-without-vars-still-renders ()
  "Callers passing three arguments keep working unchanged."
  (let* ((base (org-grimoire-test--make-meta-site "Body.\n"))
         (out  (org-grimoire-test--build base))
         (tags (org-grimoire-test--slurp
                (expand-file-name "tags/emacs.html" out))))
    (should tags)
    (should (equal "website" (org-grimoire-test--meta tags "og:type")))))

(ert-deftest org-grimoire-test-plist-merge-does-not-mutate ()
  "Merging leaves both inputs untouched and applies the overrides."
  (let* ((defaults (list :a 1 :b 2))
         (override (list :b 3 :c 4))
         (merged   (org-grimoire--plist-merge defaults override)))
    (should (equal 1 (plist-get merged :a)))
    (should (equal 3 (plist-get merged :b)))
    (should (equal 4 (plist-get merged :c)))
    (should (equal 2 (plist-get defaults :b)))
    (should (equal 3 (plist-get override :b)))))

(provide 'org-grimoire-test)
;;; org-grimoire-test.el ends here
