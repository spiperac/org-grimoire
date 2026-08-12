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

(provide 'org-grimoire-test)
;;; org-grimoire-test.el ends here
