;;
;; Whisper Engine - The Ghost Operator Site Generator
;; v0.0.9
;;

;; Configuration

(defvar ds/public-article-dir "/path/to/public-html-dir"
  "Public directory where the HTML files will be generated.")

(defvar ds/org-article-dir "/path/to/private-org-file-dir"
  "The org file dir. Article sources are here.")

(defvar ds/index-title "Ghost Operator Whispers"
  "Title of the index page.")

(defvar ds/posts-per-page 6
  "This option sets how many articles are presented on the index page.")

(defvar ds/site-url "https://site-url.tld"
  "The URL of the site.")

(defvar ds/html-nav
  "<nav>
     <a href=\"/index.html\">Home</a> |
     <a href=\"/static/services.html\">Services</a> |
     <a href=\"/static/about.html\">About</a>
   </nav>"
  "HTML snippet for navigation bar.")

(defvar ds/html-footer
  "<footer><p>© 2025 DeadSwitch | The Ghost Operator | All rights reserved.</p></footer>"
  "HTML snippet for footer.")

(defvar ds/base-html-template
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>%s</title>
  <link rel=\"stylesheet\" href=\"/static/style.css\">
  <link rel=\"icon\" href=\"/static/favicon.ico\" type=\"image/x-icon\">
  %s
</head>
<body>
  %s
  <main><div class=\"article-content\">%s</div></main>
  %s
</body>
</html>"
  "Base HTML template for Whisper Engine static pages.
Placeholders: title, og-tags, navigation, body-html, footer.")

;; Extract the keywords and metadata

(defun ds/extract-keywords (file keywords)
  "Extract the keywords from an org article."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (or (org-collect-keywords keywords) '()))))

(defun ds/get-keyword (keyword keywords)
  "Get keyword as string."
  (cdr (assoc keyword keywords)))

(defun ds/extract-properties (file)
  "Extract the properties from the first headline of an org article."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (when (re-search-forward org-heading-regexp nil t)
          (or (org-entry-properties nil 'standard) '())))))

(defun ds/get-property (property properties)
  "Get property as string."
  (cdr (assoc property properties)))

;; Find and filter the posts

(defun ds/find-posts (dir)
  "Return the HTML files in a dir structure."
  (directory-files-recursively dir "\\.html$"))

(defun ds/find-org-posts (dir)
  "Find all .org articles in a dir structure.."
  (directory-files-recursively dir "\\.org$"))

(defun ds/count-files (files)
  "Count the HTML files."
  (let ((count 0))
    (dolist (file files count)
      (when (file-exists-p file)
        (setq count (1+ count))))))

(defun ds/page-count (file-count)
  "Calculate how many pages are needed for FILE-COUNT."
  (ceiling (/ (float file-count) ds/posts-per-page)))

(defun ds/build-post-metadata (file)
  "Extract metadata and compute EXPORT_URL from the .org file."
  (let* ((raw-keywords (or (ds/extract-keywords file '("TITLE" "DATE")) '()))
         (keywords (mapcar (lambda (pair) (cons (car pair) (cadr pair))) raw-keywords)) ; fix!
         (properties (or (ds/extract-properties file) '()))
         (date-str (cdr (assoc "DATE" keywords)))
         (year     (format-time-string "%Y" (org-time-string-to-time date-str)))
         (month    (format-time-string "%m" (org-time-string-to-time date-str)))
         (basename (file-name-base file))
         (export-url (format "%s/%s/%s.html" year month basename)))
    (append keywords
            properties
            `(("EXPORT_URL" . ,export-url)))))

;; Exporting

(defun ds/ready-for-deploy-p (file)
  "Return t if FILE has #+READY_FOR_DEPLOY: t."
  (let ((keywords (ds/extract-keywords file '("READY_FOR_DEPLOY"))))
    (string= (car (cdr (assoc "READY_FOR_DEPLOY" keywords))) "t")))

(defun ds/collect-deployable-posts (dir)
  "Return a list of metadata alists for deploy-ready posts."
  (let ((posts '()))
    (dolist (file (ds/find-org-posts dir))
      (when (ds/ready-for-deploy-p file)
        (push (ds/build-post-metadata file) posts)))
    (nreverse posts)))

(defun ds/patch-src-containers-with-lang (html)
  "Patch <div class=\"org-src-container\"> to add data-lang attribute."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward
            "<div class=\"org-src-container\">\\s-*<pre class=\"src src-\\([^\"]+\\)\"" nil t)
      (let ((lang (match-string 1)))
        (replace-match
         (format "<div class=\"org-src-container\" data-lang=\"%s\">\n<pre class=\"src src-%s\"" lang lang)
         t)))
    (buffer-string)))

(defun ds/fix-broken-brs (html)
  "Fix the shr-dom-to-xml double <br> tags."
  (replace-regexp-in-string "<br></br>" "<br />" html))

(defun ds/fix-image-paths (dom)
  "Fix absolute file:// paths in <img> tags to site-relative /static paths."
  (dolist (img (dom-by-tag dom 'img))
    (let ((src (dom-attr img 'src)))
      (when (and src (string-prefix-p "file:///" src))
        ;; Replace "file:///" with "/" to get relative path
        (setf (dom-attr img 'src)
              (replace-regexp-in-string "^file://+" "/" src)))))
  dom)

(defun ds/fix-self-closing-tags (html)
  "Fix self-closing tag syntax for HTML5 compatibility."
  (replace-regexp-in-string "<\\(img\\|hr\\|input\\)\\([^>]*\\)></\\1>" "<\\1\\2 />" html))

(defun ds/remove-id-attributes (dom)
  "Recursively remove all 'id' attributes from the DOM tree."
  (cond
   ;; If it's a tag element
   ((and (listp dom) (symbolp (car dom)))
    (let* ((tag (car dom))
           (attrs (cadr dom))
           (children (cddr dom))
           ;; Remove `id` from attributes
           (clean-attrs (assq-delete-all 'id attrs))
           ;; Recurse into children
           (clean-children (mapcar #'ds/remove-id-attributes children)))
      (cons tag (cons clean-attrs clean-children))))
   ;; If it's not a tag (e.g., string, nil), return as-is
   (t dom)))

(defun ds/export-article (file)
  "Export .org article to wrapped HTML."
  (unless (libxml-available-p)
    (error "libxml support is required but not available in this Emacs"))

  (let* ((metadata (ds/build-post-metadata file))
         (date-str (ds/get-keyword "DATE" metadata))
         (title    (ds/get-keyword "TITLE" metadata))
         (teaser   (ds/get-property "TEASER" metadata))
         (image    (ds/get-property "IMAGE" metadata))
         (export-url (ds/get-keyword "EXPORT_URL" metadata))
         (year     (format-time-string "%Y" (org-time-string-to-time date-str)))
         (month    (format-time-string "%m" (org-time-string-to-time date-str)))
         (basename (file-name-base file))
         (target-dir (expand-file-name (format "%s/%s" year month) ds/public-article-dir))
         (target-file (expand-file-name (format "%s.html" basename) target-dir)))
    
    (make-directory target-dir t)

    (let ((html-body
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (let* ((raw-html (org-export-as 'html nil nil t nil)))
               (with-temp-buffer
                 (insert raw-html)
                 (goto-char (point-min))
                 (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                        (patched-dom (ds/remove-id-attributes dom))
                        (body (car (dom-by-tag patched-dom 'body)))
                        (body-children (cddr body))
                        (patched-body (ds/fix-image-paths body-children))
                        (inner-html (ds/fix-self-closing-tags
                                     (ds/fix-broken-brs (shr-dom-to-xml patched-body)))))
                   (ds/patch-src-containers-with-lang inner-html)))))))

      ;; Build OG meta tags
      (let ((meta `(("og:title" . ,title)
                    ("og:description" . ,(or teaser "A DeadSwitch Whisper"))
                    ("og:image" . ,(or image "/static/img/default.jpg"))
                    ("og:url" . ,(format "%s/%s" ds/site-url export-url)))))
        (ds/write-static-page target-file title html-body meta)))))

(defun ds/export-all-ready-articles ()
  "Export all deploy-ready articles to HTML."
  (dolist (file (ds/find-org-posts ds/org-article-dir))
    (when (ds/ready-for-deploy-p file)
      (ds/export-article file))))

(defun ds/parse-date (date-str)
  (let ((time (org-time-string-to-time date-str)))
    (float-time time)))

(defun ds/collect-sorted-posts ()
  "Collect deploy-ready posts and sort them by date (newest first)."
  (sort (ds/collect-deployable-posts ds/org-article-dir)
        (lambda (a b)
          (> (ds/parse-date (ds/get-keyword "DATE" a))
             (ds/parse-date (ds/get-keyword "DATE" b))))))

;; Page building

(defun ds/write-static-page (filepath title body-html &optional meta)
  "Wrap BODY-HTML with navigation and footer, then write to FILEPATH."
  (let* ((og-tags (when meta
                    (mapconcat
                     (lambda (pair)
                       (format "<meta property=\"%s\" content=\"%s\" />"
                               (car pair) (cdr pair)))
                     meta
                     "\n ")))
         (html   (format ds/base-html-template
                         title (or og-tags "") ds/html-nav body-html ds/html-footer)))
    (with-temp-file filepath
      (insert html))))

;; Pagination

(defun ds/generate-pagination-links (current total)
  "Generate clean, scalable pagination links for blog index."
  (let ((range-size 5)
        (links '()))
    (push (if (> current 1)
              (format "<a href=\"/%s\">Prev</a>" 
                      (if (= (1- current) 1) "index.html" (format "page%d.html" (1- current))))
            "<span class=\"disabled\">Prev</span>")
          links)

    ;; First page and leading ellipsis
    (when (> current (- range-size 1))
      (push "<a href=\"/index.html\">1</a>" links)
      (when (> current range-size)
        (push "<span class=\"ellipsis\">…</span>" links)))

    ;; Page number window
    (let* ((start (max 1 (- current 2)))
           (end   (min total (+ current 2))))
      (dotimes (i (- (1+ end) start))
        (let ((page (+ start i)))
          (push (if (= page current)
                    (format "<span class=\"current\">%d</span>" page)
                  (format "<a href=\"/%s\">%d</a>"
                          (if (= page 1) "index.html" (format "page%d.html" page)) page))
                links))))

    ;; Trailing ellipsis and last page
    (when (< current (- total (- range-size 2)))
      (when (< current (- total 3))
        (push "<span class=\"ellipsis\">…</span>" links))
      (push (format "<a href=\"/page%d.html\">%d</a>" total total) links))

    ;; Next link
    (push (if (< current total)
              (format "<a href=\"/%s\">Next</a>"
                      (format (if (= (1+ current) 1) "index.html" "page%d.html") (1+ current)))
            "<span class=\"disabled\">Next</span>")
          links)

    ;; Final HTML
    (format "<nav class=\"pagination\">%s</nav>"
            (string-join (reverse links) " "))))

(defun ds/generate-paginated-index ()
  "Generate paginated blog index pages."
  (let* ((sorted-posts (ds/collect-sorted-posts))
         (posts-per-page ds/posts-per-page)
         (total-posts (length sorted-posts))
         (total-pages (ds/page-count total-posts))
         (chunks (cl-loop for i from 0 below total-posts by posts-per-page
                          collect (cl-subseq sorted-posts i (min total-posts (+ i posts-per-page))))))
    (cl-loop for posts in chunks
             for page from 1
             for filename = (if (= page 1) "index.html" (format "page%d.html" page))
             for filepath = (expand-file-name filename ds/public-article-dir)
             do
             (let* ((items
                     (mapconcat
                      (lambda (meta)
                        (let ((title  (ds/get-keyword "TITLE" meta))
                              (date   (ds/get-keyword "DATE" meta))
                              (url    (ds/get-keyword "EXPORT_URL" meta))
                              (teaser (ds/get-property "TEASER" meta))
                              (image  (ds/get-property "IMAGE" meta)))
                          (format "<li class=\"article-list\"><a href=\"/%s\">%s</a><span class=\"date\">%s</span>%s%s</li>"
                                  url
                                  title
                                  date
                                  (if image
                                      (format "<img src=\"%s\" alt=\"%s thumbnail\" class=\"thumb\">" image title)
                                    "")
                                  (if teaser
                                      (format "<div class=\"teaser\">%s</div>" teaser)
                                    ""))))
                      posts
                      "\n"))
                    (body (format "<h1>%s</h1><ul>%s</ul>%s"
                                  ds/index-title
                                  items
                                  (ds/generate-pagination-links page total-pages))))
               (ds/write-static-page filepath ds/index-title body)))))

;; Clean up the public dir

(defun ds/clean-public-html ()
  "Delete all generated HTML files in ds/public-article-dir except static/ directory."
  (interactive)
  (let* ((dir ds/public-article-dir)
         (protected '("static")))
    (when (and dir (file-directory-p dir)
               (yes-or-no-p (format "Clean public dir (except %s)?" protected)))
      (dolist (file (directory-files dir t "^[^.].*"))
        (let ((name (file-name-nondirectory file)))
          (unless (member name protected)
            (message "Deleting: %s" name)
            (if (file-directory-p file)
                (delete-directory file t)
              (delete-file file))))))))

;; User facing functions

(defun whisper/add-site-element ()
  "Insert Whisper Engine site element interactively, with value prompts."
  (interactive)
  (let* ((choices '(("HTML Class" . "#+ATTR_HTML: :class %s")
                    ("Image Property" . ":IMAGE: %s")
                    ("Teaser Property" . ":TEASER: %s")))
         (choice (completing-read "Choose element to insert: " (mapcar #'car choices)))
         (template (cdr (assoc choice choices)))
         (value (read-string (format "Enter value for %s: " choice))))
    (insert (format template value))))

;; Generate the site

(defun ds/generate-site ()
  (interactive)
  (ds/clean-public-html)
  (ds/export-all-ready-articles)
  (ds/generate-paginated-index))

;; (ds/generate-site)
