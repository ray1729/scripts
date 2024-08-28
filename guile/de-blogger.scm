#!/usr/bin/env -S guile -e main -s
!#

(use-modules (srfi srfi-26)
             (srfi srfi-71)
             (srfi srfi-197)
             (ice-9 regex)
             (ice-9 textual-ports)
             (ice-9 binary-ports)
             (ice-9 ftw)
             (ice-9 format)
             (ice-9 string-fun)
             (web client)
             (web response))

(define base-dir "/home/ray/Workspace/personal/start-again-at-zero/")
(define posts-dir (string-append base-dir "content/posts/"))
(define image-dir (string-append base-dir "static/img/"))

(define md-img-rx (make-regexp "!\\[[^]]*\\]\\((https?[^)]+)\\)"))

(define md-img-link-rx (make-regexp "\\((https?[^)]+\\.(png|jpg))\\)" regexp/icase))

(define img-src-rx (make-regexp "<img src=\"([^\"]+)\""))

(define amazon-assoc-img-rx (make-regexp "!\\[\\]\\(http://www.assoc-amazon[^)]+\\)"))

(define (http-get-follow-redirects url)
  (let ((res body (http-get url #:streaming? #t)))
    (if (<= 300 (response-code res) 399)
        (http-get-follow-redirects (response-location res))
        (if (eq? (car (response-content-type res)) 'text/html)
            ;; Work-around for Google wrapping the image in HTML. Gah!
            (let* ((content (get-string-all body))
                   (new-url (match:substring (regexp-exec img-src-rx content) 1)))
              (close-port body)
              (http-get-follow-redirects new-url))
            (values res body)))))

(define download-image
  (let ((image-num 1)
        (images (make-hash-table)))
    (lambda (url)
      (format #t "download-image ~a~%" url)
      (if (hash-ref images url)
          (hash-ref images url)
          (let* ((res body (http-get-follow-redirects url))
                 (suffix (case (car (response-content-type res))
                           ((image/jpeg) ".jpg")
                           ((image/png) ".png")
                           (else (error (format #f "download error: unexpected content-type for ~s" url)))))
                 (filename (format #f "IMG_~4,'0d~a" image-num suffix)))
            (hash-set! images url filename)
            (set! image-num (1+ image-num))
            (call-with-output-file (string-append image-dir filename)
              (lambda (port)
                (let loop ((data (get-bytevector-some body)))
                  (unless (eof-object? data)
                    (put-bytevector port data)
                    (loop (get-bytevector-some body))))))
            (close-port body)
            filename)))))

(define (replace-images doc)
    (for-each (lambda (m)
                (let* ((url (match:substring m 1))
                       (filename (download-image url))
                       (new-url (string-append "/img/" filename)))
                  (set! doc (string-replace-substring doc url new-url))))
              (list-matches md-img-rx doc))
    doc)

(define (replace-links doc)
  (for-each (lambda (m)
              (let* ((url (match:substring m 1))
                     (filename (download-image url))
                     (new-url (string-append "/img/" filename)))
                (set! doc (string-replace-substring doc url new-url))))
            (list-matches md-img-link-rx doc))
  doc)

(define (replace-amazon-tracking-images doc)
  (for-each (lambda (m)
              (set! doc (string-replace-substring doc (match:substring m) "")))
            (list-matches amazon-assoc-img-rx doc))
  doc)

(define link-rx (make-regexp "\\[([^]]+)\\]\\(([^)]+)\\)"))

(define (remove-amazon-links doc)
  (for-each (lambda (m)
              (when (string-contains (match:substring m 2) "staagaatzer-21")
                (set! doc (string-replace-substring doc (match:substring m 0) (match:substring m 1)))))
            (list-matches link-rx doc))
  doc)

(define (replace-self-links doc)
  (string-replace-substring doc
                            "http://startagainatzero.blogspot.com/"
                            "/"))

(define (process-file path)
  (format #t "process-file ~a~%" path)
  (let ((doc (chain (call-with-input-file path get-string-all)
                    (replace-amazon-tracking-images _)
                    (remove-amazon-links _)
                    (replace-images _)
                    (replace-links _)
                    (replace-self-links _))))
    (call-with-output-file path (cut put-string <> doc))))

(define (main args)
  (for-each process-file
            (map (cute string-append posts-dir <>)
                 (scandir posts-dir (cute string-suffix? ".md" <>)))))
