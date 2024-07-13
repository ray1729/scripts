#!/usr/bin/env -S guile -e main -s
!#

(use-modules (ice-9 match)
             (ice-9 getopt-long)
             (dsv))


(define date-input-format "%d %b %Y")
(define date-output-format "%Y-%m-%d")

(define (format-date d)
  (strftime date-output-format
            (car (strptime date-input-format d))))

(define (read-statement path)
  (call-with-input-file path
    (lambda (port)
      (dsv->scm port #:format 'rfc4180))))

(define currency-charset (string->char-set "0123456789.-"))

(define (format-amount s)
  (string-filter currency-charset s))

(define (process-row row)
  (match-let (((date description location paid-out paid-in) row))
    (list (format-date date)
          description
          location
          (format-amount paid-out)
          (format-amount paid-in))))

(define (process-statement input-path output-path)
  (match-let (((_ _ _ _ header . data) (read-statement input-path)))
    (let ((updated (cons header (map process-row data))))
      (call-with-output-file output-path
        (lambda (port)
          (scm->dsv updated port #:format 'rfc4180))))))

(define* (usage #:optional errmsg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (when errmsg
        (display "Error: ")
        (display errmsg)
        (newline))
      (display "\
Usage: fix-credit-card-statement [options]
    -h, --help             Display this help.
    -i, --input=FILENAME   Input file path.
    -o, --output=FILENAME  Output file path. Required unless --overwrite is given.
    -w, --overwrite        Overwrite the input file with the updated data.
")
      (exit (if errmsg EXIT_FAILURE EXIT_SUCCESS)))))

(define (main args)
  (let* ((option-spec '((help      (single-char #\h) (value #f))
                        (input     (single-char #\i) (value #t))
                        (output    (single-char #\o) (value #t))
                        (overwrite (single-char #\w) (value #f))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (input (option-ref options 'input #f))
         (output (option-ref options 'output #f))
         (overwrite (option-ref options 'overwrite #f)))
    (cond
     (help-wanted (usage))
     ((not input) (usage "input filename is required"))
     ((and overwrite output) (usage "output filename cannot be given with --overwrite"))
     ((not (or overwrite output)) (usage "output filename is required without --overwrite")))
    (process-statement input (or output input))))
