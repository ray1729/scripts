#!/usr/bin/env -S guile -e main -s
!#

;; Script for updating current account and credit card statements
;; downloaded from Nationwide, who use a date and currency format
;; that Gnucash does not support.

(use-modules (ice-9 getopt-long)
             ((srfi srfi-1) #:select (drop))
             (dsv))

;; Date appears in Nationwide statements in the format
;; "10 Jan 2024", but this is not understood by Gnucash
;; so we convert it to YYYY-MM-DD format.
(define date-input-format "%d %b %Y")
(define date-output-format "%Y-%m-%d")

(define (format-date d)
  (strftime date-output-format
            (car (strptime date-input-format d))))

;; Characters we expect to see in a numeric amount field. The
;; Nationwide statements contain a non-ASCII currency character
;; that we want to delete.
(define currency-charset (string->char-set "0123456789.-"))

(define (format-amount s)
  (string-filter currency-charset s))

;; Profiles for the different statement formats.
;; skip: the number of leading rows to skip
;; header: boolean indicating whether or not the first unskipped
;;         row is a header
;; date-cols: list of columns containing dates
;; amount-cols: list columns containing amounts
(define profiles
  '(("credit-card" . ((skip . 4)
                      (header . #t)
                      (date-cols . (0))
                      (amount-cols . (3 4))))
    ("current-account" . ((skip . 4)
                          (header . #t)
                          (date-cols . (0))
                          (amount-cols . (3 4 5))))))

;; Predicate for validating the profile option.
(define (valid-profile? p)
  (if (assoc p profiles) #t #f))

;; Update a list by applying the given function to each of the
;; listed columns.
(define (update-list lst cols f)
  (for-each (lambda (k)
              (let ((v (list-ref lst k)))
                (list-set! lst k (f v))))
            cols))

;; Given a spec listing the date and amount columns, return a
;; function that will apply the corresponding formats to a row.
(define (process-row spec)
  (let ((date-cols (assq-ref spec 'date-cols))
        (amount-cols (assq-ref spec 'amount-cols)))
    (lambda (row)
      (when date-cols
        (update-list row date-cols format-date))
      (when amount-cols
        (update-list row amount-cols format-amount)))))

;; Read a CSV from the given path.
(define (read-statement path)
  (call-with-input-file path
    (lambda (port)
      (dsv->scm port #:format 'rfc4180))))

;; Write data to the given path in CSV format.
(define (write-statement data path)
  (call-with-output-file path
    (lambda (port)
      (scm->dsv data port #:format 'rfc4180))))

;; Apply the specified updates to data (a list of rows
;; read from the CSV). If a 'skip value is specified, drop
;; this many leading rows. If a 'header is present, only
;; apply the updates to the succeeding rows, preserving
;; the header as-is.
(define (update-data spec data)
  (let* ((skip (assq-ref spec 'skip))
         (data (if skip (drop data skip) data)))
    (for-each (process-row spec) (if (assq-ref spec 'header) (cdr data) data))
    data))

;; Apply the updates defined in `spec` to the statement read
;; from input-path and write the updated data to output-path.
(define (process-statement spec input-path output-path)
  (let ((data (read-statement input-path)))
    (write-statement (update-data spec data) output-path)))

;; Display a usage message and (optional) error message to STDERR
;; and exit. If an error message is given the exit code will be
;; non-zero.
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
    -i, --input=FILENAME   Input file path. Required.
    -o, --output=FILENAME  Output file path. Required unless --overwrite is given.
    -w, --overwrite        Overwrite the input file with the updated data.
    -p, --profile=PROFILE  Profile name [credit-card|current-account].
")
      (exit (if errmsg EXIT_FAILURE EXIT_SUCCESS)))))

;; Process command-line arguments and validate options.
;; If valid, run process-statement with the given options.
(define (main args)
  (let* ((option-spec `((help      (single-char #\h) (value #f))
                        (input     (single-char #\i) (value #t))
                        (output    (single-char #\o) (value #t))
                        (overwrite (single-char #\w) (value #f))
                        (profile   (single-char #\p) (value #t) (predicate ,valid-profile?))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (profile (option-ref options 'profile #f))
         (input (option-ref options 'input #f))
         (output (option-ref options 'output #f))
         (overwrite (option-ref options 'overwrite #f)))
    (cond
     (help-wanted (usage))
     ((not profile) (usage "profile is required"))
     ((not input) (usage "input filename is required"))
     ((and overwrite output) (usage "output filename cannot be given with --overwrite"))
     ((not (or overwrite output)) (usage "output filename is required without --overwrite")))
    (process-statement (assoc-ref profiles profile) input (or output input))))
