;; SPDX-FileCopyrightText: 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(defpackage #:core.text.read
  (:use #:cl)
  (:export #:core-text-read))
(in-package #:core.text.read)

(defvar eof-object (gensym "EOF"))

(defun read-char? (char)
  (and (eql char (peek-char nil nil nil eof-object))
       (read-char nil nil eof-object)))

(defun read-char-satisyfing (predicate)
  (and (funcall predicate (peek-char nil nil nil eof-object))
       (read-char nil nil eof-object)))

(defun read-char* (first-char? subsequent-char?)
  (let ((first-char (read-char-satisyfing first-char?)))
    (and first-char
         (with-output-to-string (out)
           (write-char first-char out)
           (loop (let ((char (read-char-satisyfing subsequent-char?)))
                   (if (not char) (return) (write-char char out))))))))

(defun skip-rest-of-line ()
  (unless (or (read-char? eof-object) (read-char? #\newline))
    (read-char)
    (skip-rest-of-line)))

(defun core-whitespace-char? (obj)
  (case obj ((#\space #\tab #\newline #\return #\,) t)))

(defun skip-whitespace-and-comments ()
  (loop (cond ((read-char? #\;)
               (skip-rest-of-line))
              ((not (read-char-satisyfing #'core-whitespace-char?))
               (return)))))

(defun terminate? (what sentinel)
  (skip-whitespace-and-comments)
  (if (read-char? eof-object)
      (error "Unterminated ~A" what)
      (not (not (read-char? sentinel)))))

(defun read-list ()
  (let ((elts '()))
    (loop (if (terminate? "list" #\))
              (return (reverse elts))
              (push (must-read-one "list element") elts)))))

(defun read-mapping ()
  (let ((table (make-hash-table :test #'equal)))
    (loop (if (terminate? "mapping" #\})
              (return table)
              (let ((key (must-read-one "mapping key")))
                (skip-whitespace-and-comments)
                (let ((val (must-read-one "mapping value")))
                  (setf (gethash key table) val)))))))

(defun symbol-first? (char)
  (and (characterp char) (alpha-char-p char)))

(defun symbol-subsequent? (char)
  (and (characterp char) (or (alpha-char-p char)
                             (digit-char-p char)
                             (case char ((#\- #\_) t)))))

(defun read-bare-symbol? ()
  (let ((name (read-char* #'symbol-first? #'symbol-subsequent?)))
    (and name (intern name :keyword))))

(defun read-sharpsign ()
  (if (read-char? #\{)
      (read-mapping)
      (let ((sym (read-bare-symbol?)))
        (case sym
          ((:|t|) :true)
          ((:|f|) :false)
          ((nil)  (error "Unknown character after sharpsign"))
          (t      (error "Unknown symbol after sharpsign: ~S" sym))))))

(defun read-char-escape ()
  (or (read-char? #\\)
      (read-char? #\")
      (read-char? #\|)
      (error "Unknown escape character")))

(defun read-quoted-stringlike (what sentinel)
  (with-output-to-string (out)
    (loop (cond ((read-char? eof-object)
                 (error "Unterminated ~A" what))
                ((read-char? sentinel)
                 (return))
                ((read-char? #\\)
                 (write-char (read-char-escape) out))
                (t
                 (write-char (read-char) out))))))

(defun read-quoted-string ()
  (read-quoted-stringlike "double-quoted string" #\"))

(defun read-quoted-symbol ()
  (intern (read-quoted-stringlike "vertical-bar symbol" #\|) :keyword))

(defun read-number? ()
  (let* ((sign (read-char? #\-))
         (magnitude (read-char* #'digit-char-p #'digit-char-p)))
    (if (not magnitude)
        (and sign (error "Sign without magnitude"))
        (let ((magnitude (read-from-string magnitude)))
          (if sign (- magnitude) magnitude)))))

(defun read-bare-symbol-or-number ()
  (or (read-bare-symbol?)
      (read-number?)))

(defun may-read-one ()
  (cond ((read-char? #\() (read-list))
        ((read-char? #\#) (read-sharpsign))
        ((read-char? #\") (read-quoted-string))
        ((read-char? #\|) (read-quoted-symbol))
        (t (or (read-bare-symbol-or-number)
               (or (read-char? eof-object)
                   (error "Unknown syntax character in input: ~S"
                          (read-char)))))))

(defun must-read-one (what)
  (let ((one (may-read-one)))
    (if (eql eof-object one) (error "Expected ~A" what) one)))

(defun core-text-read ()
  (skip-whitespace-and-comments)
  (may-read-one))
