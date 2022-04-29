;;; tomelr.el --- Convert Emacs s-expressions to TOML           -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.modi@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; tomelr.el is a library that provides functions to convert Emacs
;; symbolic expressions to TOML.

;;; Code:

(require 'map)
(require 'subr-x)  ;For `string-trim' on Emacs versions 27.2 and older


;;; Variables

(defvar tomelr-false '(:false 'false "false")
  "S-exp values to be interpreted as TOML `false'.")

(defvar tomelr-encoding-default-indentation "  "
  "String used for a single indentation level during encoding.
This value is repeated for each further nested element.")

(defvar tomelr-encoding-object-sort-predicate nil
  "Sorting predicate for TOML object keys during encoding.
If nil, no sorting is performed.  Else, TOML object keys are
ordered by the specified sort predicate during encoding.  For
instance, setting this to `string<' will have TOML object keys
ordered alphabetically.")

;;;; Internal Variables
(defvar tomelr--print-indentation-prefix "\n"
  "String used to start indentation during encoding.")

(defvar tomelr--print-indentation-depth -1
  "Current indentation level during encoding.
Dictates repetitions of `tomelr-encoding-default-indentation'.")

(defvar tomelr--print-table-hierarchy ()
  "Internal variable used to save the TOML table hierarchy.")

(defvar tomelr--print-keyval-separator " = "
  "String used to separate key-value pairs during encoding.")

(defvar tomelr--date-time-regexp
  (concat "\\`[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}"
          "\\(?:[T ][[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\(?:\\.[[:digit:]]+\\)*"
          "\\(?:Z\\|[+-][[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\)*\\)*\\'")
  "Regexp to match RFC 3339 formatted date-time with offset.

- https://toml.io/en/v1.0.0#offset-date-time
- https://tools.ietf.org/html/rfc3339#section-5.8

Examples:
  1979-05-27
  1979-05-27T07:32:00Z
  1979-05-27 07:32:00Z
  1979-05-27T00:32:00-07:00
  1979-05-27T00:32:00.999999+04:00.")



;;; Error conditions

(define-error 'tomelr-error "Unknown TOML error")
(define-error 'tomelr-key-format "Bad TOML object key" 'tomelr-error)



;;; Utilities

(defun tomelr-alist-p (list)
  "Non-nil if and only if LIST is an alist with simple keys."
  (declare (pure t) (side-effect-free error-free))
  (while (and (consp (car-safe list))
              (atom (caar list))
              (setq list (cdr list))))
  (null list))

(defun tomelr-plist-p (list)
  "Non-nil if and only if LIST is a plist with keyword keys."
  (declare (pure t) (side-effect-free error-free))
  (while (and (keywordp (car-safe list))
              (consp (cdr list))
              (setq list (cddr list))))
  (null list))

(defmacro tomelr--with-output-to-string (&rest body)
  "Eval BODY in a temporary buffer bound to `standard-output'.
Return the resulting buffer contents as a string."
  (declare (indent 0) (debug t))
  `(with-output-to-string
     (with-current-buffer standard-output
       ;; This affords decent performance gains.
       (setq-local inhibit-modification-hooks t)
       ,@body)))

(defmacro tomelr--with-indentation (&rest body)
  "Eval BODY with the TOML encoding nesting incremented by one step.
This macro sets up appropriate variable bindings for
`tomelr--print-indentation' to produce the correct indentation."
  (declare (debug t) (indent 0))
  `(let ((tomelr--print-indentation-depth (1+ tomelr--print-indentation-depth)))
     ,@body))

(defun tomelr--print-indentation ()
  "Insert the current indentation for TOML encoding at point."
  (insert tomelr--print-indentation-prefix)
  (dotimes (_ tomelr--print-indentation-depth)
    (insert tomelr-encoding-default-indentation)))



;;; Encoding

;;;; Keywords
(defun tomelr-encode-keyword (keyword)
  "Encode KEYWORD as a TOML value."
  (declare (side-effect-free t))
  ;; (message "[tomelr-encode-keyword DBG] keyword = %S" keyword)
  (cond ((eq keyword t)                "true")
        ((member keyword tomelr-false) "false")))

(defun tomelr--print-keyword (keyword)
  "Insert KEYWORD as a TOML value at point.
Return nil if KEYWORD is not recognized as a TOML keyword."
  (prog1 (setq keyword (tomelr-encode-keyword keyword))
    (and keyword (insert keyword))))

;;;; Strings
(defun tomelr--print-string (string &optional type)
  "Insert a TOML representation of STRING at point.

Optional TYPE arg gives more information about the input STRING.
For example, if the string is the name of a TOML key, it will be
set to `keyword'.

Return the same STRING passed as input.  See
`tomelr-encode-string' instead if you need a function that
returns the TOML representation as a string."
  ;; (message "[tomelr--print-string DBG] string = `%s'" string)
  (let ((special-chars '((?b . ?\b)     ;U+0008
                         (?f . ?\f)     ;U+000C
                         (?\\ . ?\\)))
        (special-chars-re (rx (in ?\" ?\\ cntrl ?\u007F))) ;cntrl is same as (?\u0000 . ?\u001F)
        begin-q end-q)
    (cond
     ((equal type 'keyword))
     ((string-match-p tomelr--date-time-regexp string)) ;RFC 3339 formatted date-time with offset
     ;; Use multi-line string quotation if the string contains a " char
     ;; or a newline - """STRING"""
     ((string-match-p "\n\\|\"" string)
      ;; From https://toml.io/en/v1.0.0#string, Any Unicode
      ;; character may be used except those that must be escaped:
      ;; backslash and the control characters other than tab, line
      ;; feed, and carriage return (U+0000 to U+0008, U+000B,
      ;; U+000C, U+000E to U+001F, U+007F).
      (setq special-chars-re (rx (in ?\\
                                     (?\u0000 . ?\u0008)
                                     ?\u000B ?\u000C
                                     (?\u000E . ?\u001F)
                                     ?\u007F)))
      (setq begin-q "\"\"\"\n")
      (setq end-q "\"\"\""))
     (t                                 ;Basic quotation "STRING"
      (push '(?\" . ?\") special-chars)
      (push '(?t . ?\t) special-chars) ;U+0009
      (push '(?n . ?\n) special-chars) ;U+000A
      (push '(?r . ?\r) special-chars) ;U+000D
      (setq begin-q "\"")
      (setq end-q begin-q)))
    (and begin-q (insert begin-q))
    (goto-char (prog1 (point) (princ string)))
    (while (re-search-forward special-chars-re nil :noerror)
      (let ((char (preceding-char)))
        (delete-char -1)
        (insert ?\\ (or
                     ;; Escape special characters
                     (car (rassq char special-chars))
                     ;; Fallback: UCS code point in \uNNNN form.
                     (format "u%04x" char)))))
    (and end-q (insert end-q))
    string))

(defun tomelr-encode-string (string)
  "Return a TOML representation of STRING."
  (tomelr--with-output-to-string (tomelr--print-string string)))

(defun tomelr--print-stringlike (object &optional type)
  "Insert OBJECT encoded as a TOML string at point.

TYPE is set to `table' if OBJECT is a TOML Table key.

Return nil if OBJECT cannot be encoded as a TOML string."
  (cond ((stringp object)
         ;; (message "[tomelr--print-stringlike DBG] %S is string" object)
         (tomelr--print-string object))
        ((keywordp object) ;Symbol beginning with `:', like `:some_key'
         ;; (message "[tomelr--print-stringlike DBG] %S is keyword" object)
         (tomelr--print-string
          (string-trim-left (symbol-name object) ":")
          'keyword))
        ((symbolp object)
         (let ((sym-name (symbol-name object)))
           ;; (message "[tomelr--print-stringlike DBG] %S is symbol, type = %S, depth = %d"
           ;;          object type tomelr--print-indentation-depth)
           (cond
            ((equal type 'table)
             (if (null (nth tomelr--print-indentation-depth tomelr--print-table-hierarchy))
                 (progn
                   (push sym-name tomelr--print-table-hierarchy)
                   (setq tomelr--print-table-hierarchy (nreverse tomelr--print-table-hierarchy)))
               ;; Throw away table keys collected at higher depths, if
               ;; any, from earlier runs of this function.
               (setq tomelr--print-table-hierarchy
                     (seq-take tomelr--print-table-hierarchy
                               (1+ tomelr--print-indentation-depth)))
               (setf (nth tomelr--print-indentation-depth tomelr--print-table-hierarchy)
                     sym-name))
             ;; (message "[tomelr--print-stringlike DBG] table hier: %S"
             ;;          tomelr--print-table-hierarchy)
             (princ (format "[%s]" (string-join tomelr--print-table-hierarchy "."))))
            (t
             (princ sym-name)))))))

(defun tomelr--print-key (key &optional type)
  "Insert a TOML key representation of KEY at point.

TYPE is set to `table' if KEY is a TOML Table key.

Signal `tomelr-key-format' if it cannot be encoded as a string."
  (or (tomelr--print-stringlike key type)
      (signal 'tomelr-key-format (list key))))

;;;; Objects
(defun tomelr--toml-table-p (object)
  "Return non-nil if OBJECT can represent a TOML Table."
  ;; TODO: Need to find a robust way of detecting TOML tables.
  ;; (message "[tomelr--print-pair DBG] object type = %S" (type-of object))
  (and (mapp object)
       (consp object)         ;      object = ((KEY . VAL)) <- cons
       (consp (car object)))) ;(car object) =  (KEY . VAL)  <- also cons

(defun tomelr--print-pair (key val)
  "Insert TOML representation of KEY - VAL pair at point."
  (let ((type (cond
               ((tomelr--toml-table-p val) 'table)
               (t nil))))
    ;; (message "[tomelr--print-pair DBG] key = %S, val = %S, type = %S"
    ;;          key val type)
    (when val                     ;Don't print the key if val is nil
      (tomelr--print-indentation) ;Newline before each key in a key-value pair
      (tomelr--print-key key type)
      ;; Skip putting the separator if `type' has a non-nil value like
      ;; `table'.
      (unless type
        (insert tomelr--print-keyval-separator))
      (tomelr--print val))))

(defun tomelr--print-map (map)
  "Insert a TOML representation of MAP at point.
This works for any MAP satisfying `mapp'."
  ;; (message "[tomelr--print-map DBG] map = %S" map)
  (unless (map-empty-p map)
    (tomelr--with-indentation
      (map-do #'tomelr--print-pair map))))

(defun tomelr--print-unordered-map (map)
  "Insert a TOML representation of MAP at point, but optionally sort MAP first.

If `tomelr-encoding-object-sort-predicate' is non-nil, this first
transforms an unsortable MAP into a sortable alist.

See `tomelr-encode-plist' that returns the same as a string."
  (if (and tomelr-encoding-object-sort-predicate
           (not (map-empty-p map)))
      (tomelr--print-alist (map-pairs map) t)
    (tomelr--print-map map)))

;;;; Lists (including alists and plists)
(defun tomelr--print-alist (alist &optional destructive)
  "Insert a TOML representation of ALIST at point.

Sort ALIST first if `tomelr-encoding-object-sort-predicate' is
non-nil.  Sorting can optionally be DESTRUCTIVE for speed.

See `tomelr-encode-alist' that returns the same as a string."
  (tomelr--print-map (if (and tomelr-encoding-object-sort-predicate alist)
                         (sort (if destructive alist (copy-sequence alist))
                               (lambda (a b)
                                 (funcall tomelr-encoding-object-sort-predicate
                                          (car a) (car b))))
                       alist)))

;; The following two are unused but useful to keep around due to the
;; inherent ambiguity of lists.
(defun tomelr-encode-alist (alist)
  "Return a TOML representation of ALIST."
  (tomelr--with-output-to-string (tomelr--print-alist alist)))

(defun tomelr-encode-plist (plist)
  "Return a TOML representation of PLIST."
  (tomelr--with-output-to-string (tomelr--print-unordered-map plist)))
;;

(defun tomelr--print-list (list)
  "Insert a TOML representation of LIST at point."
  (cond ((tomelr-alist-p list) (tomelr--print-alist list))
        ((tomelr-plist-p list) (tomelr--print-unordered-map list))
        ((listp list)          (tomelr--print-array list))
        ((signal 'tomelr-error (list list)))))

;;;; Arrays
(defun tomelr--print-array (array)
  "Insert a TOML representation of ARRAY at point.
See `tomelr-encode-array' that returns the same as a string."
  (insert "[ ")
  (unless (= 0 (length array))
    (tomelr--with-indentation
      (let ((first t))
        (mapc (lambda (elt)
                (if first
                    (setq first nil)
                  (insert ", "))
                (tomelr--print elt))
              array)))
    (insert " "))
  (insert "]"))

(defun tomelr-encode-array (array)
  "Return a TOML representation of ARRAY.
ARRAY can also be a list."
  (tomelr--with-output-to-string (tomelr--print-array array)))

;;;; Print wrapper
(defun tomelr--print (object)
  "Insert a TOML representation of OBJECT at point.
See `tomelr-encode' that returns the same as a string."
  (cond ((tomelr--print-keyword object))
        ((listp object)         (tomelr--print-list object))
        ((tomelr--print-stringlike object))
        ((numberp object)       (prin1 object))
        ((arrayp object)        (tomelr--print-array object))
        ((hash-table-p object)  (tomelr--print-unordered-map object))
        ((signal 'tomelr-error (list object)))))



;;; User API
(defun tomelr-encode (object)
  "Return a TOML representation of OBJECT as a string.
If an error is detected during encoding, an error based on
`tomelr-error' is signaled."
  (string-trim
   (tomelr--with-output-to-string (tomelr--print object))))


(provide 'tomelr)

;;; tomelr.el ends here
