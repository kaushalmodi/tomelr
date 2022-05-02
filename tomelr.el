;;; tomelr.el --- Convert s-expressions to TOML           -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.modi@gmail.com>
;; URL: https://github.com/kaushalmodi/tomelr
;; Package-Requires: ((emacs "26.3"))
;; Keywords: data, tools
;; Version: 0.0.3

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

;; tomelr.el is a library for converting Lisp data expressions to TOML
;; (https://toml.io/en/).

;;; Code:

(require 'json)
(require 'map)
(require 'subr-x)  ;For `string-trim' on Emacs versions 27.2 and older


;;; Variables

(defvar tomelr-false '(:false 'false "false")
  "S-exp values to be interpreted as TOML `false'.")

(defvar tomelr-encoding-default-indentation "  "
  "String used for a single indentation level during encoding.
This value is repeated for each further nested element.")

;;;; Internal Variables
(defvar tomelr--print-indentation-prefix "\n"
  "String used to start indentation during encoding.")

(defvar tomelr--print-indentation-depth -1
  "Current indentation level during encoding.
Dictates repetitions of `tomelr-encoding-default-indentation'.")

(defvar tomelr--print-table-hierarchy ()
  "Internal variable used to save TOML Table hierarchies.
This variable is used for both TOML Tables and Arrays of TOML
Tables.")

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

;;;; Booleans
(defun tomelr--print-boolean (object)
  "Insert TOML boolean true or false at point if OBJECT is a boolean.
Return nil if OBJECT is not recognized as a TOML boolean."
  (prog1 (setq object (cond ((eq object t) "true")
                            ((member object tomelr-false) "false")))
    (and object (insert object))))

;;;; Strings
(defun tomelr--print-string (string &optional type)
  "Insert a TOML representation of STRING at point.

Optional TYPE arg gives more information about the input STRING.
For example, if the string is the name of a TOML key, it will be
set to `keyword'.

Return the same STRING passed as input."
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

(defun tomelr--print-stringlike (object &optional type)
  "Insert OBJECT encoded as a TOML string at point.

Possible value of TYPE are `table', `table-array' or nil.

Return nil if OBJECT cannot be encoded as a TOML string."
  (let ((sym-name (cond ((stringp object)
                         object)
                        ;; Symbol beginning with `:', like `:some_key'
                        ((keywordp object)
                         (string-trim-left (symbol-name object) ":"))
                        ((symbolp object)
                         (symbol-name object)))))
    (when type
      ;; (message "[tomelr--print-stringlike DBG] %S is symbol, type = %S, depth = %d"
      ;;          object type tomelr--print-indentation-depth)
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
      )
    (cond
     ((equal type 'table)
      (princ (format "[%s]" (string-join tomelr--print-table-hierarchy "."))))
     ((equal type 'table-array)
      (princ (format "[[%s]]" (string-join tomelr--print-table-hierarchy "."))))
     ((stringp object)
      ;; (message "[tomelr--print-stringlike DBG] %S is string" object)
      (tomelr--print-string sym-name))
     ((keywordp object)
      ;; (message "[tomelr--print-stringlike DBG] %S is keyword" object)
      (tomelr--print-string sym-name 'keyword))
     (sym-name
      (princ sym-name)))))

(defun tomelr--print-key (key &optional type)
  "Insert a TOML key representation of KEY at point.

TYPE is set to `table' if KEY is a TOML Table key.

Signal `tomelr-key-format' if it cannot be encoded as a string."
  (or (tomelr--print-stringlike key type)
      (signal 'tomelr-key-format (list key))))

;;;; Objects
;; `tomelr-alist-p' is a slightly modified version of `json-alist-p'.
;; It fixes this scenario: (json-alist-p '((:a 1))) return t, which is wrong.
;; '((:a 1)) is an array of plist format maps, and not an alist.
;; (tomelr-alist-p '((:a 1))) returns nil as expected.
(defun tomelr-alist-p (list)
  "Non-nil if and only if LIST is an alist with simple keys."
  (declare (pure t) (side-effect-free error-free))
  (while (and (consp (car-safe list))
              (not (json-plist-p (car-safe list)))
              (atom (caar list)))
    ;; (message "[tomelr-alist-p DBG] INSIDE list = %S, car = %S, caar = %S, atom of caar = %S"
    ;;          list (car-safe list) (caar list) (atom (caar list)))
    (setq list (cdr list)))
  ;; (message "[tomelr-alist-p DBG] out 2 list = %S, is alist? %S" list (null list))
  (null list))

(defun tomelr--toml-table-p (object)
  "Return non-nil if OBJECT can represent a TOML Table.

Recognize both alist and plist format maps as TOML Tables.

Examples:

- Alist format: \\='((a . 1) (b . \"foo\"))
- Plist format: \\='(:a 1 :b \"foo\")"
  (or (tomelr-alist-p object)
      (json-plist-p object)))

(defun tomelr--print-pair (key val)
  "Insert TOML representation of KEY - VAL pair at point."
  (let ((type (cond
               ((tomelr--toml-table-p val) 'table)
               ((tomelr--toml-table-array-p val) 'table-array)
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

;;;; Lists (including alists and plists)
(defun tomelr--print-list (list)
  "Insert a TOML representation of LIST at point."
  (cond ((tomelr--toml-table-p list)
         (tomelr--print-map list))
        ((listp list)
         (tomelr--print-array list))
        ((signal 'tomelr-error (list list)))))

;;;; Arrays
(defun tomelr--toml-table-array-p (object)
  "Return non-nil if OBJECT can represent a TOML Table Array.

Definition of a TOML Table Array (TTA):

- OBJECT is TTA if it is of type ((TT1) (TT2) ..) where each element is a
  TOML Table (TT)."
  (when (or (listp object)
            (vectorp object))
    (seq-every-p
     (lambda (elem) (tomelr--toml-table-p elem))
     object)))

(defun tomelr--print-tta-key ()
  "Print TOML Table Array key."
  ;; (message "[tomelr--print-array DBG] depth = %d" tomelr--print-indentation-depth)
  ;; Throw away table keys collected at higher depths, if
  ;; any, from earlier runs of this function.
  (setq tomelr--print-table-hierarchy
        (seq-take tomelr--print-table-hierarchy
                  (1+ tomelr--print-indentation-depth)))

  (tomelr--print-indentation)
  (insert
   (format "[[%s]]" (string-join tomelr--print-table-hierarchy "."))))

(defun tomelr--print-array (array)
  "Insert a TOML representation of ARRAY at point."
  ;; (message "[tomelr--print-array DBG] array = %S, TTA = %S"
  ;;          array (tomelr--toml-table-array-p array))
  (cond
   ((tomelr--toml-table-array-p array)
    (unless (= 0 (length array))
      (let ((first t))
        (mapc (lambda (elt)
                (if first
                    (setq first nil)
                  (tomelr--print-tta-key))
                (tomelr--print elt))
              array))))
   (t
    (insert "[")
    (unless (= 0 (length array))
      (tomelr--with-indentation
        (let ((first t))
          (mapc (lambda (elt)
                  (if first
                      (setq first nil)
                    (insert ", "))
                  (tomelr--print elt))
                array))))
    (insert "]"))))

;;;; Print wrapper
(defun tomelr--print (object)
  "Insert a TOML representation of OBJECT at point.
See `tomelr-encode' that returns the same as a string."
  (cond ((tomelr--print-boolean object))
        ((listp object)         (tomelr--print-list object))
        ((tomelr--print-stringlike object))
        ((numberp object)       (prin1 object))
        ((arrayp object)        (tomelr--print-array object))
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
