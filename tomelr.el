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


;;; Variables

(defvar tomelr-false '(:false 'false "false")
  "S-exp values to be interpreted as TOML `false'.")

(defvar tomelr-encoding-default-indentation "  "
  "String used for a single indentation level during encoding.
This value is repeated for each further nested element.")

(defvar tomelr-encoding-lisp-style-closings nil
  "If non-nil, delimiters ] and } will be formatted Lisp-style.
This means they will be placed on the same line as the last
element of the respective array or object, without indentation.")

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

(defvar tomelr--print-keyval-separator " = "
  "String used to separate key-value pairs during encoding.")



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
(defconst tomelr-special-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which are escaped in TOML, with their Elisp counterparts.")

(defun tomelr--print-string (string &optional from)
  "Insert a TOML representation of STRING at point.
FROM is the index of STRING to start from and defaults to 0."
  ;; (message "[tomelr--print-string DBG] string = %s" string)
  (insert ?\")
  (goto-char (prog1 (point) (princ string)))
  (and from (delete-char from))
  ;; Escape only quotation mark, backslash, and the control
  ;; characters U+0000 to U+001F (RFC 4627, ECMA-404).
  (while (re-search-forward (rx (in ?\" ?\\ cntrl)) nil 'move)
    (let ((char (preceding-char)))
      (delete-char -1)
      (insert ?\\ (or
                   ;; Special TOML character (\n, \r, etc.).
                   (car (rassq char tomelr-special-chars))
                   ;; Fallback: UCS code point in \uNNNN form.
                   (format "u%04x" char)))))
  (insert ?\")
  string)

(defun tomelr-encode-string (string)
  "Return a TOML representation of STRING."
  (tomelr--with-output-to-string (tomelr--print-string string)))

(defun tomelr--print-stringlike (object)
  "Insert OBJECT encoded as a TOML string at point.
Return nil if OBJECT cannot be encoded as a TOML string."
  (cond ((stringp object)
         ;; (message "[tomelr--print-stringlike DBG] string")
         (tomelr--print-string object))
        ((keywordp object)
         ;; (message "[tomelr--print-stringlike DBG] keyword")
         (tomelr--print-string (symbol-name object) 1))
        ((symbolp object)
         ;; (message "[tomelr--print-stringlike DBG] symbol")
         (princ (symbol-name object))
         ;; (tomelr--print-string (symbol-name object))
         )))

(defun tomelr--print-key (object)
  "Insert a TOML key representation of OBJECT at point.
Signal `tomelr-key-format' if it cannot be encoded as a string."
  (or (tomelr--print-stringlike object)
      (signal 'tomelr-key-format (list object))))

;;;; Objects
(defun tomelr--print-pair (key val)
  "Insert TOML representation of KEY-VAL pair at point."
  (tomelr--print-indentation) ;Newline before each key in a key-value pair
  (tomelr--print-key key)
  (insert tomelr--print-keyval-separator)
  (tomelr--print val))

(defun tomelr--print-map (map)
  "Insert TOML object representation of MAP at point.
This works for any MAP satisfying `mapp'."
  (unless (map-empty-p map)
    (tomelr--with-indentation
      (map-do #'tomelr--print-pair map))))

(defun tomelr--print-unordered-map (map)
  "Like `tomelr--print-map', but optionally sort MAP first.
If `tomelr-encoding-object-sort-predicate' is non-nil, this first
transforms an unsortable MAP into a sortable alist."
  (if (and tomelr-encoding-object-sort-predicate
           (not (map-empty-p map)))
      (tomelr--print-alist (map-pairs map) t)
    (tomelr--print-map map)))

;;;; Lists (including alists and plists)
(defun tomelr--print-alist (alist &optional destructive)
  "Insert a TOML representation of ALIST at point.
Sort ALIST first if `tomelr-encoding-object-sort-predicate' is
non-nil.  Sorting can optionally be DESTRUCTIVE for speed."
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
  "Like `tomelr-encode-list', but insert the TOML at point."
  (cond ((tomelr-alist-p list) (tomelr--print-alist list))
        ((tomelr-plist-p list) (tomelr--print-unordered-map list))
        ((listp list)          (tomelr--print-array list))
        ((signal 'tomelr-error (list list)))))

;;;; Arrays
(defun tomelr--print-array (array)
  "Like `tomelr-encode-array', but insert the TOML at point."
  (insert ?\[)
  (unless (length= array 0)
    (tomelr--with-indentation
      (tomelr--print-indentation)
      (let ((first t))
        (mapc (lambda (elt)
                (if first
                    (setq first nil)
                  (insert ",")
                  (tomelr--print-indentation))
                (tomelr--print elt))
              array)))
    (or tomelr-encoding-lisp-style-closings
        (tomelr--print-indentation)))
  (insert ?\]))

(defun tomelr-encode-array (array)
  "Return a TOML representation of ARRAY.
ARRAY can also be a list."
  (tomelr--with-output-to-string (tomelr--print-array array)))

;;;; Print wrapper
(defun tomelr--print (object)
  "Like `tomelr-encode', but insert or print the TOML at point."
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
