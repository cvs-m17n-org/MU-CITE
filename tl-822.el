;;; tl-822.el --- RFC 822 parser for GNU Emacs

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author:   MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, RFC 822

;; This file is part of MU (Message Utilities).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-seq)
(require 'tl-str)
(require 'std11)


(defconst rfc822/RCS-ID
  "$Id: tl-822.el,v 7.65 1996-09-08 18:04:57 morioka Exp $")
(defconst rfc822/version (get-version-string rfc822/RCS-ID))


;;; @ header
;;;

(defalias 'rfc822/narrow-to-header	'std11-narrow-to-header)
(defalias 'rfc822/get-header-string	'std11-header-string)
(defalias 'rfc822/get-header-string-except 'std11-header-string-except)
(defalias 'rfc822/get-field-names	'std11-collect-field-names)


;;; @ field
;;;

(defalias 'rfc822/field-end		'std11-field-end)
(defalias 'rfc822/get-field-body	'std11-field-body)
(defalias 'rfc822/get-field-bodies	'std11-field-bodies)


;;; @ quoting
;;;

(defconst rfc822/linear-white-space-regexp "\\(\n?[ \t]\\)+")
(defconst rfc822/quoted-pair-regexp "\\\\.")
(defconst rfc822/qtext-regexp
  (concat "[^" (char-list-to-string std11-non-qtext-char-list) "]"))
(defconst rfc822/quoted-string-regexp
  (concat "\""
	  (regexp-*
	   (regexp-or rfc822/qtext-regexp rfc822/quoted-pair-regexp)
	   )
	  "\""))

(defun rfc822/strip-quoted-pair (str)
  (let ((dest "")
	(i 0)
	(len (length str))
	chr flag)
    (while (< i len)
      (setq chr (elt str i))
      (if (or flag (not (eq chr ?\\)))
	  (progn
	    (setq dest (concat dest (char-to-string chr)))
	    (setq flag nil)
	    )
	(setq flag t)
	)
      (setq i (+ i 1))
      )
    dest))

(defun rfc822/strip-quoted-string (str)
  (rfc822/strip-quoted-pair
   (let ((max (- (length str) 1))
	 )
     (if (and (eq (elt str 0) ?\")
	      (eq (elt str max) ?\")
	      )
	 (substring str 1 max)
       str)
     )))


;;; @ unfolding
;;;

(defalias 'rfc822/unfolding-string 'std11-unfold-string)


;;; @ lexical analyze
;;;

(defalias 'rfc822/lexical-analyze 'std11-lexical-analyze)


;;; @ parser
;;;

(defalias 'rfc822/parse-address		'std11-parse-address)
(defalias 'rfc822/parse-addresses	'std11-parse-addresses)
(defalias 'rfc822/address-string	'std11-address-string)
(defalias 'rfc822/full-name-string	'std11-full-name-string)
(defalias 'rfc822/extract-address-components
  'std11-extract-address-components)


;;; @ end
;;;

(provide 'tl-822)

;;; tl-822.el ends here
