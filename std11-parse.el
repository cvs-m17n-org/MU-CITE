;;; std11-parse.el --- STD 11 parser for GNU Emacs

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author:   MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, RFC 822, STD 11
;; Version: $Id: std11-parse.el,v 0.7 1996-08-28 18:07:03 morioka Exp $

;; This file is part of tl (Tiny Library).

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

(require 'std11)


;;; @ lexical analyze
;;;

(defconst std11-space-chars " \t\n")
(defconst std11-spaces-regexp (concat "^[" std11-space-chars "]+"))
(defconst std11-special-chars "][()<>@,;:\\<>.\"")
(defconst std11-atom-regexp
  (concat "^[^" std11-special-chars std11-space-chars "]+"))

(defun std11-analyze-spaces (str)
  (if (string-match std11-spaces-regexp str)
      (let ((end (match-end 0)))
	(cons (cons 'spaces (substring str 0 end))
	      (substring str end)
	      ))))

(defun std11-analyze-special (str)
  (if (and (> (length str) 0)
	   (find (aref str 0) std11-special-chars)
	   )
      (cons (cons 'specials (substring str 0 1))
	    (substring str 1)
	    )))

(defun std11-analyze-atom (str)
  (if (string-match std11-atom-regexp str)
      (let ((end (match-end 0)))
	(cons (cons 'atom (substring str 0 end))
	      (substring str end)
	      ))))

(defun std11-check-enclosure (str open close &optional recursive from)
  (let ((len (length str))
	(i (or from 0))
	)
    (if (and (> len i)
	     (eq (aref str i) open))
	(let (p chr dest)
	  (setq i (1+ i))
	  (catch 'tag
	    (while (< i len)
	      (setq chr (aref str i))
	      (cond ((eq chr ?\\)
		     (setq i (1+ i))
		     (if (>= i len)
			 (throw 'tag nil)
		       )
		     (setq i (1+ i))
		     )
		    ((eq chr close)
		     (throw 'tag (1+ i))
		     )
		    ((eq chr open)
		     (if (and recursive
			      (setq p (std11-check-enclosure
				       str open close recursive i))
			      )
			 (setq i p)
		       (throw 'tag nil)
		       ))
		    (t
		     (setq i (1+ i))
		     ))
	      ))))))

(defun std11-analyze-quoted-string (str)
  (let ((p (std11-check-enclosure str ?\" ?\")))
    (if p
	(cons (cons 'quoted-string (substring str 1 (1- p)))
	      (substring str p))
      )))

(defun std11-analyze-domain-literal (str)
  (let ((p (std11-check-enclosure str ?\[ ?\])))
    (if p
	(cons (cons 'domain-literal (substring str 1 (1- p)))
	      (substring str p))
      )))

(defun std11-analyze-comment (str)
  (let ((p (std11-check-enclosure str ?\( ?\) t)))
    (if p
	(cons (cons 'comment (substring str 1 (1- p)))
	      (substring str p))
      )))


;;; @ end
;;;

(provide 'std11-parse)

;;; std11-parse.el ends here
