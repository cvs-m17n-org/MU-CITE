;;; mu-bbdb.el --- `attribution' function for mu-cite with BBDB.

;; Copyright (C) 1996 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Version: $Id: mu-bbdb.el,v 3.2 1996/11/22 08:44:27 shuhei-k Exp $

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  - How to use
;;    1. bytecompile this file and copy it to the apropriate directory.
;;    2. put the following lines to your ~/.emacs:
;;		(require 'tl-misc)
;;		(call-after-loaded 'mu-cite
;;				   (function
;;				    (lambda ()
;;				      (require 'mu-bbdb)
;;				      )))


;;; Code:

(require 'mu-cite)
(if (not (fboundp 'eval-when))
    (require 'bbdb)
  (eval-when (compile)
    (ignore-errors
      (require 'bbdb)))
  (eval-when (load eval)
    (require 'bbdb))
  )

(defvar mu-bbdb-load-hook nil
  "*List of functions called after mu-bbdb is loaded.")

;;; @@ prefix and registration using BBDB
;;;

(defun mu-cite/get-bbdb-prefix-method ()
  (or (mu-cite/get-bbdb-attr (mu-cite/get-value 'address))
      ">")
  )

(defun mu-cite/get-bbdb-attr (addr)
  "Extract attribute information from BBDB."
  (let ((record (bbdb-search-simple nil addr)))
    (and record
         (bbdb-record-getprop record 'attribution))
    ))

(defun mu-cite/set-bbdb-attr (attr addr)
  "Add attribute information to BBDB."
  (let* ((bbdb-notice-hook nil)
         (record (bbdb-annotate-message-sender 
                  addr t
	          (bbdb-invoke-hook-for-value 
	           bbdb/mail-auto-create-p)
		  t)))
    (if record
        (progn
          (bbdb-record-putprop record 'attribution attr)
          (bbdb-change-record record nil))
      )))

(defun mu-cite/get-bbdb-prefix-register-method ()
  (let ((addr (mu-cite/get-value 'address)))
    (or (mu-cite/get-bbdb-attr addr)
    	(let ((return
	       (read-string "Citation name? "
			    (or (mu-cite/get-value 'x-attribution)
				(mu-cite/get-value 'full-name))
			    'mu-cite/minibuffer-history)
	       ))
	  (if (and (not (string-equal return ""))
                   (y-or-n-p (format "Register \"%s\"? " return)))
	      (mu-cite/set-bbdb-attr return addr)
	    )
	  return))))

(defun mu-cite/get-bbdb-prefix-register-verbose-method ()
  (let* ((addr (mu-cite/get-value 'address))
         (attr (mu-cite/get-bbdb-attr addr))
	 (return (read-string "Citation name? "
			      (or attr
				  (mu-cite/get-value 'x-attribution)
				  (mu-cite/get-value 'full-name))
			      'mu-cite/minibuffer-history))
	 )
    (if (and (not (string-equal return ""))
             (not (string-equal return attr))
	     (y-or-n-p (format "Register \"%s\"? " return))
	     )
	(mu-cite/set-bbdb-attr return addr)
      )
    return))

(or (assoc 'bbdb-prefix mu-cite/default-methods-alist)
    (setq mu-cite/default-methods-alist
          (append mu-cite/default-methods-alist
                  (list
                   (cons 'bbdb-prefix
                         (function mu-cite/get-bbdb-prefix-method))
                   (cons 'bbdb-prefix-register
                         (function mu-cite/get-bbdb-prefix-register-method))
                   (cons 'bbdb-prefix-register-verbose
                         (function
                          mu-cite/get-bbdb-prefix-register-verbose-method))
                   ))))


;;; @ end
;;;

(provide 'mu-bbdb)

(run-hooks 'mu-bbdb-load-hook)

;;; mu-bbdb.el ends here
