;;; mu-bbdb.el --- `attribution' function for mu-cite with BBDB.

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, news, citation, bbdb

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  - How to use
;;    1. bytecompile this file and copy it to the apropriate directory.
;;    2. put the following lines to your ~/.emacs:
;;             (add-hook 'mu-cite-load-hook
;;                       (function
;;                        (lambda ()
;;                          (require 'mu-bbdb)
;;                          )))


;;; Code:

(eval-when-compile (require 'cl))

;; Pickup `module-installed-p'.
(require 'path-util)

(require 'mu-cite)
(when (module-installed-p 'bbdb)
  (require 'bbdb))


;;; @ obsolete functions
;;;

;; This part will be abolished in the future.

(eval-and-compile
  (defconst mu-bbdb-obsolete-function-alist
    '((mu-cite/get-bbdb-attr		mu-bbdb-get-attr)
      (mu-cite/get-bbdb-prefix-method	mu-bbdb-get-prefix-method)
      (mu-cite/get-bbdb-prefix-register-method
       mu-bbdb-get-prefix-register-method)
      (mu-cite/get-bbdb-prefix-register-verbose-method
       mu-bbdb-get-prefix-register-verbose-method)
      (mu-cite/set-bbdb-attr		mu-bbdb-set-attr)))

  (mapcar
   (function (lambda (elem)
	       (apply (function define-obsolete-function-alias) elem)))
   mu-bbdb-obsolete-function-alist)
  )


;;; @ set up
;;;

(defgroup mu-bbdb nil
  "`attribution' function for mu-cite with BBDB."
  :prefix "mu-bbdb-"
  :group 'mu-cite
  :group 'bbdb)

(defcustom mu-bbdb-load-hook nil
  "List of functions called after mu-bbdb is loaded."
  :type 'hook
  :group 'mu-bbdb)


;;; @@ prefix and registration using BBDB
;;;

(defun mu-bbdb-get-prefix-method ()
  (or (mu-bbdb-get-attr (mu-cite-get-value 'address))
      ">"))

(defun mu-bbdb-get-attr (addr)
  "Extract attribute information from BBDB."
  (let ((record (bbdb-search-simple nil addr)))
    (when record
      (bbdb-record-getprop record 'attribution))))

(defun mu-bbdb-set-attr (attr addr)
  "Add attribute information to BBDB."
  (let* ((bbdb-notice-hook nil)
	 (record (bbdb-annotate-message-sender
		  addr t
		  (bbdb-invoke-hook-for-value
		   bbdb/mail-auto-create-p)
		  t)))
    (when record
      (bbdb-record-putprop record 'attribution attr)
      (bbdb-change-record record nil))))

(defun mu-bbdb-get-prefix-register-method ()
  (let ((addr (mu-cite-get-value 'address)))
    (or (mu-bbdb-get-attr addr)
	(let ((return
	       (read-string "Citation name? "
			    (or (mu-cite-get-value 'x-attribution)
				(mu-cite-get-value 'full-name))
			    'mu-cite-minibuffer-history)))
	  (if (and (not (string-equal return ""))
		   (y-or-n-p (format "Register \"%s\"? " return)))
	      (mu-bbdb-set-attr return addr))
	  return))))

(defun mu-bbdb-get-prefix-register-verbose-method ()
  (let* ((addr (mu-cite-get-value 'address))
	 (attr (mu-bbdb-get-attr addr))
	 (return (read-string "Citation name? "
			      (or attr
				  (mu-cite-get-value 'x-attribution)
				  (mu-cite-get-value 'full-name))
			      'mu-cite-minibuffer-history)))
    (if (and (not (string-equal return ""))
	     (not (string-equal return attr))
	     (y-or-n-p (format "Register \"%s\"? " return)))
	(mu-bbdb-set-attr return addr))
    return))

(unless (assoc 'bbdb-prefix mu-cite-default-methods-alist)
  (setq mu-cite-default-methods-alist
	(append mu-cite-default-methods-alist
		(list
		 (cons 'bbdb-prefix
		       (function mu-bbdb-get-prefix-method))
		 (cons 'bbdb-prefix-register
		       (function mu-bbdb-get-prefix-register-method))
		 (cons 'bbdb-prefix-register-verbose
		       (function
			mu-bbdb-get-prefix-register-verbose-method))))))


;;; @ end
;;;

(provide 'mu-bbdb)

(run-hooks 'mu-bbdb-load-hook)

;;; mu-bbdb.el ends here
