;;; mu-register.el --- registration feature of mu-cite

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MINOURA Makoto <minoura@netlaputa.or.jp>
;;         MORIOKA Tomohiko <tomo@m17n.org>
;;; Created: 1995-12-27 by MINOURA Makoto
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: registration, citation, mail, news

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'mu-cite)


;;; @ variables
;;;

(defcustom mu-registration-file (expand-file-name "~/.mu-cite.el")
  "The name of the user environment file for mu-cite."
  :type 'file
  :group 'mu-cite)

(defcustom mu-registration-file-modes 384
  "Mode bits of `mu-registration-file', as an integer."
  :type 'integer
  :group 'mu-cite)

(defcustom mu-registration-file-coding-system-for-read nil
  "Coding system used when reading a registration file."
  :group 'mu-cite)

(defcustom mu-cite-allow-null-string-registration nil
  "If non-nil, null-string citation-name is registered."
  :type 'boolean
  :group 'mu-cite)

(defvar mu-registration-symbol 'mu-citation-name-alist
  "*Name of the variable to register citation prefix strings.")

(defvar mu-registration-file-coding-system nil
  "Coding system used when writing a current registration file.")

(defvar mu-citation-name-alist nil)

(defvar mu-register-history nil)

(eval-when-compile (require 'static))

(static-when (featurep 'xemacs)
  (define-obsolete-variable-alias
    'mu-cite/registration-file 'mu-registration-file)

  (define-obsolete-variable-alias
    'mu-cite/allow-null-string-registration
    'mu-cite-allow-null-string-registration)

  (define-obsolete-variable-alias
    'mu-cite/registration-symbol 'mu-registration-symbol)
  )


;;; @ load / save registration file
;;;

(defun mu-cite-load-registration-file ()
  (if (file-readable-p mu-registration-file)
      (with-temp-buffer
	(insert-file-contents-as-coding-system
	 mu-registration-file-coding-system-for-read
	 mu-registration-file)
	(setq mu-registration-file-coding-system
	      buffer-file-coding-system)
	(let ((exp (read (current-buffer))))
	  (or (eq (car (cdr exp)) mu-registration-symbol)
	      (setcar (cdr exp) mu-registration-symbol))
	  (eval exp)))))

(defun mu-cite-save-registration-file ()
  (with-temp-buffer
    (insert ";;; " (file-name-nondirectory mu-registration-file) "\n")
    (insert ";;; This file is generated automatically by mu-cite "
	    mu-cite-version "\n\n")
    (insert "(setq "
	    (symbol-name mu-registration-symbol)
	    "\n      '(")
    (insert (mapconcat
	     (function prin1-to-string)
	     mu-citation-name-alist "\n        "))
    (insert "\n        ))\n\n")
    (insert ";;; "
	    (file-name-nondirectory mu-registration-file)
	    " ends here.\n")
    (write-region-as-coding-system mu-registration-file-coding-system
				   (point-min)(point-max)
				   mu-registration-file nil 'nomsg)
    (condition-case nil
	(set-file-modes mu-registration-file mu-registration-file-modes)
      (error nil))))


;;; @ database accessors
;;;

;; get citation-name from the database
(defun mu-register-get-citation-name (from)
  (cdr (assoc from mu-citation-name-alist)))

;; register citation-name to the database
(defun mu-register-add-citation-name (name from)
  (setq mu-citation-name-alist
	(put-alist from name mu-citation-name-alist))
  (mu-cite-save-registration-file))


;;; @ methods
;;;

;;;###autoload
(defun mu-cite-get-prefix-method ()
  (or (mu-register-get-citation-name (mu-cite-get-value 'address))
      ">"))

;;;###autoload
(defun mu-cite-get-prefix-register-method ()
  (let ((addr (mu-cite-get-value 'address)))
    (or (mu-register-get-citation-name addr)
	(let ((return
	       (read-string "Citation name? "
			    (or (mu-cite-get-value 'x-attribution)
				(mu-cite-get-value 'full-name))
			    'mu-register-history)))
	  (when (and (or mu-cite-allow-null-string-registration
			 (not (string-equal return "")))
		     (y-or-n-p (format "Register \"%s\"? " return)))
	    (mu-register-add-citation-name return addr))
	  return))))

;;;###autoload
(defun mu-cite-get-prefix-register-verbose-method ()
  (let* ((addr (mu-cite-get-value 'address))
	 (return1 (mu-register-get-citation-name addr))
	 (return (read-string "Citation name? "
			      (or return1
				  (mu-cite-get-value 'x-attribution)
				  (mu-cite-get-value 'full-name))
			      'mu-register-history)))
    (when (and (or mu-cite-allow-null-string-registration
		   (not (string-equal return "")))
	       (not (string-equal return return1))
	       (y-or-n-p (format "Register \"%s\"? " return)))
      (mu-register-add-citation-name return addr))
    return))


;;; @ end
;;;

(provide 'mu-register)

(mu-cite-load-registration-file)

;;; mu-register.el ends here
