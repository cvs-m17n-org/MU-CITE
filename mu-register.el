;;;
;;; mu-register.el --- `register' function for mu-cite.
;;;
;;; Copyright (C) 1995 MINOURA Makoto
;;;
;;; Author: MINOURA Makoto <minoura@leo.bekkoame.or.jp>
;;;
;;; This file is not part of tm (Tools for MIME).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;;
;;; - How to install.
;;;   1. bytecompile this file and copy it to the apropriate directory.
;;;   2. put the following lines to your .emacs.
;;;     (add-hook 'mu-cite-load-hook
;;;                (function
;;;                 (lambda ()
;;;                   (require 'mu-register))))
;;;   3. you can use the keyword `registered' in your
;;;    mu-cite/top-form and mu-cite/prefix-form, for example:
;;;     (setq mu-cite/prefix-format (list 'registered "> "))
;;;
;;; - ChangeLog.
;;;   Wed Dec 27 14:28:17 1995  MINOURA Makoto <minoura@leo.bekkoame.or.jp>
;;;
;;;	* Written.
;;;

;;; Code:

(defvar mu-register/registration-file
  (expand-file-name "~/.mu-register")
  "*The name of the user environment file for mu-register.")

(defvar mu-register/citation-name-alist nil)
(load mu-register/registration-file t t t)

(defvar mu-register/minibuffer-history nil)


;;
;; from から引用名を得る
(defsubst mu-register/get-citation-name (from)
  (cdr (assoc from mu-register/citation-name-alist)))

;;
;; 引用名を登録する
(defun mu-register/add-citation-name (name from)
  (let* ((elt (assoc from mu-register/citation-name-alist)))
    (if elt
	(setq mu-register/citation-name-alist
	      (delq elt mu-register/citation-name-alist)))
    (setq elt (cons from name))
    (setq mu-register/citation-name-alist
	  (cons elt
		mu-register/citation-name-alist))
    (mu-register/save-to-file)
    ))

;;
;; 実際に呼び出される関数
(defun mu-register/citation-name ()
  (let* ((from
	  (rfc822/address-string
	   (car (rfc822/parse-address
		 (rfc822/lexical-analyze
		  (mu-cite/get-value 'from))))))
	 (fullname (mu-cite/get-value 'full-name))
	 (return1
	  (mu-register/get-citation-name from))
	 (return))
    (if (null return1)
	(setq return1 fullname))
    (setq return
	  (read-string "Citation name? "
		       return1
		       'mu-register/minibuffer-history))
    (if (not (string-equal return return1))
	(let ((ans)
	      (cursor-in-echo-area t))
	  (while (null ans)
 	    (message (format "Register \"%s\" (y/n)? " return))
	    (setq ans (read-event))
	    (if (not (or (eq ans ?y)
			 (eq ans ?n)))
		(setq ans nil)))
	  (message "")
	  (if (eq ans ?y)
	      (mu-register/add-citation-name return from))))
    return))

;;
;; ファイルに保存
(defun mu-register/save-to-file ()
  (let* ((filename mu-register/registration-file)
	 (buffer (get-buffer-create " *mu-register*")))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-file-name filename)
      (erase-buffer)
      (insert ";; generated automatically by mu-register.\n")
      (insert "(setq mu-register/citation-name-alist\n\
 (quote\n\
  ")
      (insert (prin1-to-string mu-register/citation-name-alist))
      (insert "))\n")
      (save-buffer))
    (kill-buffer buffer)))


;;
;; Installation

(require 'mu-cite)
(if (null (assoc 'registered mu-cite/default-methods-alist))
    (setq mu-cite/default-methods-alist
	  (cons (cons 'registered (function mu-register/citation-name))
		mu-cite/default-methods-alist)))

;;
;; provide
(provide 'mu-register)

;;; mu-register.el ends here
