;;; mu-cite.el --- yet another citation tool for GNU Emacs

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         MINOURA Makoto <minoura@netlaputa.or.jp>
;;         Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Maintainer: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Version: $Revision: 7.45 $
;; Keywords: mail, news, citation

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

;;; Commentary:

;; - How to use
;;   1. bytecompile this file and copy it to the apropriate directory.
;;   2. put the following lines to your ~/.emacs:
;;      for EMACS 19 or later and XEmacs
;;		(autoload 'mu-cite/cite-original "mu-cite" nil t)
;;		;; for all but message-mode
;;		(add-hook 'mail-citation-hook 'mu-cite/cite-original)
;;		;; for message-mode only
;;		(setq message-cite-function (function mu-cite/cite-original))
;;      for EMACS 18
;;		;; for all but mh-e
;;		(add-hook 'mail-yank-hooks (function mu-cite/cite-original))
;;		;; for mh-e only
;;		(add-hook 'mh-yank-hooks (function mu-cite/cite-original))

;;; Code:

(require 'std11)
(require 'tl-str)
(require 'tl-list)


;;; @ version
;;;

(defconst mu-cite/RCS-ID
  "$Id: mu-cite.el,v 7.45 1997/01/31 12:31:20 morioka Exp $")
(defconst mu-cite/version (get-version-string mu-cite/RCS-ID))


;;; @ formats
;;;

(defvar mu-cite/cited-prefix-regexp "\\(^[^ \t\n>]+>+[ \t]*\\|^[ \t]*$\\)"
  "*Regexp to match the citation prefix.
If match, mu-cite doesn't insert citation prefix.")

(defvar mu-cite/prefix-format '(prefix-register-verbose "> ")
  "*List to represent citation prefix.
Each elements must be string or method name.")

(defvar mu-cite/top-format '(in-id
			     ">>>>>	" from " wrote:\n")
  "*List to represent top string of citation.
Each elements must be string or method name.")


;;; @ hooks
;;;

(defvar mu-cite-load-hook nil
  "*List of functions called after mu-cite is loaded.
Use this hook to add your own methods to `mu-cite/default-methods-alist'.")

(defvar mu-cite/instantiation-hook nil
  "*List of functions called just before narrowing to the message.")

(defvar mu-cite/pre-cite-hook nil
  "*List of functions called before citing a region of text.")

(defvar mu-cite/post-cite-hook nil
  "*List of functions called after citing a region of text.")


;;; @ field
;;;

(defvar mu-cite/get-field-value-method-alist
  (list (cons 'mh-letter-mode
	      (function
	       (lambda (name)
		 (if (and (stringp mh-sent-from-folder)
			  (numberp mh-sent-from-msg))
		     (save-excursion
		       (set-buffer mh-sent-from-folder)
		       (set-buffer mh-show-buffer)
		       (and (boundp 'mime::preview/article-buffer)
			    (bufferp mime::preview/article-buffer)
			    (set-buffer mime::preview/article-buffer))
		       (std11-field-body name)
		       ))
		 )))))

(defun mu-cite/get-field-value (name)
  (or (std11-field-body name)
      (let ((method (assq major-mode mu-cite/get-field-value-method-alist)))
	(if method
	    (funcall (cdr method) name)
	  ))))


;;; @ prefix registration
;;;

(defvar mu-cite/registration-file
  (expand-file-name "~/.mu-cite.el")
  "*The name of the user environment file for mu-cite.")

(defvar mu-cite/allow-null-string-registration nil
  "*If non-nil, null-string citation-name is registered.")

(defvar mu-cite/registration-symbol 'mu-cite/citation-name-alist)

(defvar mu-cite/citation-name-alist nil)
(load mu-cite/registration-file t t t)
(or (eq 'mu-cite/citation-name-alist mu-cite/registration-symbol)
    (setq mu-cite/citation-name-alist
	  (symbol-value mu-cite/registration-symbol))
    )
(defvar mu-cite/minibuffer-history nil)

;; get citation-name from the database
(defun mu-cite/get-citation-name (from)
  (assoc-value from mu-cite/citation-name-alist)
  )

;; register citation-name to the database
(defun mu-cite/add-citation-name (name from)
  (setq mu-cite/citation-name-alist
        (put-alist from name mu-cite/citation-name-alist))
  (mu-cite/save-to-file)
  )

;; save to file
(defun mu-cite/save-to-file ()
  (let* ((filename mu-cite/registration-file)
	 (buffer (get-buffer-create " *mu-register*")))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-file-name filename)
      (erase-buffer)
      (insert
       (format ";;; %s\n" (file-name-nondirectory filename)))
      (insert
       (format ";;; This file is generated automatically by mu-cite %s.\n\n"
               mu-cite/version))
      (insert (format "(setq %s\n      '(" mu-cite/registration-symbol))
      (insert (mapconcat
	       (function prin1-to-string)
	       mu-cite/citation-name-alist "\n        "))
      (insert "\n        ))\n\n")
      (insert
       (format ";;; %s ends here.\n" (file-name-nondirectory filename)))
      (save-buffer))
    (kill-buffer buffer)))


;;; @ item methods
;;;

;;; @@ ML count
;;;

(defvar mu-cite/ml-count-field-list
  '("X-Ml-Count" "X-Mail-Count" "X-Seqno" "X-Sequence" "Mailinglist-Id")
  "*List of header fields which contain sequence number of mailing list.")

(defun mu-cite/get-ml-count-method ()
  (let ((field-list mu-cite/ml-count-field-list))
    (catch 'tag
      (while field-list
        (let* ((field (car field-list))
               (ml-count (mu-cite/get-field-value field)))
          (if (and ml-count (string-match "[0-9]+" ml-count))
              (throw 'tag
                     (substring ml-count
                                (match-beginning 0)(match-end 0))
                     ))
          (setq field-list (cdr field-list))
          )))))


;;; @@ prefix and registration
;;;

(defun mu-cite/get-prefix-method ()
  (or (mu-cite/get-citation-name (mu-cite/get-value 'address))
      ">")
  )

(defun mu-cite/get-prefix-register-method ()
  (let ((addr (mu-cite/get-value 'address)))
    (or (mu-cite/get-citation-name addr)
	(let ((return
	       (read-string "Citation name? "
			    (or (mu-cite/get-value 'x-attribution)
				(mu-cite/get-value 'full-name))
			    'mu-cite/minibuffer-history)
	       ))
	  (if (and (or mu-cite/allow-null-string-registration
                       (not (string-equal return "")))
                   (y-or-n-p (format "Register \"%s\"? " return)))
	      (mu-cite/add-citation-name return addr)
	    )
	  return))))

(defun mu-cite/get-prefix-register-verbose-method ()
  (let* ((addr (mu-cite/get-value 'address))
         (return1 (mu-cite/get-citation-name addr))
	 (return (read-string "Citation name? "
			      (or return1
				  (mu-cite/get-value 'x-attribution)
				  (mu-cite/get-value 'full-name))
			      'mu-cite/minibuffer-history))
	 )
    (if (and (or mu-cite/allow-null-string-registration
                 (not (string-equal return "")))
             (not (string-equal return return1))
	     (y-or-n-p (format "Register \"%s\"? " return))
	     )
	(mu-cite/add-citation-name return addr)
      )
    return))


;;; @@ set up
;;;

(defvar mu-cite/default-methods-alist
  (list (cons 'from
	      (function
	       (lambda ()
		 (mu-cite/get-field-value "From")
		 )))
	(cons 'date
	      (function
	       (lambda ()
		 (mu-cite/get-field-value "Date")
		 )))
	(cons 'message-id
	      (function
	       (lambda ()
		 (mu-cite/get-field-value "Message-Id")
		 )))
	(cons 'subject
	      (function
	       (lambda ()
		 (mu-cite/get-field-value "Subject")
		 )))
	(cons 'ml-name
	      (function
	       (lambda ()
		 (mu-cite/get-field-value "X-Ml-Name")
		 )))
	(cons 'ml-count (function mu-cite/get-ml-count-method))
	(cons 'address-structure
	      (function
	       (lambda ()
		 (car
		  (std11-parse-address-string (mu-cite/get-value 'from))
		  ))))
	(cons 'full-name
	      (function
	       (lambda ()
		 (std11-full-name-string
		  (mu-cite/get-value 'address-structure))
		 )))
	(cons 'address
	      (function
	       (lambda ()
		 (std11-address-string
		  (mu-cite/get-value 'address-structure))
		 )))
	(cons 'id
	      (function
	       (lambda ()
		 (let ((ml-name (mu-cite/get-value 'ml-name)))
		   (if ml-name
		       (concat "["
			       ml-name
			       " : No."
			       (mu-cite/get-value 'ml-count)
			       "]")
		     (mu-cite/get-value 'message-id)
		     )))))
	(cons 'in-id
	      (function
	       (lambda ()
		 (let ((id (mu-cite/get-value 'id)))
		   (if id
		       (format ">>>>> In %s \n" id)
		     "")))))
	(cons 'prefix (function mu-cite/get-prefix-method))
	(cons 'prefix-register
	      (function mu-cite/get-prefix-register-method))
	(cons 'prefix-register-verbose
	      (function mu-cite/get-prefix-register-verbose-method))
	(cons 'x-attribution
	      (function
	       (lambda ()
                 (mu-cite/get-field-value "X-Attribution")
		 )))
	))


;;; @ fundamentals
;;;

(defvar mu-cite/methods-alist nil)

(defun mu-cite/make-methods ()
  (setq mu-cite/methods-alist
	(copy-alist mu-cite/default-methods-alist))
  (run-hooks 'mu-cite/instantiation-hook)
  )

(defun mu-cite/get-value (item)
  (let ((ret (assoc-value item mu-cite/methods-alist)))
    (if (functionp ret)
	(prog1
	    (setq ret (funcall ret))
	  (set-alist 'mu-cite/methods-alist item ret)
	  )
      ret)))

(defun mu-cite/eval-format (list)
  (mapconcat (function
	      (lambda (elt)
		(cond ((stringp elt) elt)
		      ((symbolp elt) (mu-cite/get-value elt))
		      )))
	     list "")
  )


;;; @ main function
;;;

(defun mu-cite/cite-original ()
  "Citing filter function.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard."
  (interactive)
  (mu-cite/make-methods)
  (save-restriction
    (if (< (mark t) (point))
	(exchange-point-and-mark))
    (narrow-to-region (point)(point-max))
    (run-hooks 'mu-cite/pre-cite-hook)
    (let ((last-point (point))
	  (top (mu-cite/eval-format mu-cite/top-format))
	  (prefix (mu-cite/eval-format mu-cite/prefix-format))
	  )
      (if (re-search-forward "^-*$" nil nil)
	  (forward-line 1)
	)
      (widen)
      (delete-region last-point (point))
      (insert top)
      (setq last-point (point))
      (while (< (point)(mark t))
	(or (looking-at mu-cite/cited-prefix-regexp)
	    (insert prefix))
	(forward-line 1))
      (goto-char last-point)
      )
    (run-hooks 'mu-cite/post-cite-hook)
    ))


;;; @ message editing utilities
;;;
  
(defvar citation-mark-chars ">}|"
  "*String of characters for citation delimiter. [mu-cite.el]")

(defun detect-paragraph-cited-prefix ()
  (save-excursion
    (goto-char (point-min))
    (let ((i 0)
	  (prefix
	   (buffer-substring
	    (progn (beginning-of-line)(point))
	    (progn (end-of-line)(point))
	    ))
	  str ret)
      (while (and (= (forward-line) 0)
		  (setq str (buffer-substring
			     (progn (beginning-of-line)(point))
			     (progn (end-of-line)(point))))
		  (setq ret (string-compare-from-top prefix str))
		  )
	(setq prefix
	      (if (stringp ret)
		  ret
		(second ret)))
	(setq i (1+ i))
	)
      (cond ((> i 1) prefix)
	    ((> i 0)
	     (goto-char (point-min))
	     (save-restriction
	       (narrow-to-region (point)
				 (+ (point)(length prefix)))
	       (goto-char (point-max))
	       (if (re-search-backward
		    (concat "[" citation-mark-chars "]") nil t)
		   (progn
		     (goto-char (match-end 0))
		     (if (looking-at "[ \t]+")
			 (goto-char (match-end 0))
		       )
		     (buffer-substring (point-min)(point))
		     )
		 prefix)))
	    ((progn
	       (goto-char (point-max))
	       (re-search-backward (concat "[" citation-mark-chars "]")
				   nil t)
	       )
	     (goto-char (match-end 0))
	     (if (looking-at "[ \t]+")
		 (goto-char (match-end 0))
	       )
	     (buffer-substring (point-min)(point))
	     )
	    (t ""))
      )))

(defun fill-cited-region (beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char end)
      (and (search-backward "\n" nil t)
	   (setq end (match-end 0))
	   )
      (narrow-to-region beg end)
      (let* ((fill-prefix (detect-paragraph-cited-prefix))
	     (pat (concat fill-prefix "\n"))
	     )
	(goto-char (point-min))
	(while (search-forward pat nil t)
	  (let ((b (match-beginning 0))
		(e (match-end 0))
		)
	    (delete-region b e)
	    (if (and (> b (point-min))
		     (let ((cat (char-category
				 (char-before b))))
		       (or (string-match "a" cat)
			   (string-match "l" cat)
			   ))
		     )
		(insert " ")
	      ))
	  )
	(goto-char (point-min))
	(fill-region (point-min) (point-max))
	))))

(defun compress-cited-prefix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "$") nil t)
    (while (re-search-forward
	    (concat "^\\([ \t]*[^ \t\n" citation-mark-chars "]*["
		    citation-mark-chars "]\\)+") nil t)
      (let* ((b (match-beginning 0))
	     (e (match-end 0))
	     (prefix (buffer-substring b e))
	     ps pe (s 0)
	     (nest (let ((i 0))
		     (if (string-match "<[^<>]+>" prefix)
			 (setq prefix (substring prefix 0 (match-beginning 0)))
		       )
		     (while (string-match
			     (concat "\\([" citation-mark-chars "]+\\)[ \t]*")
			     prefix s)
		       (setq i (+ i (- (match-end 1)(match-beginning 1)))
			     ps s
			     pe (match-beginning 1)
			     s (match-end 0)
			     ))
		     i)))
	(if (and ps (< ps pe))
	    (progn
	      (delete-region b e)
	      (insert (concat (substring prefix ps pe) (make-string nest ?>)))
	      ))))))

(defun replace-top-string (old new)
  (interactive "*sOld string: \nsNew string: ")
  (while (re-search-forward
          (concat "^" (regexp-quote old)) nil t)
    (replace-match new)
    ))


;;; @ end
;;;

(provide 'mu-cite)

(run-hooks 'mu-cite-load-hook)

;;; mu-cite.el ends here
