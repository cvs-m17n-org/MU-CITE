;;; tl-822.el --- RFC 822 parser for GNU Emacs

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author:   MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, RFC 822

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

(require 'tl-seq)
(require 'tl-str)
(require 'std11)


(defconst rfc822/RCS-ID
  "$Id: tl-822.el,v 7.36 1996-08-28 12:28:54 morioka Exp $")
(defconst rfc822/version (get-version-string rfc822/RCS-ID))


;;; @ header
;;;

(defalias 'rfc822/narrow-to-header 'std11-narrow-to-header)
(defalias 'rfc822/get-header-string 'std11-header-string)
(defalias 'rfc822/get-header-string-except 'std11-header-string-except)


;;; @ field
;;;

(defconst rfc822/field-name-regexp "[!-9;-~]+")

(defconst rfc822/field-top-regexp
  (concat "\\(" rfc822/field-name-regexp "\\):"))

(defconst rfc822::next-field-top-regexp (concat "\n" rfc822/field-top-regexp))

(defun rfc822/get-field-names (&optional boundary)
  (save-excursion
    (save-restriction
      (rfc822/narrow-to-header boundary)
      (goto-char (point-min))
      (let ((pat (concat "^\\(" rfc822/field-name-regexp "\\):"))
	    dest name)
	(while (re-search-forward pat nil t)
	  (setq name (buffer-substring (match-beginning 1)(match-end 1)))
	  (or (member name dest)
	      (setq dest (cons name dest))
	      )
	  )
	dest))))

(defalias `rfc822/field-end 'std11-field-end)

(defun rfc822/get-field-body (name &optional boundary)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(rfc822/narrow-to-header boundary)
	(goto-char (point-min))
	(if (re-search-forward (concat "^" name ":[ \t]*") nil t)
	    (buffer-substring-no-properties
	     (match-end 0)
	     (rfc822/field-end)
	     ))
	))))

(defun rfc822/get-field-bodies (field-names &optional default-value boundary)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(rfc822/narrow-to-header boundary)
	(let* ((dest (make-list (length field-names) default-value))
	       (s-rest field-names)
	       (d-rest dest)
	       field-name)
	  (while (setq field-name (car s-rest))
	    (goto-char (point-min))
	    (if (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
		(setcar d-rest
			(buffer-substring-no-properties
			 (match-end 0)
			 (rfc822/field-end))))
	    (setq s-rest (cdr s-rest)
		  d-rest (cdr d-rest))
	    )
	  dest)))))


;;; @ quoting
;;;

(defconst rfc822/linear-white-space-regexp "\\(\n?[ \t]\\)+")
(defconst rfc822/quoted-pair-regexp "\\\\.")
(defconst rfc822/non-qtext-char-list '(?\" ?\\ ?\r ?\n))
(defconst rfc822/qtext-regexp
  (concat "[^" (char-list-to-string rfc822/non-qtext-char-list) "]"))
(defconst rfc822/quoted-string-regexp
  (concat "\""
	  (regexp-*
	   (regexp-or rfc822/qtext-regexp rfc822/quoted-pair-regexp)
	   )
	  "\""))

(defun rfc822/wrap-as-quoted-string (str)
  "Wrap string STR as RFC 822 quoted-string. [tl-822.el]"
  (concat "\""
	  (mapconcat (function
		      (lambda (chr)
			(if (memq chr rfc822/non-qtext-char-list)
			    (concat "\\" (char-to-string chr))
			  (char-to-string chr)
			  )
			)) str "")
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

(defun rfc822/unfolding-string (str)
  (let ((dest ""))
    (while (string-match "\n\\s +" str)
      (setq dest (concat dest (substring str 0 (match-beginning 0)) " "))
      (setq str (substring str (match-end 0)))
      )
    (concat dest str)
    ))


;;; @ lexical analyze
;;;

(defconst rfc822/special-chars "][()<>@,;:\\<>.\"")
(defconst rfc822/space-chars " \t\n")
(defconst rfc822/non-atom-chars
  (concat rfc822/special-chars rfc822/space-chars))
(defconst rfc822/non-dtext-chars "][")
(defconst rfc822/non-ctext-chars "()")

(defun rfc822/analyze-spaces (str)
  (let ((i (string-match (concat "[^" rfc822/space-chars "]") str)))
    (if i
	(if (> i 0)
	    (cons (cons 'spaces (substring str 0 i))
		  (substring str i)
		  ))
      (if (not (string-equal str ""))
	  (cons (cons 'spaces str) "")
	))))

(defun rfc822/analyze-special (str)
  (if (and (> (length str) 0)
	   (find (elt str 0) rfc822/special-chars)
	   )
      (cons (cons 'specials (substring str 0 1))
	    (substring str 1)
	    ))
  )

(defun rfc822/analyze-atom (str)
  (let ((i (string-match (concat "[" rfc822/non-atom-chars "]") str)))
    (if i
	(if (> i 0)
	    (cons (cons 'atom (substring str 0 i))
		  (substring str i)
		  ))
      (if (not (string-equal str ""))
	  (cons (cons 'spaces str) "")
	))))

(defun rfc822/analyze-quoted-string (str)
  (let ((len (length str)))
    (if (and (> len 0)
	     (eq (elt str 0) ?\")
	     )
	(let ((i 1) chr dest)
	  (catch 'tag
	    (while (< i len)
	      (setq chr (aref str i))
	      (cond ((eq chr ?\\)
		     (setq i (1+ i))
		     (if (>= i len)
			 (throw 'tag nil)
		       )
		     (setq dest (concat dest (char-to-string (aref str i))))
		     )
		    ((eq chr ?\")
		     (throw 'tag
			    (cons (cons 'quoted-string dest)
				  (substring str (1+ i)))
			    )
		     )
		    (t
		     (setq dest (concat dest (char-to-string (aref str i))))
		     ))
	      (setq i (1+ i))
	      ))))))

(defun rfc822/analyze-domain-literal (str)
  (if (and (> (length str) 0)
	   (eq (aref str 0) ?\[)
	   )
      (let* ((i (string-match (concat "[" rfc822/non-dtext-chars "]") str 1))
	     (rest (and i (substring str i)))
	     )
	(if (and i
		 (> (length rest) 0)
		 (eq (aref rest 0) ?\])
		 )
	    (cons (cons 'domain-literal (substring str 1 i))
		  (substring rest 1)
		  )
	  ))))

(defun rfc822/analyze-comment (str)
  (if (and (> (length str) 0)
	   (eq (elt str 0) ?\()
	   )
      (let ((dest "")
	    p ret)
	(setq str (substring str 1))
	(catch 'tag
	  (while (not (string-equal str ""))
	    (setq p (string-match (concat "[" rfc822/non-ctext-chars "]") str))
	    (cond ((> p 0)
		   (setq dest (concat dest (substring str 0 p)))
		   (setq str (substring str p))
		   )
		  ((setq ret (rfc822/analyze-comment str))
		   (setq dest (concat dest "(" (cdr (car ret)) ")"))
		   (setq str (cdr ret))
		   )
		  (t (throw 'tag nil))
		  )
	    ))
	(if (and (> (length str) 0)
		 (eq (elt str 0) ?\))
		 )
	    (cons (cons 'comment dest)
		  (substring str 1)
		  )
	  ))))

(defun rfc822/lexical-analyze (str)
  (let (dest ret)
    (while (not (string-equal str ""))
      (setq ret
	    (or (rfc822/analyze-quoted-string str)
		(rfc822/analyze-domain-literal str)
		(rfc822/analyze-comment str)
		(rfc822/analyze-spaces str)
		(rfc822/analyze-special str)
		(rfc822/analyze-atom str)
		'((error) . "")
		))
      (setq dest (cons (car ret) dest))
      (setq str (cdr ret))
      )
    (nreverse dest)
    ))


;;; @ parser
;;;

(defun rfc822/ignored-token-p (token)
  (let ((type (car token)))
    (or (eq type 'spaces)(eq type 'comment))
    ))

(defun rfc822/parse-token (lal)
  (let (token itl)
    (while (and lal
		(progn
		  (setq token (car lal))
		  (rfc822/ignored-token-p token)
		  ))
      (setq lal (cdr lal))
      (setq itl (cons token itl))
      )
    (cons (nreverse (cons token itl))
	  (cdr lal))
    ))

(defun rfc822/parse-ascii-token (lal)
  (let (token itl parsed token-value)
    (while (and lal
		(setq token (car lal))
		(if (and (setq token-value (cdr token))
			 (find-charset-string token-value)
			 )
		    (setq token nil)
		  (rfc822/ignored-token-p token)
		  ))
      (setq lal (cdr lal))
      (setq itl (cons token itl))
      )
    (if (and token
	     (setq parsed (nreverse (cons token itl)))
	     )
	(cons parsed (cdr lal))
      )))

(defun rfc822/parse-token-or-comment (lal)
  (let (token itl)
    (while (and lal
		(progn
		  (setq token (car lal))
		  (eq (car token) 'spaces)
		  ))
      (setq lal (cdr lal))
      (setq itl (cons token itl))
      )
    (cons (nreverse (cons token itl))
	  (cdr lal))
    ))

(defun rfc822/parse-word (lal)
  (let ((ret (rfc822/parse-ascii-token lal)))
    (if ret
	(let ((elt (car ret))
	      (rest (cdr ret))
	      )
	  (if (or (assq 'atom elt)
		  (assq 'quoted-string elt))
	      (cons (cons 'word elt) rest)
	    )))))

(defun rfc822/parse-word-or-comment (lal)
  (let ((ret (rfc822/parse-token-or-comment lal)))
    (if ret
	(let ((elt (car ret))
	      (rest (cdr ret))
	      )
	  (cond ((or (assq 'atom elt)
		     (assq 'quoted-string elt))
		 (cons (cons 'word elt) rest)
		 )
		((assq 'comment elt)
		 (cons (cons 'comment-word elt) rest)
		 ))
	  ))))

(defun rfc822/parse-phrase (lal)
  (let (ret phrase)
    (while (setq ret (rfc822/parse-word-or-comment lal))
      (setq phrase (append phrase (cdr (car ret))))
      (setq lal (cdr ret))
      )
    (if phrase
	(cons (cons 'phrase phrase) lal)
      )))

(defun rfc822/parse-local-part (lal)
  (let ((ret (rfc822/parse-word lal)))
    (if ret
	(let ((local-part (cdr (car ret))) dot)
	  (setq lal (cdr ret))
	  (while (and (setq ret (rfc822/parse-ascii-token lal))
		      (setq dot (car ret))
		      (string-equal (cdr (assq 'specials dot)) ".")
		      (setq ret (rfc822/parse-word (cdr ret)))
		      (setq local-part
			    (append local-part dot (cdr (car ret)))
			    )
		      (setq lal (cdr ret))
		      ))
	  (cons (cons 'local-part local-part) lal)
	  ))))

(defun rfc822/parse-sub-domain (lal)
  (let ((ret (rfc822/parse-ascii-token lal)))
    (if ret
	(let ((sub-domain (car ret)))
	  (if (or (assq 'atom sub-domain)
		  (assq 'domain-literal sub-domain)
		  )
	      (cons (cons 'sub-domain sub-domain)
		    (cdr ret)
		    )
	    )))))

(defun rfc822/parse-domain (lal)
  (let ((ret (rfc822/parse-sub-domain lal)))
    (if ret
	(let ((domain (cdr (car ret))) dot)
	  (setq lal (cdr ret))
	  (while (and (setq ret (rfc822/parse-ascii-token lal))
		      (setq dot (car ret))
		      (string-equal (cdr (assq 'specials dot)) ".")
		      (setq ret (rfc822/parse-sub-domain (cdr ret)))
		      (setq domain
			    (append domain dot (cdr (car ret)))
			    )
		      (setq lal (cdr ret))
		      ))
	  (cons (cons 'domain domain) lal)
	  ))))

(defun rfc822/parse-at-domain (lal)
  (let ((ret (rfc822/parse-ascii-token lal)) at-sign)
    (if (and ret
	     (setq at-sign (car ret))
	     (string-equal (cdr (assq 'specials at-sign)) "@")
	     (setq ret (rfc822/parse-domain (cdr ret)))
	     )
	(cons (cons 'at-domain (append at-sign (cdr (car ret))))
	      (cdr ret))
      )))

(defun rfc822/parse-addr-spec (lal)
  (let ((ret (rfc822/parse-local-part lal))
	addr)
    (if (and ret
	     (prog1
		 (setq addr (cdr (car ret)))
	       (setq lal (cdr ret))
	       (and (setq ret (rfc822/parse-at-domain lal))
		    (setq addr (append addr (cdr (car ret))))
		    (setq lal (cdr ret))
		    )))
	(cons (cons 'addr-spec addr) lal)
      )))

(defun rfc822/parse-route (lal)
  (let ((ret (rfc822/parse-at-domain lal))
	route comma colon)
    (if (and ret
	     (progn
	       (setq route (cdr (car ret)))
	       (setq lal (cdr ret))
	       (while (and (setq ret (rfc822/parse-ascii-token lal))
			   (setq comma (car ret))
			   (string-equal (cdr (assq 'specials comma)) ",")
			   (setq ret (rfc822/parse-at-domain (cdr ret)))
			   )
		 (setq route (append route comma (cdr (car ret))))
		 (setq lal (cdr ret))
		 )
	       (and (setq ret (rfc822/parse-ascii-token lal))
		    (setq colon (car ret))
		    (string-equal (cdr (assq 'specials colon)) ":")
		    (setq route (append route colon))
		    )
	       ))
	(cons (cons 'route route)
	      (cdr ret)
	      )
      )))

(defun rfc822/parse-route-addr (lal)
  (let ((ret (rfc822/parse-ascii-token lal))
	< route addr-spec >)
    (if (and ret
	     (setq < (car ret))
	     (string-equal (cdr (assq 'specials <)) "<")
	     (setq lal (cdr ret))
	     (progn (and (setq ret (rfc822/parse-route lal))
			 (setq route (cdr (car ret)))
			 (setq lal (cdr ret))
			 )
		    (setq ret (rfc822/parse-addr-spec lal))
		    )
	     (setq addr-spec (cdr (car ret)))
	     (setq lal (cdr ret))
	     (setq ret (rfc822/parse-ascii-token lal))
	     (setq > (car ret))
	     (string-equal (cdr (assq 'specials >)) ">")
	     )
	(cons (cons 'route-addr (append route addr-spec))
	      (cdr ret)
	      )
      )))

(defun rfc822/parse-phrase-route-addr (lal)
  (let ((ret (rfc822/parse-phrase lal)) phrase)
    (if ret
	(progn
	  (setq phrase (cdr (car ret)))
	  (setq lal (cdr ret))
	  ))
    (if (setq ret (rfc822/parse-route-addr lal))
	(cons (list 'phrase-route-addr
		    phrase
		    (cdr (car ret)))
	      (cdr ret))
      )))

(defun rfc822/parse-mailbox (lal)
  (let ((ret (or (rfc822/parse-phrase-route-addr lal)
		 (rfc822/parse-addr-spec lal)))
	mbox comment)
    (if (and ret
	     (prog1
		 (setq mbox (car ret))
	       (setq lal (cdr ret))
	       (if (and (setq ret (rfc822/parse-token-or-comment lal))
			(setq comment (cdr (assq 'comment (car ret))))
			)
		   (setq lal (cdr ret))
		 )))
	(cons (list 'mailbox mbox comment)
	      lal)
      )))

(defun rfc822/parse-group (lal)
  (let ((ret (rfc822/parse-phrase lal))
	phrase colon comma mbox semicolon)
    (if (and ret
	     (setq phrase (cdr (car ret)))
	     (setq lal (cdr ret))
	     (setq ret (rfc822/parse-ascii-token lal))
	     (setq colon (car ret))
	     (string-equal (cdr (assq 'specials colon)) ":")
	     (setq lal (cdr ret))
	     (progn
	       (and (setq ret (rfc822/parse-mailbox lal))
		    (setq mbox (list (car ret)))
		    (setq lal (cdr ret))
		    (progn
		      (while (and (setq ret (rfc822/parse-ascii-token lal))
				  (setq comma (car ret))
				  (string-equal
				   (cdr (assq 'specials comma)) ",")
				  (setq lal (cdr ret))
				  (setq ret (rfc822/parse-mailbox lal))
				  (setq mbox (cons (car ret) mbox))
				  (setq lal (cdr ret))
				  )
			)))
	       (and (setq ret (rfc822/parse-ascii-token lal))
		    (setq semicolon (car ret))
		    (string-equal (cdr (assq 'specials semicolon)) ";")
		    )))
	(cons (list 'group phrase (nreverse mbox))
	      (cdr ret)
	      )
      )))

(defun rfc822/parse-address (lal)
  (or (rfc822/parse-group lal)
      (rfc822/parse-mailbox lal)
      ))

(defun rfc822/parse-addresses (lal)
  (let ((ret (rfc822/parse-address lal)))
    (if ret
	(let ((dest (list (car ret))))
	  (setq lal (cdr ret))
	  (while (and (setq ret (rfc822/parse-ascii-token lal))
		      (string-equal (cdr (assq 'specials (car ret))) ",")
		      (setq ret (rfc822/parse-address (cdr ret)))
		      )
	    (setq dest (cons (car ret) dest))
	    (setq lal (cdr ret))
	    )
	  (nreverse dest)
	  ))))

(defun rfc822/addr-to-string (seq)
  (mapconcat (function
	      (lambda (token)
		(if (eq (car token) 'spaces)
		    ""
		  (cdr token)
		  )))
	     seq "")
  )

(defun rfc822/address-string (address)
  (cond ((eq (car address) 'group)
	 (mapconcat (function rfc822/address-string)
		    (nth 2 address)
		    ", ")
	 )
	((eq (car address) 'mailbox)
	 (let ((addr (nth 1 address)))
	   (rfc822/addr-to-string
	    (if (eq (car addr) 'phrase-route-addr)
		(nth 2 addr)
	      (cdr addr)
	      )
	    )))))

(defun rfc822/full-name-string (address)
  (cond ((eq (car address) 'group)
	 (mapconcat (function
		     (lambda (token)
		       (cdr token)
		       ))
		    (nth 1 address) "")
	 )
	((eq (car address) 'mailbox)
	 (let ((addr (nth 1 address))
	       (comment (nth 2 address))
	       phrase)
	   (if (eq (car addr) 'phrase-route-addr)
	       (setq phrase (mapconcat (function
					(lambda (token)
					  (cdr token)
					  ))
				       (nth 1 addr) ""))
	     )
	   (or phrase comment)
	   ))))

(defun rfc822/extract-address-components (str)
  "Extract full name and canonical address from STR.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil. [tl-822.el]"
  (let* ((structure (car
                     (rfc822/parse-address
                      (rfc822/lexical-analyze str)
                      )))
         (phrase  (rfc822/full-name-string structure))
         (address (rfc822/address-string structure))
         )
    (list phrase address)
    ))


;;; @ end
;;;

(provide 'tl-822)

;;; tl-822.el ends here
