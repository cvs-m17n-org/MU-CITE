;;; latex-math-symbol.el --- LaTeX math symbol decoder

;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/7/1
;; Version:
;;    $Id: latex-math-symbol.el,v 1.2 1996/09/02 16:03:43 morioka Exp $
;; Keywords: LaTeX, math, mule

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

;; - How to install
;;	bytecompile this file and copy it to the apropriate directory.
;; - How to use
;;	If you use tm, please put following to your ~/.emacs:
;;	  (autoload 'latex-math-decode-buffer "latex-math-symbol" nil t)
;;	  (add-hook 'mime-viewer/plain-text-preview-hook
;;		    'latex-math-decode-buffer)
;;   Of course, it may be available for other hooks to filter messages.

;;; Code:

(defvar latex-math-symbol-table-alist
  '(("\\pi"		. "π")
    
    ("\\{"		. "｛")("\\}"		. "｝")
    
    ("\\cdot"		. "・")
    ("\\times"		. "×")
    ("\\cap"		. "∩")("\\cup"		. "∪")
    
    ("\\leq"		. "≤")("\\geq"		. "≥")
    ("\\le"		. "≤")("\\ge"		. "≥")
    ("\\subseteq"	. "⊆")("\\supseteq"	. "⊇")
    ("\\subset"		. "⊂")("\\supset"	. "⊃")
    ("\\in"		. "∈")("\\ni"		. "∋")
    ("\\mid"		. "｜")
    ("\\neq"		. "≠")("\\ne"		. "≠")
    
    ("\\forall"		. "∀")
    
    ("\\leftarrow"	. "←")("\\rightarrow"	. "→")
    ("\\gets"		. "←")("\\to"		. "→")
    
    ("^1"		. ",A9")
    ("^2"		. ",A2")
    ("^3"		. ",A3")
    ))

(defun latex-math-decode-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((rest latex-math-symbol-table-alist)
	  cell)
      (while rest
	(setq cell (car rest))
	(goto-char beg)
	(while (search-forward (car cell) nil t)
	  (replace-match (cdr cell))
	  )
	(setq rest (cdr rest))
	))))

(defun latex-math-decode-buffer ()
  (interactive)
  (latex-math-decode-region (point-min)(point-max))
  )


;;; @ end
;;;

(provide 'latex-math-symbol)

;;; latex-math-symbol.el ends here
