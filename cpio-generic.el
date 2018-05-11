;; -*- coding: utf-8 -*-
;;; cpio-generic.el --- generically useful functions created in support of CPIO mode.
;	$Id: cpio-generic.el,v 1.1.4.6 2018/05/11 20:13:13 doug Exp $	

;; COPYRIGHT
;; 
;; Copyright © 2015, 2018 Douglas Lewan, d.lewan2000@gmail.com.
;; All rights reserved.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 

;; Author: Douglas Lewan (d.lewan2000@gmail.com)
;; Maintainer: -- " --
;; Created: 2015 Apr 23
;; Version: 0.02
;; Keywords: generically useful emacs lisp functions.

;;; Commentary:

;;
;; This file contains useful generic functions,
;; commands for managing debuggers
;; and other temporarily useful hacks
;; to help with the development of cpio-mode.
;;
;; A quick glance through it suggests
;; that it has a lot of functional overlap with cpio-modes.el.
;; 

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 


;; 
;; Vars
;; 

(defvar *integer-hex-digits* nil)

(defvar *debugger-re* "^\\s-*(message \"%s(): \\([[:digit:]]+\\)\" fname)$"
  "RE to match a debugger created by M-x insert-debugger.")
(setq *debugger-re* "^\\s-*(message \"%s(): \\([[:digit:]]+\\)\" fname)")

(defvar *insert-after* nil
  "Value used to define that a marker has type 'insert after'.")
(defvar *insert-before* t
  "Value used to define that a marker has type 'insert before'.")


;; 
;; Library
;; 

(defun integer-hex-digits ()
  "Calculate the number of hex digits that are required to represent any integer."
  (let ((fname "integer-hex-digits")
	(an-integer most-negative-fixnum)
	(hex-digit-ct 0))
    (unless *integer-hex-digits*
	(while (/= 0 an-integer)
	  (setq an-integer (lsh an-integer -4))
	  (setq hex-digit-ct (1+ hex-digit-ct)))
	(setq *integer-hex-digits* hex-digit-ct)))
  *integer-hex-digits*)

(defun round-up (number modulus)
  "Round NUMBER up to the next multiple of MODULUS.
If number ≡ 0 (modulus), then the NUMBER is already rounded up,
so NUMBER is returned.
CAVEAT: If NUMBER is negative, then the result may be surprising."
  (let ((fname "round-up"))
    (unless (and (integerp number) (integerp modulus))
      (error "%s() takes integer arguments." fname))
    (if (= 0 (mod number modulus))
	number
      (* modulus (/ (+ number modulus -1) modulus)))))

(defun pad-right (string width char)
  "Pad STRING on the right with CHAR until it is WIDTH characters wide.
CHAR is typically a character or a single character string, but may be any string."
  (let ((fname "pad-right"))
    (if (characterp char) (setq char (char-to-string char)))
    (while (< (length string) width)
      (setq string (concat string char)))
    string))

(defun strip-right (re string &optional multiples)
  "Strip the given RE from the right end of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "strip-right")
	(inner-re (if multiples
		      (concat "\\(" re "\\)+\\'")
		    (concat re "\\'")))
	(result string))
    (save-match-data
      (if (string-match inner-re string)
	  (setq result (substring string 0 (match-beginning 0)))))
    result))

(defun strip-left (re string &optional multiples)
  "Strip the given RE from the left end of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "strip-left")
	(inner-re (if multiples
		      (concat "\\`+\\(" re "\\)")
		    (concat "\\`" re)))
		  
	(result string))
    (save-match-data
      (if (string-match inner-re string)
	  (setq result (substring string (match-end 0)))))
    result))

(defun strip (re string &optional multiples)
  "Remove the given RE from both ends of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "strip")
	(result))
    (strip-left re (strip-right re string multiples) multiples)))

(defun cpio-padded (string modulus pad-char)
  "Pad the given STRING."
  (let* ((fname "cpio-padded")
	 (string-length (length string))
	 (desired-length (round-up string-length modulus)))
    (pad-right string desired-length pad-char)))

(defun cpio-uid-for-owner (owner)
  "Return the uid (an integer) for the given OWNER (a string) if it exists.
If it doesn't exist, then return NIL.
If OWNER is a sequence of digits, then return OWNER as the GID.

CAVEAT: This deletes any buffer holding /etc/passwd."
  (let ((fname "cpio-uid-for-owner")
	(passwd-buffer (find-file-noselect "/etc/passwd"))
	(uid nil))
    (if (string-match-p "\\`[[:digit:]]+\\'" owner)
	(setq uid owner)
      (with-current-buffer passwd-buffer
	(goto-char (point-min))
	(save-match-data
	  (catch 'found-it
	    (while (< (point) (point-max))
	      (cond ((looking-at (concat owner ":[[:graph:]]+:\\([[:digit:]]+\\):[[:digit:]]+:"))
		     (setq uid (match-string-no-properties 1))
		     (throw 'found-it uid))
		    (t nil))
	      (forward-line))))))
    (kill-buffer passwd-buffer)
    (string-to-number uid)))

(defun cpio-gid-for-group (group)
  "Return the GID (an integer) for the given GROUP (a string) if it exists.
If it doesn't exist, then return NIL.
If GROUP is a sequence of digits, then return GROUP as the GID.

CAVEAT: This deletes any buffer holding /etc/group."
  (let ((fname "cpio-gid-for-group")
	(group-buffer (find-file-noselect "/etc/group"))
	(gid nil))
    (cond ((null group)
	   nil)
	  ((stringp group)
	   (if (string-match-p "\\`[[:digit:]]+\\'" group)
	       (setq gid group)
	     (with-current-buffer group-buffer
	       (goto-char (point-min))
	       (save-match-data
		 (catch 'found-it
		   (while (< (point) (point-max))
		     (cond ((looking-at (concat group ":[[:graph:]]+:\\([[:digit:]]+\\):"))
			    (setq gid (match-string-no-properties 1))
			    (throw 'found-it gid))
			   (t nil))
		     (forward-line))))))
	   (kill-buffer group-buffer)
	   (string-to-number gid))
	  (t nil))))


;; 
;; Commands
;; 

(defun insert-debugger ()
  "Insert a new debugger statement above the line containing point."
  (interactive)
  (let ((fname "insert-debugger"))
    (beginning-of-line)
    (open-line 1)
    (insert (format "(message \"%%s(): %d\" fname)" (count-lines (point-min) (point))))
    (indent-according-to-mode)))
(local-set-key "\M-\C-i" 'insert-debugger)

(defun update-debuggers ()
  "Update the line numbers in all the debuggers created by M-x insert-debugger."
  (interactive)
  (let ((fname "update-debuggers"))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward *debugger-re* (point-max) t)
	  (replace-match (format "%d" (count-lines (point-min) (point)))
			 nil nil nil 1))))
    (save-buffer)))
(local-set-key "\M-\C-u" 'update-debuggers)

(defun remove-debugger ()
  "Remove the next debugger.
Return T if one was found
and NIL otherwise.
This function respects narrowing."
  (interactive)
  (let ((fname "remove-debugger"))
    (cond ((re-search-forward *debugger-re* (point-max) t)
	   (delete-region (match-beginning 0) (match-end 0))
	   t)
	  (t nil))))

(defun remove-some-debuggers (arg)
  "Remove the next ARG debuggers.
Return non-NIL if any were found and deleted.
Return NIL if none were found.
This function respects narrowing."
  (interactive "p")
  (let ((fname "remove-some-debuggers")
	(ct 0))
    (while (and (< 0 arg) (remove-debugger))
      (setq ct (1+ ct))
      (setq arg (1- arg)))
    ct))

(defun remove-all-debuggers ()
  "Remove all debuggers created by (insert-debuggers).
This function respects narrowing."
  (interactive)
  (let ((fname "remove-all-debuggers"))
    (while (remove-debugger))))

;;
;; Hacks
;; 
(defun aaa ()
  "Create a general cpio-mode function set to the next cpio-newc function.
Well, that's the intent, but, really, it's a hack."
  (interactive)
  (let ((fname "aaa")
	(cpio-newc-function-name)
	(cpio-function-definition)
	(start -1)
	(end -1)
	(defun-end -1))
    
    (cond ((re-search-forward " \\(cpio-newc\\(-[-[:alnum:]]+\\)\\)" (point-max))
	   (setq cpio-newc-function-name (match-string-no-properties 1))
	   (setq cpio-function-definition 
		 (format "(setq cpio%s-function %s)\n" (match-string-no-properties 2)
			 cpio-newc-function-name))
	   (end-of-defun)
	   (insert cpio-function-definition))
	  (t nil))))

(defun bbb-newc (header-string)
  "Return a crudely parsed newc header from the given HEADER-STRING."
  (let* ((fname "bbb-newc")
	 (lengths (list 6 8 8 8 8  8 8 8 8 8  8 8 8 8 8))
	 (stops (let ((i 0)
		      (j 0)
		      (n 0))
		  (mapcar (lambda (l)
			    (prog1
				n
			      (setq n (+ n (nth i lengths)))
			      (setq i (1+ i))))
			  lengths)))
	 (i 0)
	 (j 1))
    (setq header-string (strip-right "\0" header-string t))
    (mapcar (lambda (s)
	      (prog1 (substring header-string (nth i stops) (nth j stops))
		(setq i j)
		(setq j (1+ j))))
	    stops)))



(provide 'cpio-generic)
;;; cpio-generic.el ends here
