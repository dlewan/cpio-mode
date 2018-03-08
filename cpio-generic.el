;; -*- coding: utf-8 -*-
;;; cpio-generic.el --- generically useful functions created in support of CPIO mode.
;	$Id: cpio-generic.el,v 1.1.4.4.2.1 2018/03/08 06:22:09 doug Exp $	

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
;; Version: 0.01
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
(defvar *integer-size* nil)
(setq *integer-size* nil)

(defvar *integer-hex-digits* nil)


(defvar *debugger-re* "^\\s-*(message \"%s(): \\([[:digit:]]+\\)\" fname)$"
  "RE to match a debugger created by M-x insert-debugger.")
(setq *debugger-re* "^\\s-*(message \"%s(): \\([[:digit:]]+\\)\" fname)")

;; 
;; Bit definitions from sys/bits.h
;; CAVEAT: According to the info on file attributes in the info for libc
;; you can't depend on the bit values being portable to other OSes.
;; Is there a reasonable way to autoconfiscate this?
;; 
;; /* File types.  */
;; #define	__S_IFDIR	0040000	/* Directory.  */
(defvar *cpio-directory-bits* (lsh #o4 12))
;; #define	__S_IFCHR	0020000	/* Character device.  */
(defvar *cpio-char-device-bits* (lsh #o2 12))
;; #define	__S_IFBLK	0060000	/* Block device.  */
(defvar *cpio-blk-device-bits* (lsh #o6 12))
;; #define	__S_IFREG	0100000	/* Regular file.  */
(defvar *cpio-regular-file-bits* (lsh #o1 15))
;; #define	__S_IFIFO	0010000	/* FIFO.  */
(defvar *cpio-fifo-bits* (lsh #o1 12))
;; #define	__S_IFLNK	0120000	/* Symbolic link.  */
(defvar *cpio-symlink-bits* (lsh #o12 12))
;; #define	__S_IFSOCK	0140000	/* Socket.  */
(defvar *cpio-socket-bits* (lsh #o14 12))
;; 
;; MAINTENANCE The ls(1) info page mentions other file types:
;; C - Contiguous data file
;; D - Door (Solaris only?)
;; M - Migrated file (Cray)
;; ? - Some other type.
;; 
;; /* Protection bits.  */
;; 
;; #define	__S_ISUID	04000	/* Set user ID on execution.  */
(defvar *cpio-s-isuid-bits* #o40)
;; #define	__S_ISGID	02000	/* Set group ID on execution.  */
(defvar *cpio-s-isgid-bits* #o20)
;; #define	__S_ISVTX	01000	/* Save swapped text after use (sticky).  */
(defvar *cpio-s-ivtx-bits* #o10)
;; #define	__S_IREAD	0400	/* Read by owner.  */
(defvar *cpio-s-iread-bits* #o4)
;; #define	__S_IWRITE	0200	/* Write by owner.  */
(defvar *cpio-s-iwrite-bits* #o2)
;; #define	__S_IEXEC	0100	/* Execute by owner.  */
(defvar *cpio-s-iexec-bits* #o1)
;; 

(defvar *insert-after* nil
  "Value used to define that a marker has type 'insert after'.")
(defvar *insert-before* t
  "Value used to define that a marker has type 'insert before'.")


;; 
;; Library
;; 
(defun integer-size ()
  "Return the number of bits in an [unsigned] integer."
  (let ((fname "integer-size")
	(b 1)
	(bit-ct 0))
    (cond ((null *integer-size*)
	   (while (/= 0 (logand b most-positive-fixnum))
	     (setq bit-ct (1+ bit-ct))
	     (setq b (lsh b 1)))
	   (setq *integer-size* (1+ bit-ct)))
	  (t t))
    *integer-size*))

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

(defun lsh-pair (pair n)
  "Bit shift the given PAIR, (high . low), left by N bits.
This returns the resulting pair or integer
depending on whether the high component is non-zero.
PAIR is a cons of two integers."
  ;; The concept of integer in emacs is, of course, very soft.
  ;; It likely varies from installation to installation.
  (let ((fname "lsh-pair")
	(mask 0)
	(cross-bits)
	(high (car pair))
	(low (cdr pair)))
    (if (< n 0)
	(setq mask (low-bits-mask n))
      (setq mask (high-bits-mask n)))
    (cond ((< n 0)
	   (setq cross-bits (logand high mask))
	   (setq high (lsh high n))
	   (setq low (lsh low n))
	   (setq low (logior low (lsh cross-bits (- (integer-size) n)))))
	  (t
	   (setq cross-bits (logand low mask))
	   (setq high (lsh high n))
	   (setq low (lsh low n))
	   (setq high (logior high cross-bits))))
    (if (/= 0 high)
	(cons high low)
      low)))

(defun lsh-with-carry (bits n)
  "Shift the given BITS left by N using a pair or a triple as necessary."
  ;; HEREHERE This is an obvious part of the lsh-* library here
  ;; but probably not necessary for the current project.
  (let ((fname "lsh-with-carry")
	low middle high)
    (error "%s is not yet implemented." fname)))

(defun lsh-triplet ()
  "DO THAT and update this docstring."
  (let ((fname "lsh-triplet"))
  (error "%s is not yet implemented." fname)))

(defun low-bits-mask (n)
  "Return a mask appropriate for picking up the right N bits of an integer.
If N is zero, then an empty mask is returned.
The value is a bit mask."
  ;; See (lsh-pair) for a sample use.
  (let ((fname "low-bits-mask")
	(i 0)
	(mask 0))
    (while (< i n)
      (setq mask (+ (lsh mask 1) 1))
      (setq i (1+ i)))
    mask))
    
(defun high-bits-mask (n)
  "Return a mask appropriate for picking up the left N bits of an integer.
If N is zero, then an empty mask is returned.
The value is a bit mask."
  ;; See (lsh-pair) for a sample use.
  (let ((fname "high-bits-mask")
	(i 0)
	(mask 0))
    (while (< i n)
      (setq mask (+ (lsh mask -1) most-negative-fixnum))
      (setq i (1+ i)))
    mask))
(defun hex-format-pair (pair)
  "Return a hex formatted representation of PAIR."
  (let ((fname "hex-format-pair")
	(hex-digit-count (integer-hex-digits))
	(formatter))
    (setq formatter (format "%%0%dx" hex-digit-count))
    (setq formatter (concat formatter formatter))
    (format formatter (car pair) (cdr pair))))

(defun hex-format-triple (triple)
  "Return a hex formatted representation of TRIPLE."
  (let ((fname "hex-format-triple")
	(hex-digit-count (integer-hex-digits))
	(formatter))
    (setq formatter (format "%%0%dx" hex-digit-count))
    (setq formatter (concat formatter formatter formatter))
    (format formatter (car triple) (cadr triple) (cddr triple))))


(defun drwx-to-hex (mode-string)
  "DO THAT and update this docstring."
  (let ((fname "drwx-to-hex")
	(user-offset 6)
	(group-offset 3)
	(other-offset 0)
	(user-string (substring mode-string 1 4))
	(user-value 0)
	(group-string (substring mode-string 4 7))
	(group-value 0)
	(other-string (substring mode-string 7 10))
	(other-value 0)
	(file-type-string (substring mode-string 0 1))
	(file-type-value 0)
	(value))
    (unless (string-match "\\`[-bcdlps]\\(?:[-r][-w][-xXst]\\)\\{3\\}\\'" 
			  mode-string)
      (error "%s bad mode string: [[%s]]" fname mode-string))
    (setq file-type-value (drwx-to-file-type file-type-string))
    (setq user-value (lsh (rwx-to-bits user-string) user-offset))
    (if (string-match "..[Ss]" user-string)
	(setq user-value (logior user-value (lsh *cpio-s-isuid-bits* user-offset))))
    (setq group-value (lsh (rwx-to-bits group-string) group-offset))
    (if (string-match "..[Ss]" group-string)
	(setq group-value (logior group-value (lsh *cpio-s-isgid-bits* group-offset))))
    (setq other-value (lsh (rwx-to-bits other-string) other-offset))
    (if (string-match "..t" other-string)
	(setq other-value (logior other-value (lsh *cpio-s-ivtx-bits* user-offset))))
    (setq value (logior file-type-value user-value group-value other-value))
    (format "%08X" value)))

(defun drwx-to-file-type (mode-string)
  "Convert the given mode-string to the bits specifying its file type."
  ;; -bcdlps
  (let ((fname "drwx-to-file-type"))
    (cond ((string-match "\\`-" mode-string)
	   *cpio-regular-file-bits*)
	  ((string-match "\\`b" mode-string)
	   *cpio-blk-device-bits*)
	  ((string-match "\\`c" mode-string)
	   *cpio-char-device-bits*)
	  ((string-match "\\`d" mode-string)
	   *cpio-directory-bits*)
	  ((string-match "\\`l" mode-string)
	   *cpio-symlink-bits*)
	  ((string-match "\\`p" mode-string)
	   *cpio-fifo-bits*)
	  ((string-match "\\`s" mode-string)
	   *cpio-socket-bits*)
	  (t
	   (error "%s(): bad string for file type [[%s]]" fname mode-string)))))

(defun rwx-to-bits (mode-string)
  "Convert a rwx type of mode string to corresponding bits.
rwx = 7 and rw- = 6, for example."
  (let ((fname "rwx-to-bits")
	(ret 0))
    (unless (string-match "\\`[-r][-w][-xXst]\\'" mode-string)
      (error "%s bad mode string: [[%s]]" fname mode-string))
    (if (string-match "r.." mode-string)
	(setq ret (logior ret *cpio-s-iread-bits*)))
    (if (string-match ".w." mode-string)
	(setq ret (logior ret *cpio-s-iwrite-bits*)))
    (if (string-match "..[xst]" mode-string)
	(setq ret (logior ret *cpio-s-iexec-bits*)))
    ;; Only the caller can know if this is user or group.
    ;; He must deal with set U/GID bits.
    ret))

(defun round-up (number modulus)
  "Round NUMBER up to the next multiple of MODULUS.
CAVEAT: If NUMBER is negative, then the result may be surprising."
  (let ((fname "round-up"))
    (unless (and (integerp number) (integerp modulus))
      (error "%s() takes integer arguments." fname))
    (* modulus (/ (+ number modulus -1) modulus))))

(defun pad-right (string width char)
  "Pad STRING on the right with CHAR until it is WIDTH characters wide.
CHAR is typically a character or a single character string, but may be any string."
  (let ((fname "pad-right"))
    (if (characterp char) (setq char (char-to-string char)))
    (while (< (length string) width)
      (setq string (concat string char)))
    string))

;; MAINTENANCE We need a portable version of (cpio-look-up-uid).
(defun UNUSED-cpio-look-up-uid (user-name)
  "Look up the UID for the given USER-NAME and return the UID a an integer string.
If the given user does not exist, then return FFFFFFFF."
  (let ((fname "cpio-look-up-uid"))
    (with-temp-buffer 
      ;; MAINTENANCE The following is not portable to, e.g. Windows.
      ;; It might not be portable to OSX.
      (insert-file "/etc/passwd")
      (goto-char (point-min))
      (if (re-search-forward (concat "^" user-name ":[[:graph:]]+:\\([[:digit:]]+\\):") (point-max) t)
	  (format "%08X" (string-to-number (match-string 1)))
	"FFFFFFFF"))))

;; MAINTENANCE We need a portable (cpio-look-up-gid).
(defun UNUSED-cpio-look-up-gid (group-name)
  "Look up the GID for the given GROUP-NAME and return the GID a an integer string.
If the given user does not exist, then return 99 999 999."
  (let ((fname "cpio-look-up-gid"))
    (with-temp-buffer 
      ;; MAINTENANCE The following is not portable to, e.g. Windows.
      ;; It might not be portable to OSX.
      (insert-file "/etc/group")
      (goto-char (point-min))
      (if (re-search-forward (concat "^" user-name ":[[:graph:]]+:\\([[:digit:]]+\\):") (point-max) t)
	  (format "%08X" (string-to-number (match-string 1)))
	"FFFFFFFF"))))

(defun strip-right (re string &optional multiples)
  "Strip the given RE from the right end of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "strip-right")
	(inner-re (if multiples
		      (concat "\\(" re "\\)+\\'")
		    (concat re "\\'")))
	(result string))
    (if (string-match inner-re string)
	(setq result (substring string 0 (match-beginning 0))))
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
    (if (string-match inner-re string)
	(setq result (substring string (match-end 0))))
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

(defun cpio-point ()
  "Return (point) as if it were 0-based and not 1-based.
The intent here is to make calculating padding and locations easier."
  ;; Would this be better as a macro?
  (let ((fname "cpio-point"))
    (1- (point))))

(defun cpio-goto-char (where)
  "Move point to WHERE, where WHERE is a 0-based point."
  (let ((fname "cpio-goto-char"))
    (if (< where 0)
	(signal 'wrong-type-argument (list (format "%d" where))))
    (goto-char (1+ where))))

(defun cpio-point-min ()
  "Return the minimum point given a 0-based point."
  (let ((fname "cpio-point-min"))
    (1- (point-min))))

(defun cpio-point-max ()
  "Return the maximum point given a 0-based point."
  (let ((fname "cpio-point-max"))
    (1- (point-max))))


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


;; 
;; Mode definition (IF APPROPRIATE)
;; 



(provide 'cpio-generic)
;;; cpio-generic.el ends here
