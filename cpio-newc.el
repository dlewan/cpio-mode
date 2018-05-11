;; -*- coding: utf-8 -*-
;;; cpio-newc.el --- handle portable SVR4 cpio entry header formats.
;	$Id: cpio-newc.el,v 1.3.2.7 2018/04/26 14:15:32 doug Exp $	

;; COPYRIGHT
;; 
;; Copyright © 2015, 2016, 2017, 2018 Douglas Lewan, d.lewan2000@gmail.com
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
;; Created: 2015 Jan 03
;; Version: 0.02
;; Keywords: cpio, portable newc header

;;; Commentary:

;;; Documentation:

;; Naming conventions:
;;
;; All global variables pertaining to newc-formatted cpio-archives
;; begin '*cpio-newc-...'.
;; Every FIELD in a header have corrsponding regular expressions
;; to match them named '*cpio-newc-FIELD-re*'.
;; The variable *cpio-newc-header-re* uses those regular expressions
;; to parse a newc header.
;; Every FIELD's matching substring has an index named
;; *cpio-newc-FIELD-idx*'.
;; 
;; HEREHERE The following are currently UNUSED:
;; Every FIELD has a function (cpio-newc-get-FIELD)
;; that operates on a parsed header to retrieve the value of that FIELD.
;; (It's not obvious that such functions need to be here.
;; After all, a parsed header has the same structure for each format.

;;; Code:

;;
;; Dependencies
;; 
(require 'cpio-generic)






;; 
;; Vars
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; newc header format vars
;; 

;; MAINTENANCE The following must remain in synch with *cpio-newc-header-re*.
(defvar *cpio-newc-magic-re* "070701"
  "RE to match the magic number of a newc archive.")
(setq *cpio-newc-magic-re* "070701")

(defvar *cpio-newc-ino-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_ino field in a newc header.")
(setq *cpio-newc-ino-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-mode-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_mode field in a newc header.")
(setq *cpio-newc-mode-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-uid-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_uid field in a newc header.")
(setq *cpio-newc-uid-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-gid-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_gid field in a newc header.")
(setq *cpio-newc-gid-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-nlink-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_nlink field in a newc header.")
(setq *cpio-newc-nlink-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-mtime-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_mtime field in a newc header.")
(setq *cpio-newc-mtime-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-filesize-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_filesize field in a newc header.")
(setq *cpio-newc-filesize-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-dev-maj-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_dev field in a newc header.")
(setq *cpio-newc-dev-maj-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-dev-min-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_dev field in a newc header.")
(setq *cpio-newc-dev-min-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-rdev-maj-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_rdev field in a newc header.")
(setq *cpio-newc-rdev-maj-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-rdev-min-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_rdev field in a newc header.")
(setq *cpio-newc-rdev-min-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-rdev-min-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_rdev field in a newc header.")
(setq *cpio-newc-rdev-min-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-namesize-re* "[[:xdigit:]]\\{8\\}"
  "RE to match the c_namesize field in a newc header.")
(setq *cpio-newc-namesize-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-chksum-re*  "[[:xdigit:]]\\{8\\}"
  "RE to match the CRC checksum in a newc header.")
(setq *cpio-newc-chksum-re* "[[:xdigit:]]\\{8\\}")

(defvar *cpio-newc-filename-re* "[[:print:]]+"
  "RE to match the c_filename field in a newc header.")
(setq *cpio-newc-filename-re* "[[:print:]]+")

(defvar *cpio-newc-header-re* ()
  "RE to match newc header format cpio archives.")
(setq *cpio-newc-header-re* (concat "\\(" *cpio-newc-magic-re*    "\\)"
				    "\\(" *cpio-newc-ino-re*      "\\)"
				    "\\(" *cpio-newc-mode-re*     "\\)"
				    "\\(" *cpio-newc-uid-re*      "\\)"
				    "\\(" *cpio-newc-gid-re*      "\\)"

				    "\\(" *cpio-newc-nlink-re*    "\\)"
				    "\\(" *cpio-newc-mtime-re*    "\\)"
				    "\\(" *cpio-newc-filesize-re* "\\)"
				    "\\(" *cpio-newc-dev-maj-re*  "\\)"
				    "\\(" *cpio-newc-dev-min-re*  "\\)"

				    "\\(" *cpio-newc-rdev-maj-re* "\\)"
				    "\\(" *cpio-newc-rdev-min-re* "\\)"
				    "\\(" *cpio-newc-namesize-re* "\\)"
				    "\\(" *cpio-newc-chksum-re*   "\\)"
				    "\\(" *cpio-newc-filename-re* "\\)"
				    "\0"))

(let ((i 0))
  (defvar *cpio-newc-magic-re-idx* 0	; (setq i (1+ i))
    "RE to match the magic number in a newc header.")
  (setq *cpio-newc-magic-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-ino-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the inode.")
  (setq *cpio-newc-ino-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-mode-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the mode.")
  (setq *cpio-newc-mode-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-uid-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the UID.")
  (setq *cpio-newc-uid-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-gid-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the GID.")
  (setq *cpio-newc-gid-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-nlink-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the nlink.")
  (setq *cpio-newc-nlink-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-mtime-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the mtime.")
  (setq *cpio-newc-mtime-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-filesize-re-idx* 0 ; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the filesize.")
  (setq *cpio-newc-filesize-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-dev-maj-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the dev.")
  (setq *cpio-newc-dev-maj-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-dev-min-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the dev.")
  (setq *cpio-newc-dev-min-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-rdev-maj-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the rdev.")
  (setq *cpio-newc-rdev-maj-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-rdev-min-re-idx* 0	; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the rdev.")
  (setq *cpio-newc-rdev-min-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-namesize-re-idx* 0 ; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the namesize.")
  (setq *cpio-newc-namesize-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-chksum-re-idx* 0 ; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the chksum.")
  (setq *cpio-newc-chksum-re-idx* (setq i (1+ i)))

  (defvar *cpio-newc-filename-re-idx* 0 ; (setq i (1+ i))
    "Index of the sub RE from *cpio-newc-header-re* to parse the namesize.")
  (setq *cpio-newc-filename-re-idx* (setq i (1+ i)))

)
;; 
;; EO newc header variables.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *cpio-newc-magic-re*
(defvar *cpio-newc-magic* *cpio-newc-magic-re*
  "The string that identifies an entry as a NEWC style cpio(1) entry.")
(setq *cpio-newc-magic* *cpio-newc-magic-re*)

(defvar *cpio-newc-field-width* 8
  "The width of all of the fields in a newc header.")
(setq *cpio-newc-field-width* 8)

(defvar *cpio-newc-padding-modulus* 4
  "The modulus to which some things are padded in a NEWC cpio archive.")
(setq *cpio-newc-padding-modulus* 4)

(defvar *cpio-newc-padding-char* ?\0
  "A character to be used for padding headers and entry contents
in a newc cpio archive.")
(setq *cpio-newc-padding-char* ?\0)

(defvar *cpio-newc-padding-str* "\0"
  "A single character string of the character
to be used for padding headers and entry contents
in a newc cpio archive.")
(setq *cpio-newc-padding-str* "\0")

(let ((i 0)
      (l (length *cpio-newc-magic-re*)))
  (defconst *cpio-newc-magic-field-offset* i)
  (setq *cpio-newc-magic-field-offset* i)
  (setq i (1+ i))
  (defconst *cpio-newc-ino-field-offset*      (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-ino-field-offset*          (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-mode-field-offset*     (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-mode-field-offset*         (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-uid-field-offset*      (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-uid-field-offset*          (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-gid-field-offset*      (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-gid-field-offset*          (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-nlink-field-offset*    (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-nlink-field-offset*        (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-mtime-field-offset*    (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-mtime-field-offset*        (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-filesize-field-offset* (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-filesize-field-offset*     (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-dev-maj-field-offset*  (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-dev-maj-field-offset*      (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-dev-min-field-offset*  (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-dev-min-field-offset*      (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-rdev-maj-field-offset* (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-rdev-maj-field-offset*     (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-rdev-min-field-offset* (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-rdev-min-field-offset*     (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-namesize-field-offset* (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-namesize-field-offset*     (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-chksum-field-offset*   (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-chksum-field-offset*       (+ l (* *cpio-newc-field-width* (1- i))))
  (setq i (1+ i))
  (defconst *cpio-newc-name-field-offset*     (+ l (* *cpio-newc-field-width* i)))
  (setq *cpio-newc-name-field-offset*         (+ l (* *cpio-newc-field-width* (1- i)))))

(defconst *cpio-newc-trailer "07070100000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000B00000000TRAILER!!!\0\0\0\0"
  "The TRAILER string for a newc archive.")

(defcustom *cpio-newc-blocksize* 512
  "The default block size for this cpio archive.
Taken from cpio-2.12/src/global.c."
  :type 'integer
  :group 'cpio)

(defvar *cpio-newc-magic* ()
  "The magic string for a newc formatted header string.")
(setq *cpio-newc-magic* "070701")

;; 
;; Library
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Functions for working with a cpio newc header
;; 

(defun cpio-newc-header-at-point (&optional where)
  "Return the header string at or following point WHERE.
If WHERE is not given, then use point.
CAVEATS:
1. This searches for the magic number at the begining of the header;
   if WHERE is inside the magic number, then the search will fail.
   This works best if you are (looking-at) a header.
2. This returns the pure header;
   it does not provide the filename itself."
  (unless where (setq where (point)))
  (let ((fname "cpio-newc-header-at-point")
	(found nil))
    (save-match-data
      (cond ((looking-at *cpio-newc-header-re*)
	     (match-string 0))
	    (t
	     (forward-char (length *cpio-newc-magic-re*))
	     (while (and (re-search-backward *cpio-newc-magic-re* (point-min) t)
			 (not (setq found (looking-at *cpio-newc-header-re*)))))
	     (if found 
		 (match-string 0)))))))
(setq cpio-header-at-point-func 'cpio-newc-header-at-point)

;;;;;;;;;;;;;;;;
;; 
;; Parsing a header
;; 

(defun cpio-newc-parse-header (header-string)
  "Return the internal entry header structure encoded in HEADER-STR.
The optional argument WHERE should be a buffer location
at the beginning of a known cpio newc header.
If WHERE is not given, then take point and hope.
This function does NOT get the contents."
  (let ((fname "cpio-newc-parse-header")
	(namesize)
	(filesize)
	(result))
    ;; There's an arguable level of redundancy here,
    ;; but the caller likely grabbed HEADER-STR
    ;; from the buffer and we're using the string proper.
    ;; This call establishes the match-data
    ;; that the subsequent calls will use.
    (string-match *cpio-newc-header-re* header-string)
    (setq result
	  (vector (cpio-newc-parse-ino      header-string)
		  (cpio-newc-parse-mode     header-string)
		  (cpio-newc-parse-uid      header-string)
		  (cpio-newc-parse-gid      header-string)
		  (cpio-newc-parse-nlink    header-string)
		  (cpio-newc-parse-mtime    header-string)
		  (setq filesize (cpio-newc-parse-filesize header-string))
		  (cpio-newc-parse-dev-maj  header-string)
		  (cpio-newc-parse-dev-min  header-string)
		  (cpio-newc-parse-rdev-maj header-string)
		  (cpio-newc-parse-rdev-min header-string)
		  (setq namesize (cpio-newc-parse-namesize header-string))
		  (cpio-newc-parse-chksum   header-string)
		  (cpio-newc-parse-name     header-string namesize)
		  (cpio-newc-parse-chksum   header-string)
		  (cpio-newc-header-size    header-string namesize)))
    (if (cpio-entry-name result)
	result
      nil)))

(setq cpio-parse-header-func 'cpio-newc-parse-header)

(defun cpio-newc-header-size (header-string namesize)
  "Determine the length of the header implied by the given HEADER-STRING."
  (let ((fname "cpio-newc-header-size")
	;; CAUTION: The following assumes that (string-to-number) doesn't care about leading zeroes.
	;; The namesize in the header includes the terminating NULL at the end of the name.
	(local-namesize (1- namesize))
	(total -1))
    (if (= 0 (mod (setq total (+ 1 *cpio-newc-name-field-offset* local-namesize)) 
		  *cpio-newc-padding-modulus*))
	(setq total (1+ total)))
    (round-up total *cpio-newc-padding-modulus*)))

(defun cpio-newc-parse-magic (header-string)
  "Get the magic field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-magic")
	 (this-offset *cpio-newc-magic-field-offset*)
	 (end-offset (+ this-offset (length *cpio-newc-magic-re*))))
    (substring header-string this-offset end-offset)))

(defun cpio-newc-parse-ino (header-string)
  "Get the ino field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-ino")
	 (this-offset *cpio-newc-ino-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-mode (header-string)
  "Get the mode field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-mode")
	 (this-offset *cpio-newc-mode-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset)16)))

(defun cpio-newc-parse-uid (header-string)
  "Get the uid field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-uid")
	 (this-offset *cpio-newc-uid-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-gid (header-string)
  "Get the gid field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-gid")
	 (this-offset *cpio-newc-gid-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-nlink (header-string)
  "Get the nlink field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-nlink")
	 (this-offset *cpio-newc-nlink-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-mtime (header-string)
  "Get the mtime field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-mtime")
	 (this-offset *cpio-newc-mtime-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*))
	 (time-value ()))
    (setq time-value (string-to-number (substring header-string this-offset end-offset) 16))
    (setq time-value (list (lsh (logand #xFFFF0000 time-value) -16) (logand #xFFFF)))))

(defun cpio-newc-parse-filesize (header-string)
  "Get the filesize from the HEADER-STRING."
  (let* ((fname "cpio-newc-parse-filesize")
	 (this-offset *cpio-newc-filesize-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-dev-maj (header-string)
  "Get the dev-maj field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-dev-maj")
	 (this-offset *cpio-newc-dev-maj-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-dev-min (header-string)
  "Get the dev-min field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-dev-min")
	 (this-offset *cpio-newc-dev-min-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-rdev-maj (header-string)
  "Get the rdev-maj field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-rdev-maj")
	 (this-offset *cpio-newc-rdev-maj-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-rdev-min (header-string)
  "Get the rdev-min field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-rdev-min")
	 (this-offset *cpio-newc-rdev-min-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-namesize (header-string)
  "Get the namesize field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-namesize")
	 (this-offset *cpio-newc-namesize-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-chksum (header-string)
  "Get the chksum field from HEADER-STRING."
  (let* ((fname "cpio-newc-parse-chksum")
	 (this-offset *cpio-newc-chksum-field-offset*)
	 (end-offset (+ this-offset *cpio-newc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 16)))

(defun cpio-newc-parse-name (header-string namesize)
  "Get the name field from HEADER-STRING.
N.B. When called with the correct namesize, this includes the terminating \0."
  (let* ((fname "cpio-newc-parse-name")
	 (this-offset *cpio-newc-name-field-offset*)
	 (tmp-string (substring header-string this-offset (+ this-offset namesize -1))))
    (if (string-equal tmp-string "TRAILER!!!")
	nil
      tmp-string)))

(defun cpio-newc-parse-chksum (header-string)
  "Return the checksum in the given HEADER-STRING.
For a newc header it is always nil."
  (let ((fname "cpio-newc-parse-chksum"))
    nil))

;; Is this not M-x cpio-dired-find-entry?
(defun cpio-newc-parse-contents (header-string where namesize filesize)
  "Return the contents implied by point and HEADER-STRING.
CAVEATS: See `cpio-newc-parse-magic'.
This requires the point to be at the start of HEADER-STRING in the buffer.
After all that's where the contents are, not in the header."
  (let ((fname "cpio-newc-parse-contents"))
    (buffer-substring-no-properties (+ where namesize)
				    (+ where namesize filesize))))

;;;;;;;;;;;;;;;;
;; 
;; Header accessors
;; 

;; HEREHERE Are these needed here and not up in cpio.el?

;; (defun cpio-newc-get-magic (parsed-header)
;;   "Return the magic field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-magic"))
;;     (arev parsed-header *cpio-magic-idx*)))
;; (setq cpio-get-magic-func 'cpio-newc-get-magic)

;; (defun UNUSED-cpio-newc-get-ino (parsed-header)
;;   "Return the ino field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-ino"))
;;     (aref parsed-header *cpio-ino-parsed-idx*)))
;; (setq cpio-get-ino-func 'cpio-newc-get-ino)

;; (defun UNUSED-cpio-newc-get-mode (parsed-header)
;;   "Return the mode field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-mode"))
;;     (aref parsed-header *cpio-mode-parsed-idx*)))
;; (setq cpio-get-mode-func 'cpio-newc-get-mode)

;; (defun UNUSED-cpio-newc-get-uid (parsed-header)
;;   "Return the uid field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-uid"))
;;     (aref parsed-header *cpio-uid-parsed-idx*)))
;; (setq cpio-get-uid-func 'cpio-newc-get-uid)

;; (defun UNUSED-cpio-newc-get-gid (parsed-header)
;;   "Return the gid field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-gid"))
;;     (aref parsed-header *cpio-gid-parsed-idx*)))
;; (setq cpio-get-gid-func 'cpio-newc-get-gid)

;; (defun UNUSED-cpio-newc-get-nlink (parsed-header)
;;   "Return the nlink field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-nlink"))
;;     (aref parsed-header *cpio-nlink-parsed-idx*)))
;; (setq cpio-get-nlink-func 'cpio-newc-get-nlink)

;; (defun UNUSED-cpio-newc-get-mtime (parsed-header)
;;   "Return the mtime field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-mtime"))
;;     (aref parsed-header *cpio-mtime-parsed-idx*)))
;; (setq cpio-get-mtime-func 'cpio-newc-get-mtime)

;; (defun UNUSED-cpio-newc-get-filesize (parsed-header)
;;   "Return the filesize field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-filesize"))
;;     (aref parsed-header *cpio-filesize-parsed-idx*)))
;; (setq cpio-get-filesize-func 'cpio-newc-get-filesize)

;; (defun UNUSED-cpio-newc-get-dev-maj (parsed-header)
;;   "Return the dev field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-dev-maj"))
;;     (aref parsed-header *cpio-dev-maj-parsed-idx*)))
;; (setq cpio-get-dev-maj-func 'cpio-newc-get-dev-maj)

;; (defun UNUSED-cpio-newc-get-dev-min (parsed-header)
;;   "Return the dev field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-dev-min"))
;;     (aref parsed-header *cpio-dev-min-parsed-idx*)))
;; (setq cpio-get-dev-min-func 'cpio-newc-get-dev-min)

;; (defun UNUSED-cpio-newc-get-rdev-maj (parsed-header)
;;   "Return the rdev field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-rdev-maj"))
;;     (aref parsed-header *cpio-rdev-parsed-idx*)))
;; (setq cpio-get-rdev-maj-func 'cpio-newc-get-rdev-maj)

;; (defun UNUSED-cpio-newc-get-rdev-min (parsed-header)
;;   "Return the rdev field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-rdev-min"))
;;     (aref parsed-header *cpio-rdev-parsed-idx*)))
;; (setq cpio-get-rdev-min-func 'cpio-newc-get-rdev-min)

;; (defun UNUSED-cpio-newc-get-namesize (parsed-header)
;;   "Return the namesize field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-namesize"))
;;     (aref parsed-header *cpio-namesize-parsed-idx*)))
;; (setq cpio-get-namesize-func 'cpio-newc-get-namesize)

;; (defun UNUSED-cpio-newc-get-chksum (parsed-header)
;;   "Return the chksum field from the PARSED-HEADER."
;;   (let ((fchksum "cpio-newc-get-chksum"))
;;     (aref parsed-header *cpio-chksum-parsed-idx*)))
;; (setq cpio-get-chksum-func 'cpio-newc-get-chksum)

;; (defun UNUSED-cpio-newc-get-filename (parsed-header)
;;   "Return the name field from the PARSED-HEADER."
;;   (let ((fname "cpio-newc-get-filename"))
;;     (aref parsed-header *cpio-name-parsed-idx*)))
;; (setq cpio-get-filename-func 'cpio-newc-get-filename)

;;;;;;;;;;;;;;;;
;; 
;; Header construction
;; 

(defun cpio-newc-make-header-string (attrs)
  "Make a NEWC style padded cpio header for the given ATTRibuteS.
This function does NOT include the contents."
  (let ((fname "cpio-newc-parse-header")
	(name (cpio-entry-name attrs))
	(header-string))
    (setq header-string (concat  (cpio-newc-make-magic    attrs)
				 (cpio-newc-make-ino      attrs)
				 (cpio-newc-make-mode     attrs)
				 (cpio-newc-make-uid      attrs)
				 (cpio-newc-make-gid      attrs)
				 (cpio-newc-make-nlink    attrs)
				 (cpio-newc-make-mtime    attrs)
				 (cpio-newc-make-filesize attrs)
				 (cpio-newc-make-dev-maj  attrs)
				 (cpio-newc-make-dev-min  attrs)
				 (cpio-newc-make-rdev-maj attrs)
				 (cpio-newc-make-rdev-min attrs)
				 (format "%08X" (1+ (length name)))
				 (cpio-newc-make-chksum   attrs)
				 name
				 "\0"))
    (setq header-string (pad-right header-string (round-up (length header-string) *cpio-newc-padding-modulus*) "\0"))
    ;; Check (at least during development).
    (if (string-match *cpio-newc-header-re* header-string)
	header-string
      (error "%s(): I built a bad header: [[%s]]" fname header))))
(setq cpio-make-header-string-func 'cpio-newc-make-header-string)

(defun cpio-newc-make-magic (attrs)
  "Return the NEWC magic header string"
  (let ((fname "cpio-newc-make-magic"))
    *cpio-newc-magic*))
(setq cpio-make-magic-func 'cpio-newc-make-magic)

(defun cpio-newc-make-ino (attrs)
  "Return a string value for the inode from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-ino")
	(ino (aref attrs *cpio-ino-parsed-idx*)))
    (cond ((numberp ino)
	   (format "%08X" ino))
	  ((consp ino)
	   (cond ((cddr ino)
		  (cpio-newc-BIG-inode-to-string))
		 ((cdr ino)
		  (cpio-newc-big-inode-to-string))
		 (t (error "Bad inode value: [[%s]]." ino))))
	  (t (error "Bad inode value: [[%s]]." ino)))))
(setq cpio-make-ino-func 'cpio-newc-make-ino)

(defun cpio-newc-BIG-inode-to-string (ino)
  "Convert the BIG inode format (HIGH MIDDLE . LOW) to a printable integer.
Since we're writing a NEWC CPIO header it must be < 8 digits.
N.B. On my 64 bit machine most-positive-fixnum is 2305843009213693951.
I likely won't need this, but someone might."
  ;; There's a contract here that INO is a triple of integers.
  (let ((fname "cpio-newc-BIG-inode-to-string"))
    (hex-format-triple ino)))

(defun cpio-newc-big-inode-to-string (ino)
  "Convert the big inode format (HIGH . LOW) to a printable integer.
Since we're writing a NEWC CPIO header it must be < 8 digits.
N.B. On my 64 bit machine most-positive-fixnum is 2305843009213693951.
I likely won't need this, but someone might."
  (let ((fname "cpio-newc-big-inode-to-string")
	(hex-digit-count (integer-hex-digits))
	(formatter))
    (hex-format-pair ino)))

(defun cpio-newc-make-mode (attrs)
  "Return a string value for the mode from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-mode"))
    (format "%08X" (aref attrs *cpio-mode-parsed-idx*))))
;; (setq cpio-make-mode-func 'cpio-newc-make-mode)

(defun cpio-newc-make-uid (attrs)
  "Return an integer string value for the UID from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-uid")
	(uid (aref attrs 2)))
    (cond ((numberp uid)
	   (format "%08X" uid))
	  ((string-match "\\`[[:graph:]]\\'" uid)
	   (cpio-look-up-uid uid))
	  (t (error "Bad UID: [[%s]]" uid)))))
;; (setq cpio-make-uid-function 'cpio-newc-make-uid)

(defun cpio-newc-make-gid (attrs)
  "Return an integer string value for the GID from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-gid")
	(gid (aref attrs 3)))
    (cond ((numberp gid)
	   (format "%08X" gid))
	  ((string-match "\\`[[:graph:]]\\'" gid)
	   (cpio-look-up-gid gid))
	  (t (error "Bad GID: [[%s]]" gid)))))
;; (setq cpio-make-gid-function 'cpio-newc-make-gid)

(defun cpio-newc-make-nlink (attrs)
  "Return an integer string value for the number of links from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-nlink"))
    (format "%08X" (aref attrs *cpio-nlink-parsed-idx*))))
;; (setq cpio-make-nlink-function 'cpio-newc-make-nlink)

(defun cpio-newc-make-mtime (attrs)
  "Return a string value for the mod time from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-mtime")
	(mod-time (aref attrs *cpio-mtime-parsed-idx*)))
    ;; We're only about 1/2 way through using this up it seems.
    ;; Still, time will eventually overflow a 32 bit unsigned integer.
    (format "%08X" (float-time mod-time))))
;; (setq cpio-make-mtime-function 'cpio-newc-make-mtime)

(defun cpio-newc-make-filesize (attrs)
  "Return an 8 digit hex string for the filesize attribute among the given ATTRs."
  (let ((fname "cpio-newc-make-filesize"))
    (format "%08X" (aref attrs *cpio-entry-size-parsed-idx*))))
(setq cpio-make-filesize-function 'cpio-newc-make-filesize)

(defun cpio-newc-make-dev-maj (attrs)
  "Return a string value for the WWWW from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-dev-maj")
	(dev (aref attrs 11)))
    (cond ((numberp dev)
	   (format "%08X" (logand (lsh dev -8) #xff)))
	  ;; The documenation for (file-attributes) says that this is handled
	  ;; like the inode.
	  ((consp dev)
	   (cond ((cddr dev)
		  (cpio-BIG-inode-to-string))
		 ((cdr dev)
		  (cpio-big-inode-to-string))
		 (t (error "Bad dev value: [[%s]]." ino))))
	  (t (error "Bad dev value: [[%s]]." ino)))))
;; (setq cpio-make-dev-function 'cpio-newc-make-dev)

(defun cpio-newc-make-dev-min (attrs)
  "Return a string value for the WWWW from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-dev-min")
	(dev (aref attrs 11)))
    (cond ((numberp dev)
	   (format "%08X" (logand dev #xff)))
	  ;; The documenation for (file-attributes) says that this is handled
	  ;; like the inode.
	  ;; The calculations below are, nonetheless, out of synch
	  ;; with the cpio(1GNU) code.
	  ((consp dev)
	   (cond ((cddr dev)
		  (cpio-BIG-inode-to-string))
		 ((cdr dev)
		  (cpio-big-inode-to-string))
		 (t (error "Bad dev value: [[%s]]." ino))))
	  (t (error "Bad dev value: [[%s]]." ino)))))
;; (setq cpio-make-dev-function 'cpio-newc-make-dev)

(defun cpio-newc-make-rdev-maj (attrs)
  "Return a string value for the WWWW from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-rdev-maj")
	(rdev))
    ;; MAINTENANCE Every concrete example I look at has this value for rdev-maj.
    ;; That's apparently the case for all file types except char and block special.
    ;; And, yes, I have to figure out those calculations yet.
    "00000000"))
;; (setq cpio-make-rdev-function 'cpio-newc-make-rdev)

(defun cpio-newc-make-rdev-min (attrs)
  "Return a string value for the WWWW from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-rdev-min"))
    ;; MAINTENANCE Every concrete example I look at has this value for rdev-maj.
    ;; See (cpio-newc-make-rdev-maj) for more information.
    "00000000"))
;; (setq cpio-make-rdev-function 'cpio-newc-make-rdev)

(defun cpio-newc-make-chksum (attrs)
  "Return a string value for the newc cpio entry from the file attributes ATTRS."
  (let ((fname "cpio-newc-make-chksum"))
    ;; According to the info this is only populated for crc archives.
    ;; It has always been 00000000 for my concrete newc examples.
    ;; And, indeed, it's only set in crc archives.
    ;; See copyout.c->writeout-defered-file() and nowhere else.
    "00000000"))
;; (setq cpio-make-chksum-function 'cpio-newc-make-chksum)

;; Filename is not one of ATTRS. ∴ It doesn't get a constructor here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Functions for whole entries
;; 
(defun cpio-newc-parse-header-at-point ()
  "Parse the newc cpio header that begins at point.
If there is no header there, then signal an error."
  (let ((fname "cpio-newc-parse-header-at-point"))
    (unless (looking-at-p *cpio-newc-header-re*) (error "%s(): point is not looking at a newc header."))
    (cpio-newc-parse-header (match-string-no-properties 0))))

(defun cpio-newc-goto-next-header ()
  "Move the point to the beginning of the next newc cpio header.
If point is looking-at such a header, then that is the next one
and there is no movement.
\(Thus, a caller may have to make sure that point has moved.\)
This returns the a marker for point where the header is found, if one is found.
It returns NIL otherwise.
This sets match-data for the entire header and each field."
  (let ((fname "cpio-newc-goto-next-header")
	(header-start)
	(header-string))
    (cond ((re-search-forward *cpio-newc-header-re* (point-max) t)
	   (setq header-start (goto-char (match-beginning 0)))
	   (setq header-string (match-string 0))
	   (cons (point-marker) header-string))
	  (t nil))))
(setq cpio-goto-next-header-function 'cpio-newc-goto-next-header)

(defun cpio-newc-build-catalog ()
  "Build an internal structure reflecting the contents of the newc cpio archive in the current buffer.
See the variable *cpio-catalog* for more information.
CAVEAT: This respects neither narrowing nor the point."
  (let ((fname "cpio-newc-build-catalog")
	(header-start)			;A marker.
	(header-end)
	(that-header-string)
	(header-info ())
	(parsed-header t)
	(filesize)			;A marker.
	(contents-start)
	(contents-end)			;NOT NEEDED?
	(those-contents)		;
	(catalog ()))
    (widen)
    (goto-char (point-min))
    (while (and (setq header-info (cpio-newc-goto-next-header))
		(setq header-start (car header-info))
		(setq that-header-string (cdr header-info))
		parsed-header)
      (cond ((setq parsed-header (cpio-newc-parse-header-at-point))
	     (setq filesize (cpio-entry-size parsed-header))
	     (forward-char (length that-header-string))
	     (setq header-end (point))
	     ;; A little bit of arithmetic gymnastics here
	     ;; because cpio, being written in C, starts counting at 0, but
	     ;; emacs' points start at 1.
	     (goto-char (1+ (round-up (1- header-end) *cpio-padding-modulus*)))
	     (setq contents-start (point-marker))
	     (set-marker-insertion-type contents-start *insert-after*)
	     ;; It feels like I really want a function for getting the contents.
	     ;; But it's not obvious what is simpler or appropriately more general
	     ;; than this one-liner.
	     ;; Indeed. (setq those-contents (buffer-substring-no-properties contents-start contents-end))
	     (push (cons  (cpio-entry-name parsed-header)
			  (vector
			   parsed-header
			   header-start
			   contents-start))
		   catalog)
	     (setq contents-end (+ contents-start filesize -1))
	     (goto-char contents-end))
	    (t t)))
    (nreverse catalog)))
(setq cpio-build-catalog-function 'cpio-newc-build-catalog)

(defun cpio-newc-start-of-trailer ()
  "Return the character position of the (ostensible) start of the trailer
for the current cpio archive."
  (let ((fname "cpio-newc-start-of-trailer")
	(end-of-contents 0))
    (mapc (lambda (ce)
	    (let ((attrs (cpio-entry-attrs-from-catalog-entry e)))
	      (setq end-of-contents (+ (cpio-entry-size attrs) (cpio-contents-start e)))))
	  *cpio-catalog*)
    end-of-contents))

(defun cpio-newc-end-of-archive ()
  "Calculate the location of the end of the current archive
once the TRAILER is written and padded."
  (let ((fname "cpio-newc-end-of-archive")
	(end-of-contents (cpio-newc-start-of-trailer)))
    (round-up (+ end-of-contents (length *cpio-newc-trailer*)) 512)))

(defun cpio-newc-adjust-trailer ()
  "Replace thed current trailer in the current cpio newc archive."
  (let* ((fname "cpio-newc-adjust-trailer"))
    (cpio-newc-delete-trailer)
    (cpio-newc-insert-trailer)))

(defun cpio-newc-insert-trailer ()
  "Insert a newc trailer into a cpio archive."
  (let* ((fname "cpio-newc-insert-trailer")
	 (base-trailer "07070100000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000B00000000TRAILER!!!\0\0\0\0")
	 (base-len (length base-trailer))
	 (len))
    ;; (error "%s() is not yet implemented" fname)
    ;; ...and insert the new trailer...
    (setq buffer-read-only nil)
    (insert base-trailer)
    (goto-char (point-max))
    ;; ...with padding.
    (setq len (round-up (1- (point)) *cpio-newc-blocksize*))
    (setq len (1+ (- len (point))))
    (insert (make-string len ?\0))
    (setq buffer-read-only t)))

(defun cpio-newc-delete-trailer ()
  "Delete the trailer in the current cpio newc archive."
  (let ((fname "cpio-newc-delete-trailer"))
    ;; (error "%s() is not yet implemented" fname)
    ;; First, get to the end of the last entry in the archive.
    (goto-char (point-min))
    (mapc (lambda (e)
	    (let* ((ename (car e))	;Isn't there a generic function for this?
		   (attrs (cpio-entry-attrs ename))
		   ;; Fencepost issue here.
		   (entry-end (+ (cpio-contents-start ename)
				 (cpio-entry-size attrs))))
	      (goto-char entry-end)
	      (skip-chars-forward "\0")))
	  *cpio-catalog*)
    ;; Next, delete what's left...
    (setq buffer-read-only nil)
    (delete-region (point) (point-max))
    (setq buffer-read-only t)))


;; 
;; Test and other development assistance.
;; 

(require 'cl)				;For (mapcar*)
(defun cpio-newc-present-header (header-string)
  "Parse the HEADER-STRING and present its fields nicely.
That is show their names and octal and decimal values."
  (let ((fname "cpio-newc-present-header")
	(header-contents (cpio-newc-parse-header header-string))
	(header-fields (list "magic"
			     "ino"
			     "mode"
			     "uid"
			     "gid"
			     "nlink"
			     "mtime"
			     "filesize"
			     "dev-maj"
			     "dev-min"
			     "rdev-maj"
			     "rdev-min"
			     "namesize"
			     "chksum"
			     "name")))
    (apply 'concat (mapcar* (lambda (name value)
			      (setq name name)
				    ;; (pad-right name 12 " "))
			      (format "%s\t%s\t%o\t%d\n"
				      name
				      (pad-right value 8 " ")
				      (string-to-number value 16)
				      (string-to-number value 16)))
			    header-fields header-contents))))

(defvar *locations-delay* 0.05
  "The number of seconds to wait at certain points in M-x locations.")
(setq *locations-delay* 0.05)

(defun locations ()
  "Put locations and location related data into the buffer *Locations*.
This is not done properly; it is somewhat brute force.
However, it is intended to help figure out
what the proper way to do it is."
  (interactive)
  (let ((fname "locations")
	(lbuf (get-buffer-create "*Locations*"))
	(name)
	(namesize 0)
	(soh)
	(sofn)
	(eoh)
	(filesize 0)
	(name "")
	(ct 0)
	(interval 0))
    (unless (and lbuf
		 (buffer-live-p lbuf))
      (error "Could not get buffer *Locations*."))
    (with-current-buffer lbuf (erase-buffer))
    (goto-char (point-min))
    (while (re-search-forward *cpio-newc-header-re* (point-max) t)
      (goto-char (match-beginning 0))
      (sit-for *locations-delay*)
      (setq soh (point))
      (save-match-data
	(looking-at *cpio-newc-magic*)
	(goto-char (match-end 0)))
      (forward-char (+ 8		;inode
		       8		;mode
		       8		;uid
		       8		;gid
		       8		;nlink
		       8))		;mtime

      (sit-for *locations-delay*)
      (setq filesize (string-to-number (buffer-substring-no-properties (point) (+ (point) 8))))

      (forward-char (+ 8		;filesize
		       8		;dev-maj
		       8		;dev-min
		       8		;rdev-maj
		       8))		;rdev-min
					;namesize
      (sit-for *locations-delay*)
      ;; HEREHERE
      (setq namesize (string-to-number (buffer-substring-no-properties (point) (+ (point) 8))))
      (forward-char (+ 8		;namesize
		       8))		;chksum
      (sit-for *locations-delay*)
      (setq sofn (point))
      (sit-for *locations-delay*)
      (setq name (buffer-substring-no-properties (point) (+ (point) namesize -1)))
      (sit-for *locations-delay*)
      (setq eoh (point))
      (forward-char namesize)
      (setq eon (point))
      (setq hpad (skip-chars-forward "\0"))
      (setq soc (point))
      (re-search-forward "\0\\|070701" (point-max) t)
      (goto-char (match-beginning 0))
      (sit-for *locations-delay*)
      (setq eoc (point))
      (sit-for *locations-delay*)
      (setq cpad (skip-chars-forward "\0"))

      (with-current-buffer lbuf
	(funcall 'insert-table-header-maybe ct)
	(insert (format (concat "%5s\t"		   ;Name
				"  %5d\t"	   ;Name length
				"  %5d\t"	   ;SOH
				"  %5d\t"	   ;SOFN
				
				"  %5d\t"	   ;EOH
				"  %5d\t"	   ;EON
				"  %5d\t"	   ;hpad
				"  %5d\t"	   ;File size

				"  %5d\t"	   ;SOC
				"  %5d\t"	   ;EOC
				"  %5d\t"	   ;cpad
				"\n")
			name
			namesize
			soh
			sofn
			eoh
			eon
			hpad
			filesize
			soc
			eoc
			cpad)))
	(setq ct (1+ ct)))
    (with-current-buffer lbuf
      (goto-char (point-max))
      (insert (concat "Notes: 1. Name length includes the terminating NULL.\n"
		      "       2. SOH is calculated via a search for the magic number.\n"
		      "       3. EOH and SON are equal; each calculation is via the point.\n"
		      "       4. hpad and cpad are each calculated by motion.\n"))
      (goto-char (point-min)))
    (pop-to-buffer lbuf)))

(defun insert-table-header-maybe (ct)
  "Insert a table header for a cpio entry."
  (let ((fname "insert-table-header-maybe"))
    (message "%s(): ct is [[%s]]" fname ct) (sit-for .2)
    (cond ((= 0 (mod ct 40))
	   (insert "\n")
	   (insert (concat "  Name\t"
			   "  Name\t"
			   "   SOH\t"
			   "  SOFN\t"

			   "   EOH\t"
			   "   EON\t"
			   "  hpad\t"
			   "  File\t"

			   "  SOC\t"
			   "  EOC\t"
			   " cpad"
			   "\n"))
	   (insert "\tlength\t\t\t\t\t\t  size\t\t\t\n")
	   (insert (concat
		    (make-string 8 ?=)	;name
		    (make-string 8 ?=)	;name length
		    (make-string 8 ?=)	;SOH
		    (make-string 8 ?=)	;SOFN

		    (make-string 8 ?=)	;EOH
		    (make-string 8 ?=)	;EON
		    (make-string 8 ?=)	;hpad
		    (make-string 8 ?=)	;File size

		    (make-string 8 ?=)	;SOC
 		    (make-string 8 ?=)	;EOC
 		    (make-string 8 ?=)	;cpad
 		    (make-string 8 ?=)	;????
		    "\n")))
	  (t t))))
;; 
;; The catalog
;; 



;; 
;; Commands
;; 

;; (define-derived-mode cpio-newc-mode nil "CPIO NEWC"
;;   "A mode for processing CPIO archives in the NEWC format."
;;   (goto-char (point-min))
;;   (cond ((looking-at *cpio-newc-header-re*)
;; 	 (
;; 	 )
;; 	(t nil))

(provide 'cpio-newc)
;;; cpio-newc.el ends here.
