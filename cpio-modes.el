;; -*- coding: utf-8 -*-
;;; cpio-modes.el --- handle modes.
;	$Id: cpio-modes.el,v 1.1.4.3 2018/03/08 06:10:13 doug Exp $	

;; COPYRIGHT
;; 
;; Copyright © 2017, 2018 Douglas Lewan, d.lewan2000@gmail.com.
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

;; Author: Douglas Lewan (d.lewan2000@gmail.com)
;; Maintainer: -- " --
;; Created: 2017 Nov 28
;; Version: 
;; Keywords: 

;;; Commentary:

;; This file contains code for dealing with mode bits in cpio-mode.

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 


;; 
;; Vars
;;
;;
;; Mode-related bits (adapted from /usr/include/linux/stat.h).
;; 

(defconst s-ifmt   #o0170000)
(defconst s-ifsock #o0140000)
(defconst s-iflnk  #o0120000)
(defconst s-ifreg  #o0100000)
(defconst s-ifblk  #o0060000)
(defconst s-ifdir  #o0040000)
(defconst s-ifchr  #o0020000)
(defconst s-ififo  #o0010000)
(defconst s-isuid  #o0004000)
(defconst s-isgid  #o0002000)
(defconst s-isvtx  #o0001000)

(defconst s-irwxu #o00700)
(defconst s-irusr #o00400)
(defconst s-iwusr #o00200)
(defconst s-ixusr #o00100)

(defconst s-irwxg #o00070)
(defconst s-irgrp #o00040)
(defconst s-iwgrp #o00020)
(defconst s-ixgrp #o00010)

(defconst s-irwxo #o00007)
(defconst s-iroth #o00004)
(defconst s-iwoth #o00002)
(defconst s-ixoth #o00001)

(defconst UNUSED-*cpio-low-mode-bits* (logior s-irwxu s-irwxg s-irwxo s-isuid s-isgid s-isvtx)
  "A bit mask of the modes that can be set by chmod(1).")


;; 
;; Library
;; 

(defun s-islnk (m)
  (= (logand m s-ifmt) s-iflnk))
(defun s-isreg (m)
  (= (logand m s-ifmt) s-ifreg))
(defun s-isdir (m)
  (= (logand m s-ifmt) s-ifdir))
(defun s-ischr (m)
  (= (logand m s-ifmt) s-ifchr))
(defun s-isblk (m)
  (= (logand m s-ifmt) s-ifblk))
(defun s-isfifo (m)
  (= (logand m s-ifmt) s-ififo))
(defun s-issock (m)
  (= (logand m s-ifmt) s-ifsock))

(defun cpio-int-mode-to-mode-string (int-mode)
  "Convert an integer mode value to the corresponding ls -l version."
  (let ((fname "cpio-int-mode-to-mode-string")
	(file-type  (cpio-int-mode-to-file-type         int-mode))
	(user-mode  (cpio-int-mode-to-user-permissions  int-mode))
	(group-mode (cpio-int-mode-to-group-permissions int-mode))
	(other-mode (cpio-int-mode-to-other-permissions int-mode)))
    (concat file-type user-mode group-mode other-mode)))

(defvar *cpio-modes-link*    "l")
(defvar *cpio-modes-reg*     "-")
(defvar *cpio-modes-dir*     "d")
(defvar *cpio-modes-char*    "c")
(defvar *cpio-modes-block*   "b")
(defvar *cpio-modes-fifo*    "p")
(defvar *cpio-modes-sock*    "s")
(defvar *cpio-modes-unknown* "?")

(defun cpio-int-mode-to-file-type (int-mode)
  "Extract the one character string that expresses the file type from INT-MODE.
CAUTION: Some file types are not present here:
    D -- Solaris door
    M -- Cray DMF migrated file
    n -- HP-UX network special file
    P -- Solaris port.
If you have access to any of those operating systems,
please let me know."
  (let ((fname "cpio-int-mode-to-file-type"))
    (cond ((s-islnk int-mode)
	   *cpio-modes-link*)
	  ((s-isreg int-mode)
	   *cpio-modes-reg*)
	  ((s-isdir int-mode)
	   *cpio-modes-dir*)
	  ((s-ischr int-mode)
	   *cpio-modes-char*)
	  ((s-isblk int-mode)
	   *cpio-modes-block*)
	  ((s-isfifo int-mode)
	   *cpio-modes-fifo*)
	  ((s-issock int-mode)
	   *cpio-modes-sock*)
	  (t
	   *cpio-modes-unknown*))))

(defun cpio-int-mode-to-user-permissions (int-mode)
  "Extract the 3-character string expressing the user permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-permissions")
	(read-string    (cpio-int-mode-to-user-read-string    int-mode))
	(write-string   (cpio-int-mode-to-user-write-string   int-mode))
	(execute-string (cpio-int-mode-to-user-execute-string int-mode))
	)
    (concat read-string write-string execute-string)))

(defun cpio-int-mode-to-user-read-string (int-mode)
  "Extract the 1-character string expressing the user read permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-read-string"))
    (cond ((/= (logand int-mode s-irusr) 0)
	   "r")
	  (t "-"))))

(defun cpio-int-mode-to-user-write-string (int-mode)
  "Extract the 1-character string expressing the user write permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-write-string"))
    (cond ((/= (logand int-mode s-iwusr) 0)
	   "w")
	  (t "-"))))

(defun cpio-int-mode-to-user-execute-string (int-mode)
  "Extract the 1-character string expressing the user execute permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-execute-string"))
    (cond ((/= (logand int-mode s-ixusr) 0)
	   (if (/= (logand int-mode s-isuid) 0)
	       "s"
	     "x"))
	  (t 
	   (if (/= (logand int-mode s-isuid) 0)
	       "S"
	     "-")))))


(defun cpio-int-mode-to-group-permissions (int-mode)
  "Extract the 3-character string expressing the group permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-permissions")
	(read-string    (cpio-int-mode-to-group-read-string    int-mode))
	(write-string   (cpio-int-mode-to-group-write-string   int-mode))
	(execute-string (cpio-int-mode-to-group-execute-string int-mode)))
    (concat read-string write-string execute-string)))

(defun cpio-int-mode-to-group-read-string (int-mode)
  "Extract the 1-character string expressing the group read permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-read-string"))
    (cond ((/= (logand s-irgrp int-mode) 0)
	   "r")
	  (t "-"))))

(defun cpio-int-mode-to-group-write-string (int-mode)
  "Extract the 1-character string expressing the group write permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-write-string"))
    (cond ((/= (logand s-iwgrp int-mode) 0)
	   "w")
	  (t "-"))))

(defun cpio-int-mode-to-group-execute-string (int-mode)
  "Extract the 1-character string expressing the group execute permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-execute-string"))
    (cond ((/= (logand int-mode s-ixgrp) 0)
	   (if (/= (logand int-mode s-isgid) 0)
	       "s"
	     "x"))
	  (t 
	   (if (/= (logand int-mode s-isgid) 0)
	       "S"
	     "-")))))

(defun cpio-int-mode-to-other-permissions (int-mode)
  "Extract the 3-character string expressing the other permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-permissions")
	(read-string    (cpio-int-mode-to-other-read-string    int-mode))
	(write-string   (cpio-int-mode-to-other-write-string   int-mode))
	(execute-string (cpio-int-mode-to-other-execute-string int-mode)))
    (concat read-string write-string execute-string)))

(defun cpio-int-mode-to-other-read-string (int-mode)
  "Extract the 1-character string expressing the other read permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-read-string"))
    (cond ((/= (logand s-iroth int-mode) 0)
	   "r")
	  (t "-"))))

(defun cpio-int-mode-to-other-write-string (int-mode)
  "Extract the 1-character string expressing the other write permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-write-string"))
    (cond ((/= (logand s-iwoth int-mode) 0)
	   "w")
	  (t "-"))))

(defun cpio-int-mode-to-other-execute-string (int-mode)
  "Extract the 1-character string expressing the other execute permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-execute-string"))
    (cond ((/= (logand s-ixoth int-mode) 0)
	   (if (/= (logand s-isvtx int-mode) 0)
	       "t"
	     "x"))
	  (t 
	   (if (/= (logand s-isvtx int-mode) 0)
	       "T"
	     "-")))))

(defun cpio-mode-string-to-int-mode (mode-string)
  "Convert an ls -l style mode string to its corresponding integer."
  (let ((fname "cpio-mode-string-to-int-mode"))
    (error "%s(): is not implemented yet." fname)))



(defun UNUSED-cpio-low-mode-bits (bits)
  ;; HEREHERE This is no longer needed.
  ;; ∃ (chmod) ∈ emacs.n
  ;; Look for its use.
  "Return the low mode bits in BITS.
These are the bits that can be set by chmod(1)."
  (let ((fname "cpio-low-mode-bits"))
    (logand bits *cpio-low-mode-bits*)))


;; 
;; Commands
;; 


;; 
;; Mode definition (IF APPROPRIATE)
;; 



(provide 'cpio-modes)
;;; cpio-modes ends here
