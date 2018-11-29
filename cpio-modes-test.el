;; -*- coding: utf-8 -*-
;;; cpio-modes-test.el --- tests of the code in cpio-modes.el.
;	$Id: cpio-modes-test.el,v 1.5 2018/11/29 01:57:15 doug Exp $	

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

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 

(eval-when-compile
  (require 'ert))
(load (concat default-directory "cpio-modes.el"))


;; 
;; Vars
;; 


;; 
;; Library
;; 

(ert-deftest cm-test-all-bits ()
  "Test a random mix of file types and mode bits."
  (should (string-equal "srwxrwxrwx"
			(cpio-int-mode-to-mode-string (logior s-ifsock #o777))))
  (should (string-equal "lrwxr-xr-x"
			(cpio-int-mode-to-mode-string (logior s-iflnk #o755))))
  (should (string-equal "-rwxr--r--"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o744))))
  (should (string-equal "br--r-----"
			(cpio-int-mode-to-mode-string (logior s-ifblk #o440))))
  (should (string-equal "d------r-x"
			(cpio-int-mode-to-mode-string (logior s-ifdir #o005))))
  (should (string-equal "cr-xr-xr-x"
			(cpio-int-mode-to-mode-string (logior s-ifchr #o555))))
  (should (string-equal "pr-sr-xr-x"
			(cpio-int-mode-to-mode-string (logior s-ififo s-isuid #o555))))
  (should (string-equal "srwsr-x--x"
			(cpio-int-mode-to-mode-string (logior s-ifsock s-isuid #o751))))
  (should (string-equal "lr-S------"
			(cpio-int-mode-to-mode-string (logior s-iflnk s-isuid #o400))))
  (should (string-equal "-rwxr-sr-x"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-isgid #o755))))
  (should (string-equal "brw-r-Sr--"
			(cpio-int-mode-to-mode-string (logior s-ifblk s-isgid #o644))))
  (should (string-equal "drwxrwxr-t"
			(cpio-int-mode-to-mode-string (logior s-ifdir s-isvtx #o775))))
  (should (string-equal "crw-r--r-T"
			(cpio-int-mode-to-mode-string (logior s-ifchr s-isvtx #o644)))))

(ert-deftest cm-test-mode-bits-5 ()
  "Test the sticky bit."
  (should (string-equal "---------t" (cpio-int-mode-to-mode-string 
				      (logior s-ifreg s-isvtx s-ixoth))))
  (should (string-equal "---------T" (cpio-int-mode-to-mode-string
				      (logior s-ifreg s-isvtx))))
  (should (string-equal "---------x" (cpio-int-mode-to-mode-string
				      (logior s-ifreg s-ixoth)))))

(ert-deftest cm-test-mode-bits-4 ()
  "Test the set gid bit."
  (should (string-equal "------s---" (cpio-int-mode-to-mode-string 
				      (logior s-ifreg s-isgid s-ixgrp))))
  (should (string-equal "------S---" (cpio-int-mode-to-mode-string
				      (logior s-ifreg s-isgid))))
  (should (string-equal "------x---" (cpio-int-mode-to-mode-string
				      (logior s-ifreg s-ixgrp)))))

(ert-deftest cm-test-mode-bits-3 ()
  "Test the set uid bit."
  (should (string-equal "---s------" (cpio-int-mode-to-mode-string 
				      (logior s-ifreg s-isuid s-ixusr))))
  (should (string-equal "---S------" (cpio-int-mode-to-mode-string
				      (logior s-ifreg s-isuid))))
  (should (string-equal "---x------" (cpio-int-mode-to-mode-string
				      (logior s-ifreg s-ixusr)))))

(ert-deftest cm-test-mode-bits-2 ()
  "Test random mode bits."
  (should (string-equal "-rw-rw-r--"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o664))))
  (should (string-equal "-rw-r--r--"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o644))))
  (should (string-equal "-rw-r-----"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o640))))
  (should (string-equal "-rwxr-x--x"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o751))))
  (should (string-equal "-rwxr-x---"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o750))))
  (should (string-equal "-r-xr-xr--"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o554))))
  (should (string-equal "-------rwx"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o7))))
  (should (string-equal "-r--r-xrw-"
			(cpio-int-mode-to-mode-string (logior s-ifreg #o456)))))

(ert-deftest cm-test-mode-bits-1 ()
  "Test rwx modes."
  (should (string-equal "-rwx------"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-irwxu))))
  (should (string-equal "----rwx---"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-irwxg))))
  (should (string-equal "-------rwx"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-irwxo)))))

(ert-deftest cm-test-mode-bits-0 ()
  "Test user/group/other bits one at a time."
  (should (string-equal "-r--------" 
			(cpio-int-mode-to-mode-string (logior s-ifreg s-irusr))))
  (should (string-equal "--w-------"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-iwusr))))
  (should (string-equal "---x------"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-ixusr))))
  (should (string-equal "----r-----"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-irgrp))))
  (should (string-equal "-----w----"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-iwgrp))))
  (should (string-equal "------x---"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-ixgrp))))
  (should (string-equal "-------r--"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-iroth))))
  (should (string-equal "--------w-"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-iwoth))))
  (should (string-equal "---------x"
			(cpio-int-mode-to-mode-string (logior s-ifreg s-ixoth)))))

(ert-deftest cm-test-file-type-from-int-2 ()
  "Test file types in a mix of file modes."
  (should (string-equal "s" (cpio-int-mode-to-file-type
			     (logior s-ifsock s-irwxu s-irwxg s-irwxo))))
  (should (string-equal "l" (cpio-int-mode-to-file-type
			     (logior s-iflnk s-irusr s-iwusr s-irusr s-irgrp s-iwgrp s-iroth))))
  (should (string-equal "-" (cpio-int-mode-to-file-type
			     (logior s-ifreg s-irwxu s-ixgrp s-ixoth))))
  (should (string-equal "b" (cpio-int-mode-to-file-type
			     (logior s-ifblk s-irwxu s-irwxg s-ixoth))))
  (should (string-equal "d" (cpio-int-mode-to-file-type
			     (logior s-ifdir s-irwxu s-irgrp s-ixgrp))))
  (should (string-equal "c" (cpio-int-mode-to-file-type
			     (logior s-ifchr s-irusr s-irgrp s-iroth))))
  (should (string-equal "p" (cpio-int-mode-to-file-type
			     (logior s-ififo s-isvtx s-irusr s-iwusr))))
  (should (string-equal "?" (cpio-int-mode-to-file-type
			     (logior s-isvtx s-irwxu s-irwxg s-irwxo)))))

(ert-deftest cm-test-file-type-from-int-1 ()
  "Test simple unknown file types."
  (mapc (lambda (m)
	  (should (string-equal (cpio-int-mode-to-file-type m) "?")))
	(list s-isuid
	      s-isgid
	      s-isvtx
	      s-irwxu
	      s-irusr
	      s-iwusr
	      s-ixusr
	      s-irwxg
	      s-irgrp
	      s-iwgrp
	      s-ixgrp
	      s-irwxo
	      s-iroth
	      s-iwoth
	      s-ixoth)))

(ert-deftest cm-test-file-type-from-int-0 ()
  "Test simple file types."
  (should (string-equal (cpio-int-mode-to-file-type s-ifsock) "s"))
  (should (string-equal (cpio-int-mode-to-file-type s-ififo)  "p"))
  (should (string-equal (cpio-int-mode-to-file-type s-ifblk)  "b"))
  (should (string-equal (cpio-int-mode-to-file-type s-ifchr)  "c"))
  (should (string-equal (cpio-int-mode-to-file-type s-ifdir)  "d"))
  (should (string-equal (cpio-int-mode-to-file-type s-ifreg)  "-"))
  (should (string-equal (cpio-int-mode-to-file-type s-iflnk)  "l")))

(ert-deftest cm-string-to-int-mode ()
  "Test (cpio-modestring-to-int-mode)."
  (let ((test-data (list (cons "d---------" #o0040000)
			 (cons "b---------" #o0060000)
			 (cons "c---------" #o0020000)
			 (cons "l---------" #o0120000)
			 (cons "s---------" #o0140000)

			 (cons "-r--------" #o0100400)
			 (cons "--w-------" #o0100200)
			 (cons "---x------" #o0100100)
			 (cons "----r-----" #o0100040)
			 (cons "-----w----" #o0100020)
			 (cons "------x---" #o0100010)
			 (cons "-------r--" #o0100004)
			 (cons "--------w-" #o0100002)
			 (cons "---------x" #o0100001)

		         (cons "---s------" #o0104100)
			 (cons "---S------" #o0104000)
			 (cons "------s---" #o0102010)
			 (cons "------S---" #o0102000)
			 (cons "---------t" #o0101001)
			 (cons "---------T" #o0101000)

			 (cons "drwx------" (+ #o0040000 #o700))
			 (cons "drw-------" (+ #o0040000 #o600))
			 (cons "dr-x------" (+ #o0040000 #o500))
			 (cons "lrwx------" (+ #o0120000 #o700))
			 (cons "lrw-------" (+ #o0120000 #o600))
			 (cons "lr-x------" (+ #o0120000 #o500))
			 (cons "-rwx------" (+ #o0100000 #o700))
			 (cons "-rw-------" (+ #o0100000 #o600))
			 (cons "-r-x------" (+ #o0100000 #o500))
			 
			 (cons "d---rwx---" (+ #o0040000 #o070))
			 (cons "d---rw----" (+ #o0040000 #o060))
			 (cons "d---r-x---" (+ #o0040000 #o050))
			 (cons "l---rwx---" (+ #o0120000 #o070))
			 (cons "l---rw----" (+ #o0120000 #o060))
			 (cons "l---r-x---" (+ #o0120000 #o050))
			 (cons "----rwx---" (+ #o0100000 #o070))
			 (cons "----rw----" (+ #o0100000 #o060))
			 (cons "----r-x---" (+ #o0100000 #o050))
			 
			 (cons "d------rwx" (+ #o0040000 #o007))
			 (cons "d------rw-" (+ #o0040000 #o006))
			 (cons "d------r-x" (+ #o0040000 #o005))
			 (cons "l------rwx" (+ #o0120000 #o007))
			 (cons "l------rw-" (+ #o0120000 #o006))
			 (cons "l------r-x" (+ #o0120000 #o005))
			 (cons "-------rwx" (+ #o0100000 #o007))
			 (cons "-------rw-" (+ #o0100000 #o006))
			 (cons "-------r-x" (+ #o0100000 #o005))
	 
			 (cons "d---------" (+ #o0040000 #o000))
			 (cons "drw-r-----" (+ #o0040000 #o640))
			 (cons "drw-r--r--" (+ #o0040000 #o644))
			 (cons "drw-rw----" (+ #o0040000 #o660))
			 (cons "drw-rw-r--" (+ #o0040000 #o664))
			 (cons "drwxr-x--x" (+ #o0040000 #o751))
			 (cons "drwxrwxr-x" (+ #o0040000 #o775))
			 (cons "drwxrwxrwx" (+ #o0040000 #o777))

			 (cons "l---------" (+ #o0120000 #o000))
			 (cons "lrw-r-----" (+ #o0120000 #o640))
			 (cons "lrw-r--r--" (+ #o0120000 #o644))
			 (cons "lrw-rw----" (+ #o0120000 #o660))
			 (cons "lrw-rw-r--" (+ #o0120000 #o664))
			 (cons "lrwxr-x--x" (+ #o0120000 #o751))
			 (cons "lrwxrwxr-x" (+ #o0120000 #o775))
			 (cons "lrwxrwxrwx" (+ #o0120000 #o777))

			 (cons "----------" (+ #o0100000 #o000))
			 (cons "-rw-r-----" (+ #o0100000 #o640))
			 (cons "-rw-r--r--" (+ #o0100000 #o644))
			 (cons "-rw-rw----" (+ #o0100000 #o660))
			 (cons "-rw-rw-r--" (+ #o0100000 #o664))
			 (cons "-rwxr-x--x" (+ #o0100000 #o751))
			 (cons "-rwxrwxr-x" (+ #o0100000 #o775))
			 (cons "-rwxrwxrwx" (+ #o0100000 #o777)))))

    (mapc (lambda (datum)
	    (let ((drwx (car datum))
		  (octal (cdr datum)))
	      (should (progn (message "Expecting [[%s]] --> [[%07o]]." drwx octal)
			     (= (cpio-mode-string-to-int-mode drwx) octal)))))
	  test-data)))




(unless noninteractive

  (with-current-buffer *cab-info-buffer* (erase-buffer))

  (ert "cm-")

  (pop-to-buffer *cab-info-buffer*))


(provide 'cpio-modes)
;;; cpio-modes.el ends here

