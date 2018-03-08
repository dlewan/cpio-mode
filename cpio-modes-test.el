;; -*- coding: utf-8 -*-
;;; cpio-modes-test.el --- tests of the code in cpio-modes.el.
;	$Id: cpio-modes-test.el,v 1.1.4.3 2018/03/08 06:10:13 doug Exp $	

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

;; To run all the tests here try this:
;; emacs -Q --load cpio-modes-test.el --eval '(ert t)'

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 

(require 'ert)
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


;; 
;; Commands
;; 


;; 
;; Mode definition (IF APPROPRIATE)
;; 



(provide 'cpio-modes)
;;; cpio-modes.el ends here

