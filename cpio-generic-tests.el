;; -*- coding: utf-8 -*-
;;; cpio-generic-tests.el --- tests of cpio-generic.el
;	$Id: cpio-generic-tests.el,v 1.1.4.4 2018/04/26 14:15:31 doug Exp $	

;; COPYRIGHT
;; 
;; Copyright © 2015, 2018 Douglas Lewan, d.lewan2000@gmail.com
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
;; Created: 2015 Dec 27
;; Version: 0.02
;; Keywords: 

;;; Commentary:

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 


;; 
;; Vars
;; 


;; 
;; Library
;; 


;; 
;; Tests
;; 

(ert-deftest cpio-generic-drwx-test
    (let (test-data (list (cons "d---------" (format "%x" (string-to-number "0040000" 8)))
			  (cons "b---------" (format "%x" (string-to-number "0060000" 8)))
			  (cons "c---------" (format "%x" (string-to-number "0020000" 8)))
			  (cons "l---------" (format "%x" (string-to-number "0120000" 8)))
			  (cons "s---------" (format "%x" (string-to-number "0140000" 8)))
			  (cons "-r--------" (format "%x" (string-to-number "0000400" 8)))
			  (cons "--w-------" (format "%x" (string-to-number "0000200" 8)))
			  (cons "---x------" (format "%x" (string-to-number "0000100" 8)))
			  (cons "----r-----" (format "%x" (string-to-number "0000040" 8)))
			  (cons "-----w----" (format "%x" (string-to-number "0000020" 8)))
			  (cons "------x---" (format "%x" (string-to-number "0000010" 8)))
			  (cons "-------r--" (format "%x" (string-to-number "0000004" 8)))
			  (cons "--------w-" (format "%x" (string-to-number "0000002" 8)))
			  (cons "---------x" (format "%x" (string-to-number "0000001" 8)))
			  (cons "---u------" (format "%x" (string-to-number "0040000" 8)))
			  (cons "------g---" (format "%x" (string-to-number "0020000" 8)))
			  (cons "---------s" (format "%x" (string-to-number "0010000" 8)))
			  (cons "drwx------" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "700" 8))))
			  (cons "drw-------" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "600" 8))))
			  (cons "dr-x------" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "500" 8))))
			  (cons "lrwx------" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "700" 8))))
			  (cons "lrw-------" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "600" 8))))
			  (cons "lr-x------" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "500" 8))))
			  (cons "-rwx------" (format "%x" (string-to-number "700" 8)))
			  (cons "-rw-------" (format "%x" (string-to-number "600" 8)))
			  (cons "-r-x------" (format "%x" (string-to-number "500" 8)))

			  (cons "d---rwx---" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "70" 8))))
			  (cons "d---rw----" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "60" 8))))
			  (cons "d---r-x---" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "50" 8))))
			  (cons "l---rwx---" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "70" 8))))
			  (cons "l---rw----" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "60" 8))))
			  (cons "l---r-x---" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "50" 8))))
			  (cons "----rwx---" (format "%x" (string-to-number "70" 8)))
			  (cons "----rw----" (format "%x" (string-to-number "60" 8)))
			  (cons "----r-x---" (format "%x" (string-to-number "50" 8)))

			  (cons "d------rwx" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "7" 8))))
			  (cons "d------rw-" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "6" 8))))
			  (cons "d------r-x" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "5" 8))))
			  (cons "l------rwx" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "7" 8))))
			  (cons "l------rw-" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "6" 8))))
			  (cons "l------r-x" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "5" 8))))
			  (cons "-------rwx" (format "%x" (string-to-number "7" 8)))
			  (cons "-------rw-" (format "%x" (string-to-number "6" 8)))
			  (cons "-------r-x" (format "%x" (string-to-number "5" 8)))

			  (cons "d---------" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "0" 8))))
			  (cons "drw-r-----" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "640" 8))))
			  (cons "drw-r--r--" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "644" 8))))
			  (cons "drw-rw----" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "660" 8))))
			  (cons "drw-rw-r--" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "664" 8))))
			  (cons "drwxr-x--x" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "751" 8))))
			  (cons "drwxrwxr-x" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "775" 8))))
			  (cons "drwxrwxrwx" (format "%x" (+ (string-to-number "004000" 8) (string-to-number "777" 8))))
			  (cons "l---------" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "0" 8))))
			  (cons "lrw-r-----" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "640" 8))))
			  (cons "lrw-r--r--" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "644" 8))))
			  (cons "lrw-rw----" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "660" 8))))
			  (cons "lrw-rw-r--" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "664" 8))))
			  (cons "lrwxr-x--x" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "751" 8))))
			  (cons "lrwxrwxr-x" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "775" 8))))
			  (cons "lrwxrwxrwx" (format "%x" (+ (string-to-number "012000" 8) (string-to-number "777" 8))))
			  (cons "----------" (format "%x" (string-to-number "0" 8)))
			  (cons "-rw-r-----" (format "%x" (string-to-number "640" 8)))
			  (cons "-rw-r--r--" (format "%x" (string-to-number "644" 8)))
			  (cons "-rw-rw----" (format "%x" (string-to-number "660" 8)))
			  (cons "-rw-rw-r--" (format "%x" (string-to-number "664" 8)))
			  (cons "-rwxr-x--x" (format "%x" (string-to-number "751" 8)))
			  (cons "-rwxrwxr-x" (format "%x" (string-to-number "775" 8)))
			  (cons "-rwxrwxrwx" (format "%x" (string-to-number "777" 8)))))
      (mapc (lambda (datum)
	      (let ((drwx (car datum))
		    (hex (cdr datum)))
		(should (string-equal (drwx-to-hex drwx) hex))))
	    test-data)))


;; 
;; Mode definition (IF APPROPRIATE)
;; 



(provide 'cpio-generic-test)
;;; cpio-generic-test.el ends here

