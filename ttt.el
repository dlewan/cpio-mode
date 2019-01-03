;;; ttt.el --- Run lots of tests against cpio-mode. -*- coding: utf-8 -*-

;; COPYRIGHT

;; Copyright © 2017, 2018, 2019 Douglas Lewan, d.lewan2000@gmail.com.
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
;; Created: 2018 Nov 27
;; Version: 0.13β
;; Keywords: files

;;; Commentary:

;;
;; Eval this buffer to run lots of tests against cpio-mode.
;;

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 


;; 
;; Vars
;; 
(defvar ttt-buffers)
(setq ttt-buffers (list "cab-test.el" "cpio-dired-bin-test.el" "cpio-dired-crc-test.el" "cpio-dired-odc-test.el" "cpio-dired-test.el"))


;; 
;; Library
;; 

(defun randomize-list (ltr)
  "Randomize a list using Knuth's algorithm."
  (mapcar (lambda (le)
	    (cdr le))
	  (sort (mapcar (lambda (le)
			  (cons (random) le))
			ltr)
		(lambda (l r)
		  (cond ((< (car l) (car r))
			 t)
			(t
			 nil))))))


;; 
;; Commands
;; 


;;
;; Do something.
;;

(setq ttt-buffers (randomize-list (append ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers)))

(mapc 'find-file-noselect ttt-buffers)

;; (mapc 'eval-buffer b)
(let ((ct 0))
  (mapc (lambda (b)
	  (eval-buffer b)
	  (with-current-buffer "*ert*"
	    (rename-buffer (format "*ert*<%d>" ct)))
	  (setq ct (1+ ct))
	  (sit-for 2.0))
	ttt-buffers))


(provide 'ttt)
;;; ttt.el ends here

