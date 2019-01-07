;;; cpio-generic-tests.el --- tests of cpio-generic.el. -*- coding: utf-8 -*-

;; COPYRIGHT
;; 
;; Copyright © 2019 Free Software Foundation, Inc.
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
;; Author: Douglas Lewan <d.lewan2000@gmail.com>
;; Maintainer: Douglas Lewan <d.lewan2000@gmail.com>
;; Created: 2015 Dec 27
;; Version: 0.15β
;; Keywords: 

;;; Commentary:

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;




;; 
;; Vars
;; 


;; 
;; Library
;; 


;; 
;; Tests
;; 



;; 
;; Run tests.
;; 

(unless noninteractive

  (with-current-buffer *cab-info-buffer* (erase-buffer))

  (ert "^cpio-generic-drwx-test$")

  (pop-to-buffer *cab-info-buffer*))


(provide 'cpio-generic-test)
;;; cpio-generic-test.el ends here

