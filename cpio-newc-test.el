a;; -*- coding: utf-8 -*-
;;; cpio-newc-tests.el --- Functions to help test cpio-newc.el
;	$Id: cpio-newc-tests.el,v 1.1.4.5 2018/03/08 06:10:14 doug Exp $	

;; COPYRIGHT

;; Copyright © 2015, 2017 Douglas Lewan, d.lewan2000@gmail.com.
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
;; Keywords: cpio, newc, test

;; (error "You can't actually load cpio-newc-tests.el yet.")


;;; Commentary:

;;; Documentation:

;; Right now this is just a text file containing things
;; for which test code should be written.
;; It's not even proposing a test harness, just topics.

;; • DONE symbolic mode encoding
;; • symbolic mode decoding
;; • DONE (round-up)
;; • DONE (Full header length) == 0 mod 4.
;; • header construction then parsing yields the original information.
;; • (mapcar 'length (cpio-newc-parse-header-at-point))
;;   => (6 8 8 8 8 8 8 8 8 8 8 8 8 8 n)
;; • (= (length (cpio-newc-header-at-point))
;;     (apply '+ (mapcar 'length (cpio-newc-parse-header-at-point))))
;; • STARTED (cpio-discern-archive-type)

;;; Code:

;;
;; Dependencies
;; 
(require 'ert)
(require 'cpio-newc)

;; 
;; Vars
;; 

;; MAINTENANCE: The following list of fields denotes the individual fields in the headers variable.
(defvar *cpio-newc-header-field-names* ()
  "An ordered list of the field names of a newc header.")
(setq *cpio-newc-header-field-names* (list "magic number"
					   "inode"
					   "mode"
					   "uid"
					   "gid"

					   "number of links"
					   "modification time"
					   "filesize"
					   "major device"
					   "minor device"

					   "major rdev"
					   "minoe rdev"
					   "name size"
					   "checksum"
					   "filename")))

;; MAINTENANCE: See the MAINTENANCE note on *cpio-newc-header-field-names*.
(defvar headers ()
  "A list of ( HEADER . PARSED-HEADER ) pairs.
The HEADER is padded;
you should strip the trailing NULLS before using it.")
(setq headers (list (cons "070701005A0DDD000081A4000003E8000003E800000001567EA4FC000000D5000000FC0000000100000000000000000000000900000000CALENDAR
