;; -*- coding: utf-8 -*-
;;; cpio-dired-test.el --- Tests of cpio-dired-mode.
;	$Id: cpio-dired-test.el,v 1.10 2018/11/19 21:25:38 doug Exp $	

;; COPYRIGHT

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
;; Created: 2018 Mar 23
;; Version: 0.02
;; Keywords: cpio-mode, cpio-dired-mode, automated test

;;; Commentary:

;; This file defines and runs tests of commands in cpio-dired-mode.
;; The tests are admittedly to a certain degree rosy scenario tests.
;; In particular, there's no error case verification.
;; You are, of course, free to add such testing.

;; Most tests run a few variants.
;; For example, (1) operate on this entry,
;; (2) operate on the next N entries,
;; (3) operate on entries matching a regular expression.

;; Since there are three objects of interest:
;; (1) the archive itself,
;; (2) the catalog, and
;; (3) the dired-style buffer,
;; all three tend to be checked after an operation.

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 

;; HEREHERE Remove the load of test code.
(load (concat default-directory "test-generic.el"))

(eval-when-compile
  (if (file-exists-p (concat default-directory "cpio.elc"))
      (load (concat default-directory "cpio.elc")))
  (load (concat default-directory "cpio.el")))

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(defvar cpio-archive-buffer)
(defvar cpio-dired-buffer)
(defvar run-dir)
(defvar cpio-dired-del-marker)
(defvar cpio-dired-keep-marker-copy-str)
(defvar cpio-dired-keep-marker-rename)
(declare-function cpio-catalog "cpio.el")
(declare-function cpio-contents-buffer-name "cpio.el")
(declare-function cpio-dired-add-entry "cpio-dired.el")
(declare-function cpio-dired-buffer-name "cpio-dired.el")
(declare-function cpio-dired-change-marks "cpio-dired.el")
(declare-function cpio-dired-clean-directory "cpio-dired.el")
(declare-function cpio-dired-copy-entry-name-as-kill "cpio-dired.el")
(declare-function cpio-dired-create-directory "cpio-dired.el")
(declare-function cpio-dired-diff "cpio-dired.el")
(declare-function cpio-dired-display-entry "cpio-dired.el")
(declare-function cpio-dired-do-async-shell-command "cpio-dired.el")
(declare-function cpio-dired-do-chgrp "cpio-dired.el")
(declare-function cpio-dired-do-chmod "cpio-dired.el")
(declare-function cpio-dired-do-chown "cpio-dired.el")
(declare-function cpio-dired-do-compress "cpio-dired.el")
(declare-function cpio-dired-do-copy "cpio-dired.el")
(declare-function cpio-dired-do-copy-regexp "cpio-dired.el")
(declare-function cpio-dired-do-delete "cpio-dired.el")
(declare-function cpio-dired-do-flagged-delete "cpio-dired.el")
(declare-function cpio-dired-do-hardlink "cpio-dired.el")
(declare-function cpio-dired-do-hardlink-regexp "cpio-dired.el")
(declare-function cpio-dired-do-isearch "cpio-dired.el")
(declare-function cpio-dired-do-isearch-regexp "cpio-dired.el")
(declare-function cpio-dired-do-kill-lines "cpio-dired.el")
(declare-function cpio-dired-do-print "cpio-dired.el")
(declare-function cpio-dired-do-query-replace-regexp "cpio-dired.el")
(declare-function cpio-dired-do-redisplay "cpio-dired.el")
(declare-function cpio-dired-do-rename "cpio-dired.el")
(declare-function cpio-dired-do-rename-regexp "cpio-dired.el")
(declare-function cpio-dired-do-search "cpio-dired.el")
(declare-function cpio-dired-do-symlink "cpio-dired.el")
(declare-function cpio-dired-do-symlink-regexp "cpio-dired.el")
(declare-function cpio-dired-do-touch "cpio-dired.el")
(declare-function cpio-dired-downcase "cpio-dired.el")
(declare-function cpio-dired-extract-all "cpio-dired.el")
(declare-function cpio-dired-extract-entries "cpio-dired.el")
(declare-function cpio-dired-find-alternate-entry "cpio-dired.el")
(declare-function cpio-dired-find-entry-other-window "cpio-dired.el")
(declare-function cpio-dired-flag-auto-save-entries "cpio-dired.el")
(declare-function cpio-dired-flag-backup-entries "cpio-dired.el")
(declare-function cpio-dired-flag-entries-regexp "cpio-dired.el")
(declare-function cpio-dired-flag-entry-deletion "cpio-dired.el")
(declare-function cpio-dired-flag-garbage-entries "cpio-dired.el")
(declare-function cpio-dired-get-entry-name "cpio-dired.el")
(declare-function cpio-dired-goto-entry "cpio-dired.el")
(declare-function cpio-dired-hide-all "cpio-dired.el")
(declare-function cpio-dired-hide-details-mode "cpio-dired.el")
(declare-function cpio-dired-hide-subdir "cpio-dired.el")
(declare-function cpio-dired-isearch-entry-names "cpio-dired.el")
(declare-function cpio-dired-isearch-entry-names-regexp "cpio-dired.el")
(declare-function cpio-dired-kill "cpio-dired.el")
(declare-function cpio-dired-mark "cpio-dired.el")
(declare-function cpio-dired-mark-directories "cpio-dired.el")
(declare-function cpio-dired-mark-entries-containing-regexp "cpio-dired.el")
(declare-function cpio-dired-mark-entries-regexp "cpio-dired.el")
(declare-function cpio-dired-mark-executables "cpio-dired.el")
(declare-function cpio-dired-mark-subdir-entries "cpio-dired.el")
(declare-function cpio-dired-mark-symlinks "cpio-dired.el")
(declare-function cpio-dired-mark-this-entry "cpio-dired.el")
(declare-function cpio-dired-mouse-find-entry-other-window "cpio-dired.el")
(declare-function cpio-dired-move-to-first-entry "cpio-dired.el")
(declare-function cpio-dired-next-dirline "cpio-dired.el")
(declare-function cpio-dired-next-line "cpio-dired.el")
(declare-function cpio-dired-next-marked-entry "cpio-dired.el")
(declare-function cpio-dired-prev-marked-entry "cpio-dired.el")
(declare-function cpio-dired-previous-line "cpio-dired.el")
(declare-function cpio-dired-save-archive "cpio-dired.el")
(declare-function cpio-dired-show-entry-type "cpio-dired.el")
(declare-function cpio-dired-sort-toggle-or-edit "cpio-dired.el")
(declare-function cpio-dired-toggle-marks "cpio-dired.el")
(declare-function cpio-dired-toggle-read-only "cpio-dired.el")
(declare-function cpio-dired-undo "cpio-dired.el")
(declare-function cpio-dired-unmark "cpio-dired.el")
(declare-function cpio-dired-unmark-all-entries "cpio-dired.el")
(declare-function cpio-dired-unmark-all-marks "cpio-dired.el")
(declare-function cpio-dired-unmark-backward "cpio-dired.el")
(declare-function cpio-dired-up-directory "cpio-dired.el")
(declare-function cpio-dired-upcase "cpio-dired.el")
(declare-function cpio-dired-view-archive "cpio-dired.el")
(declare-function cpio-dired-view-entry "cpio-dired.el")
(declare-function cpio-epa-dired-do-decrypt "cpio-dired.el")
(declare-function cpio-epa-dired-do-encrypt "cpio-dired.el")
(declare-function cpio-epa-dired-do-sign "cpio-dired.el")
(declare-function cpio-epa-dired-do-verify "cpio-dired.el")
(declare-function cpio-image-dired-delete-tag "cpio-dired.el")
(declare-function cpio-image-dired-dired-comment-entries "cpio-dired.el")
(declare-function cpio-image-dired-dired-display-external "cpio-dired.el")
(declare-function cpio-image-dired-dired-display-image "cpio-dired.el")
(declare-function cpio-image-dired-dired-edit-comment-and-tags "cpio-dired.el")
(declare-function cpio-image-dired-dired-toggle-marked-thumbs "cpio-dired.el")
(declare-function cpio-image-dired-display-thumb "cpio-dired.el")
(declare-function cpio-image-dired-display-thumbs "cpio-dired.el")
(declare-function cpio-image-dired-display-thumbs-append "cpio-dired.el")
(declare-function cpio-image-dired-jump-thumbnail-buffer "cpio-dired.el")
(declare-function cpio-image-dired-mark-tagged-entries "cpio-dired.el")
(declare-function cpio-image-dired-tag-entries "cpio-dired.el")
(declare-function cpio-mode "cpio.el")
(declare-function cpio-view-dired-style-buffer "cpio-dired.el")
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;


;; 
;; Vars
;; 

(defvar *cdmt-newc-small-archive* "test_data/alphabet/alphabet_small.newc.cpio"
  "A small archive used for testing.")
(setq *cdmt-newc-small-archive* "test_data/alphabet/alphabet_small.newc.cpio")
(defvar *cdmt-newc-large-archive* "test_data/alphabet/alphabet.newc.cpio"
  "A large archive used for testing.")
(setq *cdmt-newc-large-archive* "test_data/alphabet/alphabet.newc.cpio")

(setq *cdmt-small-archive* *cdmt-newc-small-archive*)
(setq *cdmt-large-archive* *cdmt-newc-large-archive*)
(setq *cdmt-archive-format* "newc")

(defvar *cdmt-newc-untouched-small-archive* "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
"
  "The contents of the untouched small archive.")
(setq *cdmt-newc-untouched-small-archive* "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
")

(defvar *cdmt-newc-untouched-small-dired-buffer* "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
"
  "The contents of an untouched archive's dired-style buffer.")
(setq *cdmt-newc-untouched-small-dired-buffer* "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
")

(defvar *cdmt-newc-untouched-small-catalog* "((\"a\" .
\\s-+[[235538648 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+4 253 0 0 0 2 0 \"a\"«
\\s-+#<marker at 1 in alphabet_small.newc.cpio> #<marker at 113 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aa\" .
\\s-+[[235538674 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+5 253 0 0 0 3 0 \"aa\"«
\\s-+#<marker at 117 in alphabet_small.newc.cpio> #<marker at 233 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaa\" .
\\s-+[[235538688 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+6 253 0 0 0 4 0 \"aaa\"«
\\s-+#<marker at 241 in alphabet_small.newc.cpio> #<marker at 357 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaaa\" .
\\s-+[[235538691 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+7 253 0 0 0 5 0 \"aaaa\"«
\\s-+#<marker at 365 in alphabet_small.newc.cpio> #<marker at 481 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaaaa\" .
\\s-+[[235538692 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+8 253 0 0 0 6 0 \"aaaaa\"«
\\s-+#<marker at 489 in alphabet_small.newc.cpio> #<marker at 605 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaaaa.d\" .
\\s-+[[235538695 16877 1000 1000 2
\\s-+(23268 65535)
\\s-+0 253 0 0 0 8 0 \"aaaaa.d\"«
\\s-+#<marker at 613 in alphabet_small.newc.cpio> #<marker at 733 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"b\" .
\\s-+[[235538693 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+4 253 0 0 0 2 0 \"b\"«
\\s-+#<marker at 733 in alphabet_small.newc.cpio> #<marker at 845 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bb\" .
\\s-+[[235538694 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+5 253 0 0 0 3 0 \"bb\"«
\\s-+#<marker at 849 in alphabet_small.newc.cpio> #<marker at 965 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbb\" .
\\s-+[[235538696 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+6 253 0 0 0 4 0 \"bbb\"«
\\s-+#<marker at 973 in alphabet_small.newc.cpio> #<marker at 1089 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbbb\" .
\\s-+[[235538697 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+7 253 0 0 0 5 0 \"bbbb\"«
\\s-+#<marker at 1097 in alphabet_small.newc.cpio> #<marker at 1213 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbbbb\" .
\\s-+[[235538698 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+8 253 0 0 0 6 0 \"bbbbb\"«
\\s-+#<marker at 1221 in alphabet_small.newc.cpio> #<marker at 1337 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbbbb.d\" .
\\s-+[[235538701 16877 1000 1000 2
\\s-+(23268 65535)
\\s-+0 253 0 0 0 8 0 \"bbbbb.d\"«
\\s-+#<marker at 1345 in alphabet_small.newc.cpio> #<marker at 1465 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"c\" .
\\s-+[[235538699 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+4 253 0 0 0 2 0 \"c\"«
\\s-+#<marker at 1465 in alphabet_small.newc.cpio> #<marker at 1577 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"cc\" .
\\s-+[[235538700 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+5 253 0 0 0 3 0 \"cc\"«
\\s-+#<marker at 1581 in alphabet_small.newc.cpio> #<marker at 1697 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"ccc\" .
\\s-+[[235538702 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+6 253 0 0 0 4 0 \"ccc\"«
\\s-+#<marker at 1705 in alphabet_small.newc.cpio> #<marker at 1821 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"cccc\" .
\\s-+[[235538703 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+7 253 0 0 0 5 0 \"cccc\"«
\\s-+#<marker at 1829 in alphabet_small.newc.cpio> #<marker at 1945 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"ccccc\" .
\\s-+[[235538704 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+8 253 0 0 0 6 0 \"ccccc\"«
\\s-+#<marker at 1953 in alphabet_small.newc.cpio> #<marker at 2069 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"ccccc.d\" .
\\s-+[[235538707 16877 1000 1000 2
\\s-+(23268 65535)
\\s-+0 253 0 0 0 8 0 \"ccccc.d\"«
\\s-+#<marker at 2077 in alphabet_small.newc.cpio> #<marker at 2197 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«))
"
  "An string representing an untouched catalog.")

(setq *cdmt-newc-untouched-small-catalog* "((\"a\" .
\\s-+[[235538648 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+4 253 0 0 0 2 0 \"a\"«
\\s-+#<marker at 1 in alphabet_small.newc.cpio> #<marker at 113 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aa\" .
\\s-+[[235538674 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+5 253 0 0 0 3 0 \"aa\"«
\\s-+#<marker at 117 in alphabet_small.newc.cpio> #<marker at 233 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaa\" .
\\s-+[[235538688 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+6 253 0 0 0 4 0 \"aaa\"«
\\s-+#<marker at 241 in alphabet_small.newc.cpio> #<marker at 357 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaaa\" .
\\s-+[[235538691 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+7 253 0 0 0 5 0 \"aaaa\"«
\\s-+#<marker at 365 in alphabet_small.newc.cpio> #<marker at 481 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaaaa\" .
\\s-+[[235538692 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+8 253 0 0 0 6 0 \"aaaaa\"«
\\s-+#<marker at 489 in alphabet_small.newc.cpio> #<marker at 605 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"aaaaa.d\" .
\\s-+[[235538695 16877 1000 1000 2
\\s-+(23268 65535)
\\s-+0 253 0 0 0 8 0 \"aaaaa.d\"«
\\s-+#<marker at 613 in alphabet_small.newc.cpio> #<marker at 733 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"b\" .
\\s-+[[235538693 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+4 253 0 0 0 2 0 \"b\"«
\\s-+#<marker at 733 in alphabet_small.newc.cpio> #<marker at 845 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bb\" .
\\s-+[[235538694 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+5 253 0 0 0 3 0 \"bb\"«
\\s-+#<marker at 849 in alphabet_small.newc.cpio> #<marker at 965 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbb\" .
\\s-+[[235538696 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+6 253 0 0 0 4 0 \"bbb\"«
\\s-+#<marker at 973 in alphabet_small.newc.cpio> #<marker at 1089 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbbb\" .
\\s-+[[235538697 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+7 253 0 0 0 5 0 \"bbbb\"«
\\s-+#<marker at 1097 in alphabet_small.newc.cpio> #<marker at 1213 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbbbb\" .
\\s-+[[235538698 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+8 253 0 0 0 6 0 \"bbbbb\"«
\\s-+#<marker at 1221 in alphabet_small.newc.cpio> #<marker at 1337 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"bbbbb.d\" .
\\s-+[[235538701 16877 1000 1000 2
\\s-+(23268 65535)
\\s-+0 253 0 0 0 8 0 \"bbbbb.d\"«
\\s-+#<marker at 1345 in alphabet_small.newc.cpio> #<marker at 1465 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"c\" .
\\s-+[[235538699 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+4 253 0 0 0 2 0 \"c\"«
\\s-+#<marker at 1465 in alphabet_small.newc.cpio> #<marker at 1577 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"cc\" .
\\s-+[[235538700 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+5 253 0 0 0 3 0 \"cc\"«
\\s-+#<marker at 1581 in alphabet_small.newc.cpio> #<marker at 1697 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"ccc\" .
\\s-+[[235538702 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+6 253 0 0 0 4 0 \"ccc\"«
\\s-+#<marker at 1705 in alphabet_small.newc.cpio> #<marker at 1821 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"cccc\" .
\\s-+[[235538703 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+7 253 0 0 0 5 0 \"cccc\"«
\\s-+#<marker at 1829 in alphabet_small.newc.cpio> #<marker at 1945 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"ccccc\" .
\\s-+[[235538704 33188 1000 1000 1
\\s-+(23281 65535)
\\s-+8 253 0 0 0 6 0 \"ccccc\"«
\\s-+#<marker at 1953 in alphabet_small.newc.cpio> #<marker at 2069 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (\"ccccc.d\" .
\\s-+[[235538707 16877 1000 1000 2
\\s-+(23268 65535)
\\s-+0 253 0 0 0 8 0 \"ccccc.d\"«
\\s-+#<marker at 2077 in alphabet_small.newc.cpio> #<marker at 2197 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«))
")

(defvar *cdmt-newc-untouched-large-archive-buffer* "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
d	(( filename ))

d

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
dd	(( filename ))
\\0\\0\\0
dd

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ddd	(( filename ))
\\0\\0
ddd

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
dddd	(( filename ))
\\0
dddd

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ddddd	(( filename ))

ddddd

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ddddd.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
e	(( filename ))

e

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ee	(( filename ))
\\0\\0\\0
ee

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
eee	(( filename ))
\\0\\0
eee

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
eeee	(( filename ))
\\0
eeee

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
eeeee	(( filename ))

eeeee

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
eeeee.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
f	(( filename ))

f

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ff	(( filename ))
\\0\\0\\0
ff

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
fff	(( filename ))
\\0\\0
fff

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
ffff	(( filename ))
\\0
ffff

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
fffff	(( filename ))

fffff

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
fffff.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
g	(( filename ))

g

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
gg	(( filename ))
\\0\\0\\0
gg

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ggg	(( filename ))
\\0\\0
ggg

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
gggg	(( filename ))
\\0
gggg

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ggggg	(( filename ))

ggggg

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ggggg.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
h	(( filename ))

h

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
hh	(( filename ))
\\0\\0\\0
hh

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
hhh	(( filename ))
\\0\\0
hhh

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
hhhh	(( filename ))
\\0
hhhh

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
hhhhh	(( filename ))

hhhhh

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
hhhhh.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
i	(( filename ))

i

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ii	(( filename ))
\\0\\0\\0
ii

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
iii	(( filename ))
\\0\\0
iii

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
iiii	(( filename ))
\\0
iiii

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
iiiii	(( filename ))

iiiii

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
iiiii.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
j	(( filename ))

j

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
jj	(( filename ))
\\0\\0\\0
jj

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
jjj	(( filename ))
\\0\\0
jjj

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
jjjj	(( filename ))
\\0
jjjj

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
jjjjj	(( filename ))

jjjjj

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
jjjjj.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
k	(( filename ))

k

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
kk	(( filename ))
\\0\\0\\0
kk

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
kkk	(( filename ))
\\0\\0
kkk

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
kkkk	(( filename ))
\\0
kkkk

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
kkkkk	(( filename ))

kkkkk

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
kkkkk.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
l	(( filename ))

l

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ll	(( filename ))
\\0\\0\\0
ll

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
lll	(( filename ))
\\0\\0
lll

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
llll	(( filename ))
\\0
llll

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
lllll	(( filename ))

lllll

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
lllll.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
m	(( filename ))

m

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
mm	(( filename ))
\\0\\0\\0
mm

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
mmm	(( filename ))
\\0\\0
mmm

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
mmmm	(( filename ))
\\0
mmmm

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
mmmmm	(( filename ))

mmmmm

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
mmmmm.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
n	(( filename ))

n

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
nn	(( filename ))
\\0\\0\\0
nn

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
nnn	(( filename ))
\\0\\0
nnn

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
nnnn	(( filename ))
\\0
nnnn

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
nnnnn	(( filename ))

nnnnn

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
nnnnn.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
o	(( filename ))

o

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
oo	(( filename ))
\\0\\0\\0
oo

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ooo	(( filename ))
\\0\\0
ooo

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
oooo	(( filename ))
\\0
oooo

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ooooo	(( filename ))

ooooo

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ooooo.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
p	(( filename ))

p

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
pp	(( filename ))
\\0\\0\\0
pp

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ppp	(( filename ))
\\0\\0
ppp

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
pppp	(( filename ))
\\0
pppp

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ppppp	(( filename ))

ppppp

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ppppp.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
q	(( filename ))

q

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
qq	(( filename ))
\\0\\0\\0
qq

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
qqq	(( filename ))
\\0\\0
qqq

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
qqqq	(( filename ))
\\0
qqqq

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
qqqqq	(( filename ))

qqqqq

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
qqqqq.d	(( filename ))
\\0\\0070701	(( magic    ))
070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
r	(( filename ))

r

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
rr	(( filename ))
\\0\\0\\0
rr

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
rrr	(( filename ))
\\0\\0
rrr

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
rrrr	(( filename ))
\\0
rrrr

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
rrrrr	(( filename ))

rrrrr

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
rrrrr.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
s	(( filename ))

s

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ss	(( filename ))
\\0\\0\\0
ss

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
sss	(( filename ))
\\0\\0
sss

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
ssss	(( filename ))
\\0
ssss

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
sssss	(( filename ))

sssss

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
sssss.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
t	(( filename ))

t

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
tt	(( filename ))
\\0\\0\\0
tt

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ttt	(( filename ))
\\0\\0
ttt

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
tttt	(( filename ))
\\0
tttt

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ttttt	(( filename ))

ttttt

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ttttt.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
u	(( filename ))

u

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
uu	(( filename ))
\\0\\0\\0
uu

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
uuu	(( filename ))
\\0\\0
uuu

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
uuuu	(( filename ))
\\0
uuuu

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
uuuuu	(( filename ))

uuuuu

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
uuuuu.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
v	(( filename ))

v

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
vv	(( filename ))
\\0\\0\\0
vv

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
vvv	(( filename ))
\\0\\0
vvv

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
vvvv	(( filename ))
\\0
vvvv

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
vvvvv	(( filename ))

vvvvv

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
vvvvv.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
w	(( filename ))

w

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ww	(( filename ))
\\0\\0\\0
ww

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
www	(( filename ))
\\0\\0
www

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
wwww	(( filename ))
\\0
wwww

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
wwwww	(( filename ))

wwwww

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
wwwww.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
x	(( filename ))

x

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
xx	(( filename ))
\\0\\0\\0
xx

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
xxx	(( filename ))
\\0\\0
xxx

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
xxxx	(( filename ))
\\0
xxxx

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
xxxxx	(( filename ))

xxxxx

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
xxxxx.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
y	(( filename ))

y

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
yy	(( filename ))
\\0\\0\\0
yy

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
yyy	(( filename ))
\\0\\0
yyy

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
yyyy	(( filename ))
\\0
yyyy

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
yyyyy	(( filename ))

yyyyy

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
yyyyy.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
z	(( filename ))

z

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
zz	(( filename ))
\\0\\0\\0
zz

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
zzz	(( filename ))
\\0\\0
zzz

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
zzzz	(( filename ))
\\0
zzzz

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
zzzzz	(( filename ))

zzzzz

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
zzzzz.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
"
  "Contents of the untouched large cpio archive buffer.")
(setq *cdmt-newc-untouched-large-archive-buffer* "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
d	(( filename ))

d

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
dd	(( filename ))
\\0\\0\\0
dd

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ddd	(( filename ))
\\0\\0
ddd

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
dddd	(( filename ))
\\0
dddd

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ddddd	(( filename ))

ddddd

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ddddd.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
e	(( filename ))

e

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ee	(( filename ))
\\0\\0\\0
ee

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
eee	(( filename ))
\\0\\0
eee

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
eeee	(( filename ))
\\0
eeee

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
eeeee	(( filename ))

eeeee

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
eeeee.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
f	(( filename ))

f

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ff	(( filename ))
\\0\\0\\0
ff

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
fff	(( filename ))
\\0\\0
fff

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
ffff	(( filename ))
\\0
ffff

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
fffff	(( filename ))

fffff

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
fffff.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
g	(( filename ))

g

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
gg	(( filename ))
\\0\\0\\0
gg

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ggg	(( filename ))
\\0\\0
ggg

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
gggg	(( filename ))
\\0
gggg

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ggggg	(( filename ))

ggggg

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ggggg.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
h	(( filename ))

h

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
hh	(( filename ))
\\0\\0\\0
hh

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
hhh	(( filename ))
\\0\\0
hhh

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
hhhh	(( filename ))
\\0
hhhh

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
hhhhh	(( filename ))

hhhhh

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
hhhhh.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
i	(( filename ))

i

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ii	(( filename ))
\\0\\0\\0
ii

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
iii	(( filename ))
\\0\\0
iii

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
iiii	(( filename ))
\\0
iiii

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
iiiii	(( filename ))

iiiii

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
iiiii.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
j	(( filename ))

j

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
jj	(( filename ))
\\0\\0\\0
jj

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
jjj	(( filename ))
\\0\\0
jjj

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
jjjj	(( filename ))
\\0
jjjj

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
jjjjj	(( filename ))

jjjjj

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
jjjjj.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
k	(( filename ))

k

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
kk	(( filename ))
\\0\\0\\0
kk

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
kkk	(( filename ))
\\0\\0
kkk

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
kkkk	(( filename ))
\\0
kkkk

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
kkkkk	(( filename ))

kkkkk

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
kkkkk.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
l	(( filename ))

l

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ll	(( filename ))
\\0\\0\\0
ll

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
lll	(( filename ))
\\0\\0
lll

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
llll	(( filename ))
\\0
llll

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
lllll	(( filename ))

lllll

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
lllll.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
m	(( filename ))

m

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
mm	(( filename ))
\\0\\0\\0
mm

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
mmm	(( filename ))
\\0\\0
mmm

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
mmmm	(( filename ))
\\0
mmmm

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
mmmmm	(( filename ))

mmmmm

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
mmmmm.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
n	(( filename ))

n

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
nn	(( filename ))
\\0\\0\\0
nn

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
nnn	(( filename ))
\\0\\0
nnn

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
nnnn	(( filename ))
\\0
nnnn

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
nnnnn	(( filename ))

nnnnn

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
nnnnn.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
o	(( filename ))

o

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
oo	(( filename ))
\\0\\0\\0
oo

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ooo	(( filename ))
\\0\\0
ooo

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
oooo	(( filename ))
\\0
oooo

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ooooo	(( filename ))

ooooo

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ooooo.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
p	(( filename ))

p

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
pp	(( filename ))
\\0\\0\\0
pp

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ppp	(( filename ))
\\0\\0
ppp

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
pppp	(( filename ))
\\0
pppp

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ppppp	(( filename ))

ppppp

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ppppp.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
q	(( filename ))

q

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
qq	(( filename ))
\\0\\0\\0
qq

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
qqq	(( filename ))
\\0\\0
qqq

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
qqqq	(( filename ))
\\0
qqqq

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
qqqqq	(( filename ))

qqqqq

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
qqqqq.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
r	(( filename ))

r

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
rr	(( filename ))
\\0\\0\\0
rr

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
rrr	(( filename ))
\\0\\0
rrr

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
rrrr	(( filename ))
\\0
rrrr

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
rrrrr	(( filename ))

rrrrr

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
rrrrr.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
s	(( filename ))

s

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ss	(( filename ))
\\0\\0\\0
ss

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
sss	(( filename ))
\\0\\0
sss

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
ssss	(( filename ))
\\0
ssss

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
sssss	(( filename ))

sssss

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
sssss.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
t	(( filename ))

t

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
tt	(( filename ))
\\0\\0\\0
tt

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ttt	(( filename ))
\\0\\0
ttt

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
tttt	(( filename ))
\\0
tttt

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ttttt	(( filename ))

ttttt

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ttttt.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
u	(( filename ))

u

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
uu	(( filename ))
\\0\\0\\0
uu

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
uuu	(( filename ))
\\0\\0
uuu

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
uuuu	(( filename ))
\\0
uuuu

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
uuuuu	(( filename ))

uuuuu

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
uuuuu.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
v	(( filename ))

v

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
vv	(( filename ))
\\0\\0\\0
vv

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
vvv	(( filename ))
\\0\\0
vvv

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
vvvv	(( filename ))
\\0
vvvv

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
vvvvv	(( filename ))

vvvvv

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
vvvvv.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
w	(( filename ))

w

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
ww	(( filename ))
\\0\\0\\0
ww

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
www	(( filename ))
\\0\\0
www

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
wwww	(( filename ))
\\0
wwww

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
wwwww	(( filename ))

wwwww

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
wwwww.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
x	(( filename ))

x

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
xx	(( filename ))
\\0\\0\\0
xx

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
xxx	(( filename ))
\\0\\0
xxx

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
xxxx	(( filename ))
\\0
xxxx

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
xxxxx	(( filename ))

xxxxx

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
xxxxx.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
y	(( filename ))

y

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
yy	(( filename ))
\\0\\0\\0
yy

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
yyy	(( filename ))
\\0\\0
yyy

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
yyyy	(( filename ))
\\0
yyyy

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
yyyyy	(( filename ))

yyyyy

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
yyyyy.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
z	(( filename ))

z

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
zz	(( filename ))
\\0\\0\\0
zz

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
zzz	(( filename ))
\\0\\0
zzz

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
zzzz	(( filename ))
\\0
zzzz

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
zzzzz	(( filename ))

zzzzz

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
zzzzz.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
")

(defvar *cdmt-newc-untouched-large-dired-buffer* "CPIO archive: alphabet.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dd
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ddd
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dddd
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ddddd
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ddddd.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} e
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ee
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eee
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eeee
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eeeee
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eeeee.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} f
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ff
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} fff
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ffff
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} fffff
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} fffff.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} g
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} gg
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ggg
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} gggg
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ggggg
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ggggg.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} h
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hh
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhh
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhhh
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhhhh
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhhhh.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} i
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ii
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iii
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iiii
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iiiii
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iiiii.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} j
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jj
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjj
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjjj
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjjjj
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjjjj.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} k
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kk
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkk
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkkk
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkkkk
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkkkk.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} l
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ll
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} lll
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} llll
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} lllll
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} lllll.d
.+"					;emacs barfs if it's much longer than this.
  "Contents of an untouched cpio-dired directory for the large cpio archive.")

(setq *cdmt-newc-untouched-large-dired-buffer* "CPIO archive: alphabet.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dd
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ddd
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dddd
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ddddd
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ddddd.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} e
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ee
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eee
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eeee
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eeeee
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} eeeee.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} f
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ff
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} fff
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ffff
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} fffff
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} fffff.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} g
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} gg
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ggg
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} gggg
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ggggg
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ggggg.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} h
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hh
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhh
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhhh
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhhhh
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} hhhhh.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} i
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ii
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iii
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iiii
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iiiii
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} iiiii.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} j
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jj
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjj
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjjj
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjjjj
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} jjjjj.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} k
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kk
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkk
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkkk
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkkkk
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} kkkkk.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} l
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ll
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} lll
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} llll
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} lllll
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} lllll.d
.+
")


(setq *cdmt-small-archive* *cdmt-newc-small-archive*)
(setq *cdmt-large-archive* *cdmt-newc-large-archive*)
(setq *cdmt-header-re* *cpio-newc-header-re*)

(setq *cpio-magic-re-idx* *cpio-newc-magic-re-idx*)
(setq *cpio-mode-re-idx* *cpio-newc-mode-re-idx*)
(setq *cpio-uid-re-idx* *cpio-newc-uid-re-idx*)
(setq *cpio-gid-re-idx* *cpio-newc-gid-re-idx*)
(setq *cpio-nlink-re-idx* *cpio-newc-nlink-re-idx*)
(setq *cpio-mtime-re-idx* *cpio-newc-mtime-re-idx*)
(setq *cpio-filesize-re-idx* *cpio-newc-filesize-re-idx*)
(setq *cpio-namesize-re-idx* *cpio-newc-namesize-re-idx*)
(setq *cpio-chksum-re-idx* *cpio-newc-chksum-re-idx*)
(setq *cpio-filename-re-idx* *cpio-newc-filename-re-idx*)

(setq *cdmt-archive-format* "newc")



;; 
;; Tests
;; 

;; N.B. cdmt-newc- = cpio-dired-mode-test-

(defvar run-dir default-directory)

(custom-set-variables (list 'cpio-try-names nil))

;; All tests use M-x cpio-dired-kill.
(ert-deftest cdmt-newc-cpio-dired-kill () ;✓
  "Test the function of M-x cpio-dired-kill."
  (let ((test-name "cdmt-newc-cpio-dired-kill")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents))

    (cdmt-reset 'make)
    
    (cpio-dired-kill)
    
    (should (progn (message "Dired style buffer should not be live.")
		   (not (buffer-live-p cpio-dired-buffer))))

    (should (progn (message "Archive buffer should not be live.")
		   (not (buffer-live-p cpio-archive-buffer))))))

(ert-deftest cdmt-newc-cpio-dired-do-isearch ()
  "Test cpio-dired-do-isearch.
cpio-dired-do-isearch is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-isearch)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-isearch-regexp ()
  "Test cpio-dired-do-isearch-regexp.
cpio-dired-do-isearch-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-isearch-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-isearch-entry-names ()
  "Test cpio-dired-isearch-entry-names.
cpio-dired-isearch-entry-names is not yet implemented -- expect an error."
  (should-error (cpio-dired-isearch-entry-names)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-isearch-entry-names-regexp ()
  "Test cpio-dired-isearch-entry-names-regexp.
cpio-dired-isearch-entry-names-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-isearch-entry-names-regexp)
		:type 'error))

;;;;;;;; This gets an end-of-buffer error under ERT.
;;;;;;;; (ert-deftest cdmt-newc-cpio-dired-save-archive-0 () ;✓
;;;;;;;;   "Test the function of M-x cpio-dired-save-archive."
;;;;;;;;   (let ((test-name "cdmt-newc-cpio-dired-save-archive")
;;;;;;;;         (cpio-archive-buffer)
;;;;;;;; 	(cpio-archive-buffer-contents-before)
;;;;;;;;         (cpio-archive-buffer-contents)
;;;;;;;;         (cpio-dired-buffer)
;;;;;;;;         (cpio-dired-buffer-contents-before)
;;;;;;;;         (cpio-dired-buffer-contents)
;;;;;;;;         )
;;;;;;;;     (cdmt-reset 'make)

;;;;;;;;     (progn (goto-char (point-min))
;;;;;;;; 	   (re-search-forward " aa$" (point-max))
;;;;;;;; 	   (cpio-dired-do-delete 1)
;;;;;;;; 	   (setq cpio-archive-buffer-contents-before
;;;;;;;; 		 (cdmt-filter-archive-contents (with-current-buffer cpio-archive-buffer
;;;;;;;; 						 (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents-before (with-current-buffer cpio-dired-buffer
;;;;;;;; 						     (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     (should (progn  (message "Archive buffer should be modified.")
;;;;;;;; 		     (buffer-modified-p cpio-archive-buffer)))
;;;;;;;;     (should (and "Archive buffer should be missing exactly the entry for aa."
;;;;;;;; 		 (string-equal "070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaa	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; b	(( filename ))

;;;;;;;; b

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bbb

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbbb

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 00000000	(( mode     ))
;;;;;;;; 00000000	(( uid      ))
;;;;;;;; 00000000	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 0000000B	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents-before)))
;;;;;;;;     (should (and "Checking dired-style buffer before saving."
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.newc.cpio:

;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
;;;;;;;; " cpio-dired-buffer-contents-before)))

;;;;;;;;     (progn (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-filter-archive-contents 
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min)(point)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (should (and "Archive buffer should no longer be modified."
;;;;;;;; 		 (not (buffer-modified-p cpio-archive-buffer))))
;;;;;;;;     (should (and "Checking the archive buffer after saving."
;;;;;;;; 		 (string-equal "070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaa	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; b	(( filename ))

;;;;;;;; b

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bbb

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbbb

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 00000000	(( mode     ))
;;;;;;;; 00000000	(( uid      ))
;;;;;;;; 00000000	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 0000000B	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents)))

;;;;;;;;     (should (and "Checking the dired-style buffer after saving."
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.newc.cpio:

;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
;;;;;;;; " cpio-dired-buffer-contents)))

;;;;;;;;     ;; The archive strings should be identical up to the TRAILER!!! padding.
;;;;;;;;     ;; NO! Padding after any added, deleted or changed entry will also change.
;;;;;;;;     ;; (string-match "TRAILER!!!" cpio-archive-buffer-contents-before)
;;;;;;;;     ;; (setq cpio-archive-buffer-contents-before (substring cpio-archive-buffer-contents-before 0 (match-end 0)))
;;;;;;;;     ;; (string-match "TRAILER!!!" cpio-archive-buffer-contents)
;;;;;;;;     ;; (setq cpio-archive-buffer-contents (substring cpio-archive-buffer-contents 0 (match-end 0)))
;;;;;;;;     ;; (should (string-equal cpio-archive-buffer-contents-before cpio-archive-buffer-contents))

;;;;;;;;     (should (progn (message "The dired style buffer should not have changed.")
;;;;;;;; 		 (string-equal cpio-dired-buffer-contents-before cpio-dired-buffer-contents)))

;;;;;;;;     (cdmt-reset)

;;;;;;;;     (progn (goto-char (point-min))
;;;;;;;; 	   (re-search-forward " aaaa$" (point-max))
;;;;;;;; 	   (setq unread-command-events (listify-key-sequence "dddd\n"))
;;;;;;;; 	   (cpio-dired-do-rename 1)
;;;;;;;; 	   (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-filter-archive-contents
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (should (and "Expecting the standard archive with aaaa moved to ddddd."
;;;;;;;; 		 (string-equal "070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; b	(( filename ))

;;;;;;;; b

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bbb

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbbb

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))

;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; dddd	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 00000000	(( mode     ))
;;;;;;;; 00000000	(( uid      ))
;;;;;;;; 00000000	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 0000000B	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents)))

;;;;;;;;     (should (and "Expecting a dired style buffer without aaaa."
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.newc.cpio:

;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dddd
;;;;;;;; " cpio-dired-buffer-contents)))

;;;;;;;;     (cdmt-reset)

;;;;;;;;     (progn (goto-char (point-min))
;;;;;;;; 	   (re-search-forward " b$" (point-max))
;;;;;;;; 	   (cpio-dired-mark 4)
;;;;;;;; 	   (setq unread-command-events (listify-key-sequence "d\n"))
;;;;;;;; 	   (cpio-dired-do-rename 1)
;;;;;;;; 	   (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-filter-archive-contents
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (should (and "Expecting a small archive with d/b, d/bb, d/bbb, d/bbbb."
;;;;;;;; 		 (string-equal "070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; dddd	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000007	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; d/bbbb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bbbb

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; d/bbb	(( filename ))

;;;;;;;; bbb

;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; d/bb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000004	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; d/b	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; b

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 00000000	(( mode     ))
;;;;;;;; 00000000	(( uid      ))
;;;;;;;; 00000000	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 0000000B	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents)))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (should (and "Looking for a small dired-style buffer with d/b, d/bb, d/bbb, d/bbbb"
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.newc.cpio:

;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dddd
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d/bbbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d/bbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d/bb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d/b
;;;;;;;; " cpio-dired-buffer-contents)))

;;;;;;;;     (cdmt-reset)

;;;;;;;;     (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
;;;;;;;; 	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
;;;;;;;; 	   ;; HEREHERE This rename does something wrong.
;;;;;;;; 	   (cpio-dired-do-rename 1)
;;;;;;;; 	   (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-filter-archive-contents
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (should (string-equal "070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000002	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000003	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000008	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000006	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000041ED	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000002	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000008	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000005	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; dddd	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000007	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000012	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; newDirectory/bbbb	(( filename ))

;;;;;;;; bbbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000011	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; newDirectory/bbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000005	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000010	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; newDirectory/bb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bb

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000004	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 0000000F	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; newDirectory/b	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; b

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000011	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; newDirectory/ccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; ccc

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 000081A4	(( mode     ))
;;;;;;;; 000003E8	(( uid      ))
;;;;;;;; 000003E8	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000006	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 00000011	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; newDirectory/aaa	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaa

;;;;;;;; 070701	(( magic    ))
;;;;;;;; DEADBEEF	(( ino      ))
;;;;;;;; 00000000	(( mode     ))
;;;;;;;; 00000000	(( uid      ))
;;;;;;;; 00000000	(( gid      ))
;;;;;;;; 00000001	(( nlink    ))
;;;;;;;; DEADBEEF	(( mtime    ))
;;;;;;;; 00000000	(( filesize ))
;;;;;;;; DEADBEEF	(( dev maj  ))
;;;;;;;; DEADBEEF	(( dev min  ))
;;;;;;;; DEADBEEF	(( rdev maj ))
;;;;;;;; DEADBEEF	(( rdev min ))
;;;;;;;; 0000000B	(( namesize ))
;;;;;;;; 00000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))

;;;;;;;;     (should (= 0 1))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (should (string-match "CPIO archive: alphabet_small.newc.cpio:

;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
;;;;;;;;   drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} dddd
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/bbbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/bbb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/bb
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/b
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/ccc
;;;;;;;;   -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaa
;;;;;;;; " cpio-dired-buffer-contents))

;;;;;;;;     (should (= 0 1))

;;;;;;;;     ;; (cdmt-newc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     ))

(ert-deftest cdmt-newc-cpio-dired-add-entry ()
  "Test cpio-dired-add-entry.
cpio-dired-add-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-add-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-change-marks ()
  "Test cpio-dired-change-marks.
cpio-dired-change-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-change-marks)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-clean-directory ()
  "Test cpio-dired-clean-directory.
cpio-dired-clean-directory is not yet implemented -- expect an error."
  (should-error (cpio-dired-clean-directory)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-copy-entry-name-as-kill ()
  (should-error (cpio-dired-copy-entry-name-as-kill 1)
		:type 'error))

(ert-deftest NOT-YET-cdmt-newc-cpio-dired-copy-entry-name-as-kill ()
  "Test cpio-dired-copy-entry-name-as-kill.
cpio-dired-copy-entry-name-as-kill is not yet implemented -- expect an error."
  (let ((test-name "cdmt-newc-cpio-dired-copy-entry-name-as-kill")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-contents-buffer-name)
	(cpio-contents-buffer)
	(cpio-contents-buffer-string)
	(cpio-contents-window)
	(entry-name)
	(current-kill-before)
	(kill-ring-before)
	(entry-names)
	(interprogram-paste-function nil))

    (cdmt-reset 'make)
    
    (progn (setq current-kill-before (current-kill 0 'do-not-move))
	   (cpio-dired-next-line 2)
	   (push (cpio-dired-get-entry-name) entry-names)
	   (cpio-dired-copy-entry-name-as-kill 1))
    
    (while entry-names
      (should (string-equal (current-kill 0) (pop entry-names)))
      (current-kill 1))
    ;; Use (equal) here because the kill ring could have been empty.
    (should (equal (current-kill 0) current-kill-before))
    
    (progn (cpio-dired-next-line 2)
	   (cpio-dired-copy-entry-name-as-kill 4)
	   (save-excursion
	     (let ((i 0))
	       (while (< i 4)
		 (push (cpio-dired-get-entry-name) entry-names)
		 (cpio-dired-next-line 1)
		 (setq i (1+ i))))))
    
    (while entry-names
      (should (string-equal (current-kill 0) (pop entry-names)))
      (current-kill 1))
    ;; Use (equal) here because the kill ring could have been empty.
    (should (equal (current-kill 0) current-kill-before))))

(ert-deftest cdmt-newc-cpio-dired-diff ()
  "Test cpio-dired-diff) ;.
cpio-dired-diff) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-diff) ;)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-display-entry () ;✓
  "Test the function of M-x cpio-dired-display-entry."
  (let ((test-name "cdmt-newc-cpio-dired-display-entry")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-contents-buffer-name)
	(cpio-contents-buffer)
	(cpio-contents-buffer-string)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(cpio-contents-window)
	(entry-name))

    (cdmt-reset 'make)
    
    ;; ????????
    (progn (set-buffer (setq cpio-dired-buffer
			     (get-buffer (cpio-dired-buffer-name *cdmt-small-archive*))))
	   (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq entry-name "aaa")
	   (cpio-dired-goto-entry entry-name)
	   
	   (setq cpio-contents-buffer (cpio-dired-display-entry))
	   
	   ;; (cpio-dired-display-entry) changes the current buffer.
	   (setq cpio-contents-buffer-string
		 (with-current-buffer cpio-contents-buffer
		   (buffer-substring-no-properties (point-min)
						   (point-max))))
	   (setq cpio-contents-window (get-buffer-window cpio-contents-buffer))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   
	   (with-current-buffer cpio-dired-buffer
	     (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog))))
    
    (with-current-buffer cpio-dired-buffer
      (should (progn (message "Viewing an entry should not change the archive buffer.")
		     (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
      (should (progn (message "Viewing an entry should not change the dired-style buffer.")
		     (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
      (should (progn (message "The contents buffer should not be null.")
		     (not (null cpio-contents-buffer))))
      (should (progn (message "The contents buffer should be live.")
		     (buffer-live-p cpio-contents-buffer)))
      (should (progn (message "Check the entry's contents buffer.")
		     (string-equal cpio-contents-buffer-string "\naaa\n\n")))
      (should (progn (message "The entry's contents' window should be live.")
		     (window-live-p cpio-contents-window)))
      (should (progn (message "Expecting no change to the catalog.")
		     (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq entry-name "ccc")
	   (cpio-dired-goto-entry entry-name)
	   
	   (setq cpio-contents-buffer (cpio-dired-display-entry))

	   (with-current-buffer cpio-dired-buffer
	     (setq cpio-contents-buffer-string
		   (with-current-buffer cpio-contents-buffer
		     (buffer-substring-no-properties (point-min)
						     (point-max))))
	     (setq cpio-contents-window (get-buffer-window cpio-contents-buffer))
	     (setq cpio-archive-buffer-contents
		   (cdmt-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))
	     (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog))))
    
    (with-current-buffer cpio-dired-buffer
      (should (progn (message "Checking the archive buffer.")
		     (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
      (should (progn (message "Checking the dired-style buffer.")
		     (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
      (should (not (null cpio-contents-buffer)))
      (should (buffer-live-p cpio-contents-buffer))
      (should (string-equal cpio-contents-buffer-string "\nccc\n\n"))
      (should (window-live-p cpio-contents-window))
      (should (progn (message "Expecting no change to the catalog.")
		     (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))))

(ert-deftest cdmt-newc-cpio-dired-do-async-shell-command ()
  "Test cpio-dired-do-async-shell-command) ;.
cpio-dired-do-async-shell-command) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-async-shell-command)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-chgrp () ;✓
  "Test the function of M-x cpio-dired-do-chgrp."
  (let ((test-name "cdmt-newc-cpio-dired-do-chgrp")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-move-to-first-entry)
	   (setq unread-command-events (listify-key-sequence "9999\n"))
	   (cpio-dired-do-chgrp 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))

    (should (progn (message "Expecting an unchanged archive. (8814)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (progn (message "Expecting 'a' to have group 9999.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  9999        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (and "Expecting a catalog with  the first entry having group 9999."
		  (string-match "((¨a¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 9999 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small.newc.cpio> #<marker at 113 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small.newc.cpio> #<marker at 233 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small.newc.cpio> #<marker at 357 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small.newc.cpio> #<marker at 481 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small.newc.cpio> #<marker at 605 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa.d¨ .
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa.d¨«
\\s-+#<marker at 613 in alphabet_small.newc.cpio> #<marker at 733 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small.newc.cpio> #<marker at 845 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small.newc.cpio> #<marker at 965 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small.newc.cpio> #<marker at 1089 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small.newc.cpio> #<marker at 1213 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small.newc.cpio> #<marker at 1337 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb.d¨ .
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb.d¨«
\\s-+#<marker at 1345 in alphabet_small.newc.cpio> #<marker at 1465 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small.newc.cpio> #<marker at 1577 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small.newc.cpio> #<marker at 1697 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small.newc.cpio> #<marker at 1821 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small.newc.cpio> #<marker at 1945 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small.newc.cpio> #<marker at 2069 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨ccccc.d¨ .
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc.d¨«
\\s-+#<marker at 2077 in alphabet_small.newc.cpio> #<marker at 2197 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq unread-command-events (listify-key-sequence "8888\n"))
	   (cpio-dired-do-chgrp 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The archive buffer doesn't change until saving.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (progn (message "Expecting the first 4 entries to have group 8888.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  8888        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  8888        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  8888        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  8888        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting a catalog with the first 3 entries having group 8888.")
		   (string-match"((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777\n"))
	   (cpio-dired-do-chgrp 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The archive is not changed until saved. (8894)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (progn (message "Expecting \`...\' to have group 7777.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (and "Expecting \`...\' to have group 7777."
		 (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 7777 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 7777 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ 7777 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-chmod ()
  "Test cpio-dired-do-chmod."
  (let ((test-name "cmt-cpio-dired-do-chmod")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq unread-command-events (listify-key-sequence "0755\n"))
	   (cpio-dired-do-chmod 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting the first entry to have mode -rwxr-xr-x." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rwxr-xr-x   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting a mode of 0755 on the first entry." test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "Expecting a mode of 0755 (33261) on the first entry.")
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "0600\n"))
	   (cpio-dired-do-chmod 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting aaa, aaaa, aaaaa to have mode -rw------.")
		   (message "Expecting aaaaa.d to have mode dr--------.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rwxr-xr-x   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:A\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drw-------   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    
    (should (progn (message "%s(): Expecting an unchanged archive buffer." test-name)
		   (message "The archive is not modified until saving.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting aaa, aaaa, aaaaa to have mode 0100600 (33152)." test-name)
		   (message "Expecting aaaaa.d to have mode 040600 (16768).")
		   
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16768 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "0660\n"))
	   (cpio-dired-do-chmod)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting ... to have mode -rw-rw----." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rwxr-xr-x   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-rw----   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drw-------   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-rw----   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-rw----   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    
    (should (progn (message "%s(): Expecting an untouched archive." test-name)
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "Expecting ... to have mode 0100660 (33200).")
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16768 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-save-archive)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))

    (should (progn (message "%s(): Expecting all the above mode changes in the archive buffer." test-name)
		   (message "• a has mode 0100755 (000081ED).")
		   (message "• aaa, aaaa, aaaaa have mode 0100600 (00008180).")
		   (message "• aaaaa.d has mode 040600 (00004180).")
		   (message "• ... have mode 0660 (000081B0 for files or 000041B0 for directories).")
		 (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081B0	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00008180	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00008180	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
00004180	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081B0	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081B0	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting all the above mode changes in the dired buffer." test-name)
		   (message "• a has mode -rwxr-xr-x")
		   (message "• aaa, aaaa, aaaaa, aaaaa.d have mode -rw-r--r--")
		   (message "• ... have mode -rw-rw----")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rwxr-xr-x   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-rw----   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-------   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drw-------   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-rw----   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-rw----   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting all the above mode changes in the catalog.")
		   (message "• a has mode 33261.")
		   (message "• aaa, aaaa, aaaaa have mode 33152.")
		   (message "• aaaaa.d has mode 16768.")
		   (message "• ... have mode 33200 for files.")
		   (message "                16816 for directories.")
		 (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 238 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 363 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 488 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16768 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 970 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1095 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1220 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1702 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1827 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1952 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-chown () ;✓
  "Test the function of M-x cpio-do-chown."
  (let ((test-name "cdmt-newc-cpio-dired-do-chown")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (message "%s(): starting." test-name)
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq unread-command-events (listify-key-sequence "9999\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): The archive buffer is not modified until saved. (10741)" test-name)
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "%s(): Expecting a cpio-dired buffer with the owner of 'a' being 9999." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  9999  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "The owner of 'a' should be 9999.")
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 9999 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "8888\n"))
	   (cpio-dired-do-chown 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): The archive buffer is not modified until saved. (11111)" test-name)
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "%s(): Expecting 4 entries with owner 8888." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  8888  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  8888  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  8888  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  8888  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting  a catalog with 4 entries owned by 8888.")
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 8888 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 8888 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 8888 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 8888 [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))

    (should (progn (message "%s(): The archive buffer is not modified until saved. (10818)" test-name)
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "Expecting \`...\' to be owned by 7777.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  7777  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  7777  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  7777  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting ... to be owned by 7777." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 7777 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 7777 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 7777 [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-chown-1 ()
  "Test the change-owner-user function of M-x cpio-dired-do-chown."
  (let ((test-name "cdmt-newc-cpio-dired-do-chown")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq unread-command-events (listify-key-sequence "9999:1111\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting an untouched archive. (11010)" test-name)
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "%s(): Expecting entry 'a' to have owner 9999 and group 1111." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  9999  1111        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting entry 'a' to have owner 9999 and group 1111." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 9999 1111 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "8888:2222\n"))
	   (cpio-dired-do-chown 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting an untouched archive. (9918)" test-name)
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting 4 entries with owner 8888 and group 2222." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  8888  2222        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting 4 entries with owner 8888 and group 2222." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 8888 2222 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 8888 2222 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 8888 2222 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 8888 2222 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777:3333\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting an untouched archive. (9958)" test-name)
		   (string-equal *cdmt-newc-untouched-small-archive*  cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting \`...\' to have owner 7777 and group 3333." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  7777  3333        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  7777  3333        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  7777  3333        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting ... to have owner 7777 and group 3333." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 7777 3333 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 7777 3333 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 7777 3333 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-compress ()
  "Test cpio-dired-do-compress.
cpio-dired-do-compress is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-compress)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-copy-0 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-newc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (message "%s(): starting." test-name)
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq unread-command-events (listify-key-sequence "d\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))

    (should (progn (message "%s(): Checking that entry »a« has been copied to »d«."  test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
d	(( filename ))

a


" cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Checking that there is an entry »d« in the dired style buffer." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting to see an entry »d«." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨d¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨d¨«
\\s-+#<marker at 2197 in alphabet_small\.newc\.cpio> #<marker at 2309 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-copy-1 () ;✓
  "Test the function of M-x cpio-do-copy."
  :expected-result :failed
  (let ((test-name "cdmt-newc-cpio-dired-do-copy-1")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (message "%s(): starting." test-name)
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
	   (cpio-dired-do-copy 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    ;; HEREHERE I think that this expected value is incorrect.
    ;; It implies an archive with two entries
    ;; whose padding does not end after a (mod ... *cpio-padding-modulus*) = 0
    ;; character position.
    ;; However, cpio(1GNU) seems to cope with it.
    ;; Note the use of the term character position, since cpio counts characters
    ;; starting at 0, while emacs starts counting (point) at 1.
    (should (progn (message "%s(): Checking for »aaa«, »aaaa«, »aaaaa«, »aaaaa« copied to newDirectory in the archive." test-name)
		   (string-equal "uNlIkElY" cpio-archive-buffer-contents)))
    
    (should (progn (message "%s(): Checking for the presence of »newDirectory/aaa«, »newDirectory/aaaa«, »newDirectory/aaaaa«, »newDirectory/aaaaa«." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaa
" cpio-dired-buffer-contents)))

    (should (string-match "uNlIkElY" cpio-catalog-contents-after))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-copy-2 () ;✓
  "Test the function of M-x cpio-do-copy operating on multiple entries."
  (let ((test-name "cdmt-newc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (message "%s(): starting." test-name)
    
    (cdmt-reset 'make)
    
    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "newDirectory-1\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting an archive with each 3 letter entry copied to newDirectory-1." test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory-1/ccc	(( filename ))
\\0\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory-1/bbb	(( filename ))
\\0\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory-1/aaa	(( filename ))
\\0\\0\\0
aaa

\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting a dired-style buffer with 3 entries in newDirectory-1." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/ccc
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/bbb
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/aaa
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting to see ... entries in newDirectory-1." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨newDirectory-1/ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-1/ccc¨«
\\s-+#<marker at 2197 in alphabet_small\.newc\.cpio> #<marker at 2329 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨newDirectory-1/bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-1/bbb¨«
\\s-+#<marker at 2337 in alphabet_small\.newc\.cpio> #<marker at 2469 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
\\s-+(¨newDirectory-1/aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-1/aaa¨«
\\s-+#<marker at 2477 in alphabet_small\.newc\.cpio> #<marker at 2609 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))


(ert-deftest cdmt-newc-cpio-dired-do-copy-3 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-newc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (message "%s(): starting." test-name)
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "...")
	   (setq unread-command-events (listify-key-sequence "newDirectory-3\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    ;; HEREHERE Once again, I think this archive has some incorrect
    ;; entry padding.
    (should (progn (message "%s(): Expecting an archive with each entry named with at least 3 letters copied to newDirectory-3." test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000017	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/ccccc.d	(( filename ))
\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000015	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/ccccc	(( filename ))
\\0
ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000014	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/cccc	(( filename ))
\\0\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/ccc	(( filename ))
\\0\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000017	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/bbbbb.d	(( filename ))
\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000015	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/bbbbb	(( filename ))
\\0
bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000014	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/bbbb	(( filename ))
\\0\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/bbb	(( filename ))
\\0\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000017	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/aaaaa.d	(( filename ))
\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000015	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/aaaaa	(( filename ))
\\0
aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000014	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/aaaa	(( filename ))
\\0\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory-3/aaa	(( filename ))
\\0\\0\\0
aaa

\\0\\0
" cpio-archive-buffer-contents)))
    
    (should (progn (message "%s(): Expecting all entries named with at least 3 letters to have copies in newDirectory-3." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
C drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/ccccc.d
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/ccccc
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/cccc
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/ccc
C drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbbbb.d
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbbbb
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbbb
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbb
C drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaaaa.d
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaaaa
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaaa
C -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaa
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting all entries named with at least 3 letters to have copies in newDirectory-3." test-name)
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 23 0 ¨newDirectory-3/ccccc\.d¨«
\\s-+#<marker at 2197 in alphabet_small\.newc\.cpio> #<marker at 2333 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 ¨newDirectory-3/ccccc¨«
\\s-+#<marker at 2333 in alphabet_small\.newc\.cpio> #<marker at 2465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 20 0 ¨newDirectory-3/cccc¨«
\\s-+#<marker at 2473 in alphabet_small\.newc\.cpio> #<marker at 2605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-3/ccc¨«
\\s-+#<marker at 2613 in alphabet_small\.newc\.cpio> #<marker at 2745 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 23 0 ¨newDirectory-3/bbbbb\.d¨«
\\s-+#<marker at 2753 in alphabet_small\.newc\.cpio> #<marker at 2889 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 ¨newDirectory-3/bbbbb¨«
\\s-+#<marker at 2889 in alphabet_small\.newc\.cpio> #<marker at 3021 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 20 0 ¨newDirectory-3/bbbb¨«
\\s-+#<marker at 3029 in alphabet_small\.newc\.cpio> #<marker at 3161 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-3/bbb¨«
\\s-+#<marker at 3169 in alphabet_small\.newc\.cpio> #<marker at 3301 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 23 0 ¨newDirectory-3/aaaaa\.d¨«
\\s-+#<marker at 3309 in alphabet_small\.newc\.cpio> #<marker at 3445 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 ¨newDirectory-3/aaaaa¨«
\\s-+#<marker at 3445 in alphabet_small\.newc\.cpio> #<marker at 3577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 20 0 ¨newDirectory-3/aaaa¨«
\\s-+#<marker at 3585 in alphabet_small\.newc\.cpio> #<marker at 3717 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-3/aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-3/aaa¨«
\\s-+#<marker at 3725 in alphabet_small\.newc\.cpio> #<marker at 3857 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-copy-regexp ()
  "Test cpio-dired-do-copy-regexp.
cpio-dired-do-copy-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-copy-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-delete () ;✓
  "Test the function of M-x cpio-dired-do-delete."
  (let ((test-name "cdmt-newc-cpio-dired-do-delete")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (message "%s(): starting." test-name)

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-do-delete 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting entry »a« to be deleted." test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting entry »a« to be deleted." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting a catalog with entry »a« deleted." test-name)
		   (string-match "((¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 117 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 125 in alphabet_small\.newc\.cpio> #<marker at 241 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 249 in alphabet_small\.newc\.cpio> #<marker at 365 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 373 in alphabet_small\.newc\.cpio> #<marker at 489 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 497 in alphabet_small\.newc\.cpio> #<marker at 617 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 617 in alphabet_small\.newc\.cpio> #<marker at 729 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 849 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 857 in alphabet_small\.newc\.cpio> #<marker at 973 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 981 in alphabet_small\.newc\.cpio> #<marker at 1097 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1105 in alphabet_small\.newc\.cpio> #<marker at 1221 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1229 in alphabet_small\.newc\.cpio> #<marker at 1349 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1349 in alphabet_small\.newc\.cpio> #<marker at 1461 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1581 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1589 in alphabet_small\.newc\.cpio> #<marker at 1705 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1713 in alphabet_small\.newc\.cpio> #<marker at 1829 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1837 in alphabet_small\.newc\.cpio> #<marker at 1953 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 1961 in alphabet_small\.newc\.cpio> #<marker at 2081 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 2)
	   (cpio-dired-do-delete 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): $xpecting an archive with one archive deleted but otherwise unchanged." test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting a cpio-dired buffer with 4 more entries deleted." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting a catalog with entries" test-name)
		   (message "    »aaaa«, »aaaaa«, »aaaaa.d« and »b« deleted.")
		   (string-match "((¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 117 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 125 in alphabet_small\.newc\.cpio> #<marker at 241 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 249 in alphabet_small\.newc\.cpio> #<marker at 365 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 373 in alphabet_small\.newc\.cpio> #<marker at 489 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 497 in alphabet_small\.newc\.cpio> #<marker at 613 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 621 in alphabet_small\.newc\.cpio> #<marker at 737 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 745 in alphabet_small\.newc\.cpio> #<marker at 865 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 865 in alphabet_small\.newc\.cpio> #<marker at 977 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 981 in alphabet_small\.newc\.cpio> #<marker at 1097 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1105 in alphabet_small\.newc\.cpio> #<marker at 1221 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1229 in alphabet_small\.newc\.cpio> #<marker at 1345 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1353 in alphabet_small\.newc\.cpio> #<marker at 1469 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 1477 in alphabet_small\.newc\.cpio> #<marker at 1597 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "\n"))
	   (cpio-dired-do-delete 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "%s(): Expecting an archive with ... deleted." test-name)
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
00000000	(( mode     ))
00000000	(( uid      ))
00000000	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000B	(( namesize ))
00000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "%s(): Expecting a cpio-dired buffer with ... all deleted." test-name)
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "%s(): Expecting a catalog with further entries \`...\' deleted." test-name)
		   (string-match "((¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 117 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 125 in alphabet_small\.newc\.cpio> #<marker at 241 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 249 in alphabet_small\.newc\.cpio> #<marker at 365 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 373 in alphabet_small\.newc\.cpio> #<marker at 489 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 497 in alphabet_small\.newc\.cpio> #<marker at 617 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 617 in alphabet_small\.newc\.cpio> #<marker at 729 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 849 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 857 in alphabet_small\.newc\.cpio> #<marker at 973 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 981 in alphabet_small\.newc\.cpio> #<marker at 1097 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 1105 in alphabet_small\.newc\.cpio> #<marker at 1225 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-flagged-delete ()
  "Test cpio-dired-do-flagged-delete.
cpio-dired-do-flagged-delete is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-flagged-delete)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-hardlink ()
  "Test cpio-dired-do-hardlink.
cpio-dired-do-hardlink is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-hardlink)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-hardlink-regexp ()
  "Test cpio-dired-do-hardlink-regexp.
cpio-dired-do-hardlink-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-hardlink-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-kill-lines ()
  "Test cpio-dired-do-kill-lines.
cpio-dired-do-kill-lines is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-kill-lines)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-print ()
  "Test cpio-dired-do-print.
cpio-dired-do-print is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-print)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-query-replace-regexp ()
  "Test cpio-dired-do-query-replace-regexp.
cpio-dired-do-query-replace-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-query-replace-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-redisplay ()
  "Test cpio-dired-do-redisplay.
cpio-dired-do-redisplay is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-redisplay)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-rename () ;✓
  (let ((test-name "cdmt-newc-cpio-dired-do-rename")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq unread-command-events (listify-key-sequence "d\n"))
	   (cpio-dired-do-rename 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an untouched archive.")
		   (message "The archive gets updated on save.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "Expecting a dired buffer with no entry »a«, but an entry »d«.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    
    (should (progn (message "Expecting catalog with first entry »d«.")
		   (string-match "((¨d¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨d¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
	   (cpio-dired-do-rename 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an as yet unchanged archive.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "Expecting a dired style buffer with entries »aaaa«, »aaaaa«, »aaaaa.d« and »b« moved to »newDirectory«.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    
    (should (progn (message "Expecting a catalog with the above changes.")
		   (string-match "((¨d¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨d¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 17 0 ¨newDirectory/aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 18 0 ¨newDirectory/aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory/aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 ¨newDirectory/aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "newDirectory-1\n"))
	   (cpio-dired-do-rename 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an as yet unchanged archive.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (progn (message "Expecting a dired buffer with \`...\' all under newDirectory-1.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting a catalog with \`...\' entries in newDirectory-1.")
		   (string-match "((¨d¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨d¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 17 0 ¨newDirectory/aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 18 0 ¨newDirectory/aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory/aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory/aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 ¨newDirectory/aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-1/bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-1/bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨newDirectory-1/ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 ¨newDirectory-1/ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-do-rename-regexp ()
  "Test cpio-dired-do-rename-regexp.
cpio-dired-do-rename-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-rename-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-search () ;HEREHERE ()
  "Test cpio-dired-do-search) ;HEREHERE.
cpio-dired-do-search) ;HEREHERE is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-search)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-symlink ()
  "Test cpio-dired-do-symlink.
cpio-dired-do-symlink is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-symlink)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-do-symlink-regexp ()
  "Test cpio-dired-do-symlink-regexp.
cpio-dired-do-symlink-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-symlink-regexp)
		:type 'error))

;; MAINTENANCE Date formats age. Some of these results will fail in 6 months.
(ert-deftest cdmt-newc-cpio-dired-do-touch ()
  "Test cpio-dired-do-touch."
  ;; Test this one by hand because the original mod times will change.
  (should-error (cpio-dired-do-symlink-regexp)
		:type 'error)
;;;;   (let ((test-name "cdmt-newc-cpio-dired-find-entry")
;;;;         (cpio-archive-buffer)
;;;;         (cpio-archive-buffer-contents)
;;;;         (cpio-dired-buffer)
;;;;         (cpio-dired-buffer-contents)
;;;; 	(cpio-catalog-contents-before)
;;;; 	(cpio-catalog-contents-after)
;;;; 	(cpio-contents-window)
;;;; 	(entry-name)
;;;; 	(past-entries ())
;;;; 	(time-str))
;;;; 
;;;;     (cdmt-reset 'make)

;;;;     (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
;;;; 	   (setq entry-name "aaa")
;;;; 	   (cpio-dired-goto-entry entry-name)
;;;; 	   (setq time-str "2018 Nov 1")
;;;; 	   (setq unread-command-events (listify-key-sequence (format "%s\n" time-str)))
;;;; 	   (cpio-dired-do-touch 1)
;;;; 	   
;;;; 	   (setq cpio-archive-buffer-contents
;;;; 		 (cdmt-filter-archive-contents
;;;; 		  (with-current-buffer cpio-archive-buffer
;;;; 		    (buffer-substring-no-properties (point-min) (point-max))) 'keep-mtime))
;;;; 	   (setq cpio-dired-buffer-contents
;;;; 		 (with-current-buffer cpio-dired-buffer
;;;; 		   (buffer-substring-no-properties (point-min) (point-max))))
;;;; 	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog 'keep-mtime)))
;;;;     
;;;;     (should (progn (message "%s(): Expecting a dired buffer with »aaa« having a modification time of »%s«." test-name time-str)
;;;; 		   (string-match "CPIO archive: alphabet_small.newc.cpio:
;;;; 
;;;;   -rw-r--r--   1  1000  1000        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
;;;;   -rw-r--r--   1  1000  1000        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
;;;;   -rw-r--r--   1  1000  1000        6 Nov 01 00:00 aaa
;;;;   -rw-r--r--   1  1000  1000        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
;;;;   -rw-r--r--   1  1000  1000        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
;;;;   drwxr-xr-x   2  1000  1000        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
;;;;   -rw-r--r--   1  1000  1000        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
;;;;   -rw-r--r--   1  1000  1000        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
;;;;   -rw-r--r--   1  1000  1000        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
;;;;   -rw-r--r--   1  1000  1000        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
;;;;   -rw-r--r--   1  1000  1000        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
;;;;   drwxr-xr-x   2  1000  1000        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
;;;;   -rw-r--r--   1  1000  1000        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
;;;;   -rw-r--r--   1  1000  1000        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
;;;;   -rw-r--r--   1  1000  1000        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
;;;;   -rw-r--r--   1  1000  1000        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
;;;;   -rw-r--r--   1  1000  1000        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
;;;;   drwxr-xr-x   2  1000  1000        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d\n"
;;;; 			  cpio-dired-buffer-contents)))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting an archive with »aaa« having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-archive-buffer-contents))
;;;; 
;;;;     ;; (should (progn (message "%s: Expecting a catalog with »aaa« having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-catalog-contents-after))
;;;; 
;;;;     (cpio-reset 'make)
;;;; 
;;;;     (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
;;;; 	   (setq entry-name "b")
;;;; 	   (cpio-dired-goto-entry entry-name)
;;;; 	   (cpio-dired-mark 4)
;;;; 	   (setq time-str "2018 Nov 2 1:02:03")
;;;; 	   (setq unread-command-events (listify-key-sequence (format "%s\n" time-str)))
;;;; 	   (cpio-dired-do-touch 1)
;;;; 	   
;;;; 	   (setq cpio-archive-buffer-contents
;;;; 		 (cdmt-filter-archive-contents
;;;; 		  (with-current-buffer cpio-archive-buffer
;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;; 	   (setq cpio-dired-buffer-contents
;;;; 		 (with-current-buffer cpio-dired-buffer
;;;; 		   (buffer-substring-no-properties (point-min) (point-max))))
;;;; 	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting a dired buffer with files starting sith »b«" test-name
;;;;     ;; (message "%s "having modification times of »2018 Nov 2 1:02:03«." test-name
;;;;     (should (string-equal "uNlIkElY" cpio-dired-buffer-ontents))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting an archive with »aaa« having a modification time of »2018 Nov 2 1:02:03«" test-name)
;;;;     (should (string-equal "uNlIkElY" cpio-archive-buffer-contents))
;;;; 
;;;;     ;; (should (progn (message "%s: Expecting a catalog with »aaa« having a modification time of »2018 Nov 2 1:02:03«" test-name)
;;;;     (should (string-equal "uNlIkElY" cpio-catalog-contents-after))
;;;; 
;;;;     (cpio-reset 'make)
;;;; 
;;;;     (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
;;;; 	   (setq entry-name "c")
;;;; 	   (cpio-dired-goto-entry entry-name)
;;;; 	   (cpio-dired-mark 4)
;;;; 	   (setq time-str "2018-11-24 05:06:07")
;;;; 	   (setq unread-command-events (listify-key-sequence (format "%s\n" time-str)))
;;;; 	   (cpio-dired-do-touch 4)
;;;; 	   
;;;; 	   (setq cpio-archive-buffer-contents
;;;; 		 (cdmt-filter-archive-contents
;;;; 		  (with-current-buffer cpio-archive-buffer
;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;; 	   (setq cpio-dired-buffer-contents
;;;; 		 (with-current-buffer cpio-dired-buffer
;;;; 		   (buffer-substring-no-properties (point-min) (point-max))))
;;;; 	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting a dired buffer with files starting sith »c«" test-name
;;;;     ;; (message "%s "having modification times of »%s«." test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-dired-buffer-ontents))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting an archive with »aaa« having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-archive-buffer-contents))
;;;; 
;;;;     ;; (should (progn (message "%s: Expecting a catalog with »aaa« having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-catalog-contents-after))
;;;; 
;;;;     (cpio-reset 'make)
;;;; 
;;;;     (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
;;;; 	   (setq entry-name "c")
;;;; 	   (cpio-dired-goto-entry entry-name)
;;;; 	   (cpio-dired-mark-entries-regexp "\\`...\\'")
;;;; 	   (setq time-str "2001 january 1 1:01:01")
;;;; 	   (setq unread-command-events (listify-key-sequence (format "%s\n" time-str)))
;;;; 	   (cpio-dired-do-touch 1)
;;;; 	   
;;;; 	   (setq cpio-archive-buffer-contents
;;;; 		 (cdmt-filter-archive-contents
;;;; 		  (with-current-buffer cpio-archive-buffer
;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;; 	   (setq cpio-dired-buffer-contents
;;;; 		 (with-current-buffer cpio-dired-buffer
;;;; 		   (buffer-substring-no-properties (point-min) (point-max))))
;;;; 	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting a dired buffer with 3-char filenames" test-name
;;;;     ;; (message "%s "having modification times of »%s«." test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-dired-buffer-ontents))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting an archive with 3-char filenames having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-archive-buffer-contents))
;;;; 
;;;;     ;; (should (progn (message "%s: Expecting a catalog with 3-char filenames having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-catalog-contents-after))
;;;; 
;;;;     (cpio-reset 'make)
;;;; 
;;;;     (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
;;;; 	   (setq entry-name "c")
;;;; 	   (cpio-dired-goto-entry entry-name)
;;;; 	   (cpio-dired-mark-entries-regexp "\\`....\\'")
;;;; 	   (setq time-str "1/13/2018 14:15:16")
;;;; 	   (setq unread-command-events (listify-key-sequence (format "%s\n" time-str)))
;;;; 	   (cpio-dired-do-touch 1)
;;;; 	   
;;;; 	   (setq cpio-archive-buffer-contents
;;;; 		 (cdmt-filter-archive-contents
;;;; 		  (with-current-buffer cpio-archive-buffer
;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;; 	   (setq cpio-dired-buffer-contents
;;;; 		 (with-current-buffer cpio-dired-buffer
;;;; 		   (buffer-substring-no-properties (point-min) (point-max))))
;;;; 	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting a dired buffer with 3-char filenames" test-name
;;;;     ;; (message "%s "having modification times of »%s«." test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-dired-buffer-ontents))
;;;; 
;;;;     ;; (should (progn (message "%s(): Expecting an archive with 3-char filenames having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-archive-buffer-contents))
;;;; 
;;;;     ;; (should (progn (message "%s: Expecting a catalog with 3-char filenames having a modification time of »%s«" test-name time-str)
;;;;     (should (string-equal "uNlIkElY" cpio-catalog-contents-after))
;;;; 
;;;;    (cdmt-test-save *cdmt-archive-format*)))
    )
(ert-deftest cdmt-newc-cpio-dired-downcase ()
  "Test cpio-dired-downcase.
cpio-dired-downcase is not yet implemented -- expect an error."
  (should-error (cpio-dired-downcase)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-extract-all ()
  "Test cpio-dired-extract-all.
cpio-dired-extract-all is not yet implemented -- expect an error."
  (should-error (cpio-dired-extract-all)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-extract-entries ()
  "Test cpio-dired-extract-entries.
cpio-dired-extract-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-extract-entries)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-find-alternate-entry ()
  "Test cpio-dired-find-alternate-entry.
cpio-dired-find-alternate-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-find-alternate-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-find-entry () ;✓
  "Test the function of M-x cpio-find-entry.
Expect errors about killed buffers.
They reflect an outstanding bug in cpio-affiliated buffers."
  
  (let ((test-name "cdmt-newc-cpio-dired-find-entry")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-contents-buffer)
	(cpio-contents-buffer-string)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(cpio-contents-window)
	(entry-name)
	(past-entries ()))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq entry-name "aaa")
	   (cpio-dired-goto-entry entry-name)
	   (cpio-dired-display-entry)
	   ;; (cpio-dired-display-entry) changes the current buffer.
	   (with-current-buffer cpio-dired-buffer
	     (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name entry-name)))
	     (setq cpio-contents-buffer-string (with-current-buffer cpio-contents-buffer
						 (buffer-substring-no-properties (point-min) (point-max))))
	     (setq cpio-contents-window (get-buffer-window cpio-contents-buffer))
	     (setq cpio-archive-buffer-contents
		   (cdmt-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))
	     (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog))))
    
    (with-current-buffer cpio-dired-buffer
      (should (progn (message "Expecting an untouched small archive. (14832)")
		     (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
      (should (progn (message  "Expecting an untouched small dired buffer (14834)")
		     (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
      (should (progn (message "Expecting a real entry contents buffer. (14836)")
		     (not (null cpio-contents-buffer))))
      (should (progn (message "...and expecting the entry contents buffer to be live. (14838)")
		     (buffer-live-p cpio-contents-buffer)))
      (should (progn (message (format "Expecting that buffer to contain the contents of [[%s]]. (14840)" entry-name))
		     (string-equal cpio-contents-buffer-string (concat "\n" entry-name "\n\n"))))
      (should (progn (message "Expecting a live entry contents window. (14842)")
		     (window-live-p cpio-contents-window)))
      (should (progn (message "Expecting an unchanged entry contents buffer. (14844)")
		     (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))
    
    (push entry-name past-entries)
    
    (switch-to-buffer cpio-dired-buffer)
    
    (progn (setq entry-name "ccc")
	   (cpio-dired-goto-entry entry-name)
	   (cpio-dired-display-entry)
	   ;; (cpio-dired-display-entry) changes the current-buffer.
	   (with-current-buffer cpio-dired-buffer
	     (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name entry-name)))
	     (setq cpio-contents-buffer-string (with-current-buffer cpio-contents-buffer
						 (buffer-substring-no-properties (point-min)
										 (point-max))))
	     (setq cpio-contents-window (get-buffer-window cpio-contents-buffer))
	     (setq cpio-archive-buffer-contents
		   (cdmt-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))))
    
    (with-current-buffer cpio-dired-buffer
      (should (progn (message "Expecting an unchanged small archive. (14870).")
		     (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
      (should (progn (message "Expecting an untouched cpio-dired buffer (14872)")
		     (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
      (should (not (null cpio-contents-buffer)))
      (should (buffer-live-p cpio-contents-buffer))
      (should (string-equal cpio-contents-buffer-string "\nccc\n\n"))
      (should (window-live-p cpio-contents-window))
      (should (progn (message "Expecting an unchanged catalog. (17957)")
		     (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))
    
    ;; Now make sure that any past entries are still there.
    (mapc (lambda (en)
	    (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name entry-name)))
	    (setq cpio-contents-buffer-string (with-current-buffer cpio-contents-buffer
						(buffer-substring-no-properties (point-min)
										(point-max))))
	    (should (not (null cpio-contents-buffer)))
	    (should (buffer-live-p cpio-contents-buffer))
	    (should (string-equal cpio-contents-buffer-string (concat "\n" entry-name "\n\n")))
	    (should (window-live-p cpio-contents-window)))
	  past-entries)
    
    ;; Affiliated buffers don't get killed when the parent does yet.
    (push entry-name past-entries)
    (mapc (lambda (en)
	    (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name en)))
	    (if (buffer-live-p cpio-contents-buffer)
		(kill-buffer cpio-contents-buffer)))
	  past-entries)))

(ert-deftest cdmt-newc-cpio-dired-find-entry-other-window ()
  "Test cpio-dired-find-entry-other-window.
cpio-dired-find-entry-other-window is not yet implemented -- expect an error."
  (should-error (cpio-dired-find-entry-other-window)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-flag-auto-save-entries () ;✓
  "Test the function of M-x cpio-dired-flag-auto-save-entries."
  (let ((test-name "cdmt-newc-cpio-dired-flag-auto-save-entries")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (beginning-of-line)
	   (while (re-search-forward " \\(.\\)$" (point-max) t)
	     (setq unread-command-events (listify-key-sequence (concat "#" (match-string-no-properties 1) "\n")))
	     (cpio-dired-do-copy 1))
	   (cpio-dired-flag-auto-save-entries)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an archive with autosave entries")
		   (message "for each single character entry.")
		   (message "(The copy used to create them must update the archive.)")
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
#a	(( filename ))
\\0\\0\\0
a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
#b	(( filename ))
\\0\\0\\0
b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
#c	(( filename ))
\\0\\0\\0
c


" cpio-archive-buffer-contents)))

    (should (progn (message "Expecting a cpio-dired buffer with an autosave entry for each onecharacter entry.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} #a
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} #b
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} #c
" cpio-dired-buffer-contents)))
    
    (should (progn (message "Expecting a catalog with autosave entries for each one-character entry.")
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨#a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨#a¨«
\\s-+#<marker at 2197 in alphabet_small\.newc\.cpio> #<marker at 2313 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨#b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨#b¨«
\\s-+#<marker at 2317 in alphabet_small\.newc\.cpio> #<marker at 2433 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨#c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨#c¨«
\\s-+#<marker at 2437 in alphabet_small\.newc\.cpio> #<marker at 2553 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-flag-backup-entries ()
  "Test cpio-dired-flag-backup-entries.
cpio-dired-flag-backup-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-backup-entries)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-flag-entries-regexp ()
  "Test cpio-dired-flag-entries-regexp.
cpio-dired-flag-entries-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-entries-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-flag-entry-deletion () ;✓
  "Test the function of M-x cpio-flag-entry-deletion."
  (let ((test-name "cdmt-newc-cpio-dired-flag-entry-deletion")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-flag-entry-deletion 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting and unchanged small archive. (15472)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (progn (message "Expecting a cpio-dired buffer with one entry flagged for deletion.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17958)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 2)
	   (cpio-dired-flag-entry-deletion 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting and unchanged small archive. (15510)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a cpio-dired buffer with another 4 entries flagged for deletion.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
D drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17959)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-flag-garbage-entries ()
  "Test cpio-dired-flag-garbage-entries."
  (let ((test-name "cdmt-newc-cpio-dired-flag-entry-deletion")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(entry-name "aa"))
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cpio-catalog))
	   (cpio-dired-goto-entry entry-name)
	   (mapc (lambda (s)		;suffix
		   (setq unread-command-events (listify-key-sequence (concat entry-name "." s "\n")))
		   (cpio-dired-do-copy 1))
		 (list "aux" "bak" "dvi" "log" "orig" "rej" "toc"))
	   (cpio-dired-flag-garbage-entries)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))

    (should (progn (message "Expecting an archive with entries for suffixes")
		   (message "    aux bak dvi log orig reg toc.")
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000007	(( namesize ))
00000000	(( chksum   ))
aa.aux	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000007	(( namesize ))
00000000	(( chksum   ))
aa.bak	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000007	(( namesize ))
00000000	(( chksum   ))
aa.dvi	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000007	(( namesize ))
00000000	(( chksum   ))
aa.log	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aa.orig	(( filename ))
\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000007	(( namesize ))
00000000	(( chksum   ))
aa.rej	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000007	(( namesize ))
00000000	(( chksum   ))
aa.toc	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "Expecting a dired-style buffer with marked entries")
		   (message "    for the suffixes")
		   (message "    aux bak dvi log orig reg toc.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.aux
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.bak
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.dvi
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.log
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.orig
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.rej
D -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa.toc
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting a catalog with entries with the suffixes")
		   (message "    aux bak dvi log orig reg toc.")
		   (string-match "((¨a¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small\.newc\.cpio> #<marker at 113 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small\.newc\.cpio> #<marker at 233 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small\.newc\.cpio> #<marker at 357 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small\.newc\.cpio> #<marker at 481 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small\.newc\.cpio> #<marker at 605 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa\.d¨«
\\s-+#<marker at 613 in alphabet_small\.newc\.cpio> #<marker at 733 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small\.newc\.cpio> #<marker at 845 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small\.newc\.cpio> #<marker at 965 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small\.newc\.cpio> #<marker at 1089 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small\.newc\.cpio> #<marker at 1213 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small\.newc\.cpio> #<marker at 1337 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb\.d¨«
\\s-+#<marker at 1345 in alphabet_small\.newc\.cpio> #<marker at 1465 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small\.newc\.cpio> #<marker at 1577 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small\.newc\.cpio> #<marker at 1697 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small\.newc\.cpio> #<marker at 1821 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small\.newc\.cpio> #<marker at 1945 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small\.newc\.cpio> #<marker at 2069 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc\.d¨ \.
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc\.d¨«
\\s-+#<marker at 2077 in alphabet_small\.newc\.cpio> #<marker at 2197 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.aux¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 ¨aa\.aux¨«
\\s-+#<marker at 2197 in alphabet_small\.newc\.cpio> #<marker at 2317 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.bak¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 ¨aa\.bak¨«
\\s-+#<marker at 2325 in alphabet_small\.newc\.cpio> #<marker at 2445 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.dvi¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 ¨aa\.dvi¨«
\\s-+#<marker at 2453 in alphabet_small\.newc\.cpio> #<marker at 2573 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.log¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 ¨aa\.log¨«
\\s-+#<marker at 2581 in alphabet_small\.newc\.cpio> #<marker at 2701 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.orig¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aa\.orig¨«
\\s-+#<marker at 2709 in alphabet_small\.newc\.cpio> #<marker at 2829 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.rej¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 ¨aa\.rej¨«
\\s-+#<marker at 2837 in alphabet_small\.newc\.cpio> #<marker at 2957 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«)
 (¨aa\.toc¨ \.
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 ¨aa\.toc¨«
\\s-+#<marker at 2965 in alphabet_small\.newc\.cpio> #<marker at 3085 in alphabet_small\.newc\.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))

    (cdmt-test-save *cdmt-archive-format*)))

(ert-deftest cdmt-newc-cpio-dired-goto-entry ()
  "Test cpio-dired-goto-entry.
cpio-dired-goto-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-goto-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-hide-all ()
  "Test cpio-dired-hide-all.
cpio-dired-hide-all is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-all)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-hide-details-mode ()
  "Test cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing.
cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-hide-subdir ()
  "Test cpio-dired-hide-subdir) ;.
cpio-dired-hide-subdir) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-subdir)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-mark () ;✓
  "Test the function of M-x cpio-dired-mark."
  (let ((test-name "cdmt-newc-cpio-dired-mark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting and unchanged small archive. (16216)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a cpio-dired buffer with the first entry marked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17960)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 2)
	   (cpio-dired-mark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting and unchanged small archive. (16254)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a cpio-dired buffer with 4 more entries marked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17961)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-mark-directories ()
  "Test cpio-dired-mark-directories.
cpio-dired-mark-directories is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-directories)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-mark-entries-containing-regexp ()
  "Test cpio-dired-mark-entries-containing-regexp.
cpio-dired-mark-entries-containing-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-entries-containing-regexp)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-mark-entries-regexp () ;✓
  (let ((test-name "cdmt-newc-cpio-dired-mark-entries-regexp")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting and unchanged small archive. (16313)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a cpio-dired buffer with ... marked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17962)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-mark-executables ()
  "Test cpio-dired-mark-executables.
cpio-dired-mark-executables is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-executables)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-mark-subdir-entries ()
  "Test cpio-dired-mark-subdir-entries.
cpio-dired-mark-subdir-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-subdir-entries)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-mark-symlinks ()
  "Test cpio-dired-mark-symlinks.
cpio-dired-mark-symlinks is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-symlinks)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-mouse-find-entry-other-window ()
  "Test cpio-dired-mouse-find-entry-other-window.
cpio-dired-mouse-find-entry-other-window is not yet implemented -- expect an error."
  (should-error (cpio-dired-mouse-find-entry-other-window)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-next-dirline () ;✓
  "Test the function of M-x cpio-dired-next-dirline."
  (let ((test-name "cdmt-newc-cpio-dired-next-dirline")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(entry-name))

    (cdmt-reset 'make 'large)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-dirline 1)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The current entry should be aaaaa.d")
		   (string-equal "aaaaa.d" entry-name)))
    (should (progn (message "Expecting an untouched large archive buffer.")
		   (string-equal *cdmt-newc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (progn (message "The dired style buffer should be untouched.")
		   (string-match *cdmt-newc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog.")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-dirline 2)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The current entry should be ccccc.d")
		   (string-equal "ccccc.d" entry-name)))
    (should (progn (message "The archive buffer should be untouched. (1)")
		   (string-equal *cdmt-newc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (progn (message "The dired style buffer should be untouched. (1)")
		   (string-match *cdmt-newc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog.")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-dirline 4)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The current entry should be ggggg.d")
		   (string-equal "ggggg.d" entry-name)))
    (should (progn (message "The archive buffer should be untouched. (2)")
		   (string-equal *cdmt-newc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (progn (message "The dired style buffer shouold be untouched (2)")
		   (string-match *cdmt-newc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog.")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-dirline 8)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The current entry should be ooooo.d.")
		   (string-equal "ooooo.d" entry-name)))
    (should (progn (message "The archive buffer should be untouched. (3)")
		   (string-equal *cdmt-newc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (progn (message "The dired style buffer should be untouched. (3)")
		   (string-match *cdmt-newc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog.")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-dirline 16)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The current entry should be zzzzz.d.")
		   (string-equal "zzzzz.d" entry-name)))
    (should (progn (message "The archive buffer should be untouched. (4)")
		   (string-equal *cdmt-newc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (progn (message "The dired style buffer should be untouched. (4)")
		   (string-match *cdmt-newc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog.")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-dirline 1)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "The current entry should still be zzzzz.d.")
		   (string-equal "zzzzz.d" entry-name)))
    (should (progn (message "The archive buffer should be untouched. (5)")
		   (string-equal *cdmt-newc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (progn (message "The dired style buffer should be untouched. (5)")
		   (string-match *cdmt-newc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog.")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-next-line () ;✓
  "Test the function of M-x cpio-dired-next-line."
  (let ((test-name "cdmt-newc-cpio-dired-next-line")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(entry-name))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (string-equal "a" entry-name))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16526)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting and unchanged small archive. (16526)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17963)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-line 2)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (string-equal "aaa" entry-name))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16545)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting and unchanged small archive. (16544)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-line 4)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (string-equal "b" entry-name))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16563)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting and unchanged small archive. (16561)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17964)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-line 100)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (equal nil entry-name))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16582)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting and unchanged small archive. (16579)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17965)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-next-marked-entry ()
  "Test cpio-dired-next-marked-entry.
cpio-dired-next-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-next-marked-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-next-marked-entry ()
  "Test cpio-dired-next-marked-entry.
cpio-dired-next-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-next-marked-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-next-subdir ()
  "Test the function of M-x cpio-next-subdir."
  (should-error (cpio-dired-next-marked-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-prev-marked-entry ()
  "Test cpio-dired-prev-marked-entry.
cpio-dired-prev-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-prev-marked-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-prev-marked-entry ()
  "Test cpio-dired-prev-marked-entry.
cpio-dired-prev-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-prev-marked-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-prev-subdir ()
  "Test the function of M-x cpio-dired-prev-subdir."
  (should-error (cpio-dired-previous-line)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-previous-line () ;✓
  (let ((test-name "cdmt-newc-cpio-dired-previous-line")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(where))

    (cdmt-reset)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (goto-char (point-max))
	   (cpio-dired-previous-line 1)
	   (setq where (point))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (= where 1155))
    (should (progn (message "Expecting and unchanged small archive. (16642)")
		   (string-equal *cdmt-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16649)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17966)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-previous-line 2)
	   (setq where (point))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents 
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (= where 1019))
    (should (string-match *cdmt-small-archive* cpio-archive-buffer-contents))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16667)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17967)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-previous-line 4)
	   (setq where (point))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (= where 774))
    (should (progn (message "Expecting and unchanged small archive. (16677)")
		   (string-equal *cdmt-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (16686)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17968)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-previous-line ()
  "Test cpio-dired-previous-line.
cpio-dired-previous-line is not yet implemented -- expect an error."
  (should-error (cpio-dired-previous-line)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-previous-line ()
  "Test cpio-dired-previous-line.
cpio-dired-previous-line is not yet implemented -- expect an error."
  (should-error (cpio-dired-previous-line)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-show-entry-type ()
  "Test cpio-dired-show-entry-type.
cpio-dired-show-entry-type is not yet implemented -- expect an error."
  (should-error (cpio-dired-show-entry-type)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-sort-toggle-or-edit ()
  "Test cpio-dired-sort-toggle-or-edit.
cpio-dired-sort-toggle-or-edit is not yet implemented -- expect an error."
  (should-error (cpio-dired-sort-toggle-or-edit)
		:type 'error))

;; I'm not sure how to test this.
;; (ert-deftest cdmt-newc-cpio-dired-summary () ;✓
;;   "Test the function of M-x cpio-dired-summary."
;;   (shell-command "cd test_data/alphabet ; make newc" nil nil)
;;   (let ((test-name "cdmt-newc-cpio-dired-summary")
;;         (cpio-archive-buffer (find-file-noselect  *cdmt-small-archive*))
;;         (cpio-archive-buffer-contents)
;;         (cpio-dired-buffer)
;;         (cpio-dired-buffer-contents)
;;         )
;;     (with-current-buffer cpio-archive-buffer
;;       (cpio-mode))
;;     (setq cpio-dired-buffer (get-buffer-create (cpio-dired-buffer-name *cdmt-small-archive*)))
;; 
;;     (should (string-equal (with-output-to-string
;; 			    (cpio-dired-summary))
;; 
;;     ))

(ert-deftest cdmt-newc-cpio-dired-toggle-marks ()
  "Test cpio-dired-toggle-marks.
cpio-dired-toggle-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-marks)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-toggle-marks ()
  "Test cpio-dired-toggle-marks.
cpio-dired-toggle-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-marks)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-toggle-read-only ()
  "Test cpio-dired-toggle-read-only.
cpio-dired-toggle-read-only is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-read-only)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-toggle-read-only ()
  "Test cpio-dired-toggle-read-only.
cpio-dired-toggle-read-only is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-read-only)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-undo ()
  "Test cpio-dired-undo.
cpio-dired-undo is not yet implemented -- expect an error."
  (should-error (cpio-dired-undo)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-unmark () ;✓
  "Test the function of M-x cpio-dired-unmark."
  (let ((test-name "cdmt-newc-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-unmark 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an untouched small archive.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with every entry except the first marked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17969)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-next-line 2)
	   (cpio-dired-unmark 2)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an untouched small archive.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecing a dired bugger with all but two entries marked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17970)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 4)
	   (cpio-dired-unmark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an untouched small archive.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with another 4 entries unmarked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17971)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (dired-next-line 4)
	   (cpio-dired-unmark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an untouched archive.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with yet the last entry unmarked.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17972)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-unmark-all-entries ()
  "Test cpio-dired-unmark-all-entries."
  (let ((test-name "cdmt-newc-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-move-to-first-entry)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?A)
	   (cpio-dired-mark-this-entry ?B) (cpio-dired-mark-this-entry ?B)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E)
	   (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a variety of marks in a dired-style buffer.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
A -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
B -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
B -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
F drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17973)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-unmark-all-entries "" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with no marks.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17974)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-move-to-first-entry)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?A)
	   (cpio-dired-mark-this-entry ?B) (cpio-dired-mark-this-entry ?B)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E)
	   (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a variety of marks in a dired-style buffer.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
A -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
B -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
B -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
F drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    
    (should (progn (message "Expecting an unchanged catalog. (17975)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-unmark-all-entries "B" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with no B marks.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
A -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
F drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    
    (should (progn (message "Expecting an unchanged catalog. (17976)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-unmark-all-entries "F" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with neither B nor F marks.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
A -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17977)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-unmark-all-marks ()
  "Test cpio-dired-unmark-all-marks."
  (let ((test-name "cdmt-newc-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-move-to-first-entry)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?A)
	   (cpio-dired-mark-this-entry ?B) (cpio-dired-mark-this-entry ?B)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E)
	   (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a variety of marks in a dired-style buffer.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
A -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
B -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:unmark]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
B -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
E -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
F drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
F -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
\\* -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
\\* drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17978)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-unmark-all-marks)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting an unchanged archive buffer.")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting a dired-style buffer with no marks.")
		   (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting an unchanged catalog. (17979)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-unmark-all-marks () ;✓
  "Test the function of M-x cpio-unmark-all-marks."
  (let ((test-name "cdmt-newc-cpio-dired-unmark-all-marks")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-mark 2)
	   (cpio-dired-next-line 2)
	   (let ((cpio-dired-marker-char cpio-dired-del-marker))
	     (cpio-dired-mark 4))
	   (let ((cpio-dired-marker-char cpio-dired-keep-marker-copy-str))
	     (cpio-dired-mark 8))
	   (let ((cpio-dired-marker-char cpio-dired-keep-marker-rename))
	     (cpio-dired-mark 16))
	   (cpio-dired-unmark-all-marks)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting and unchanged small archive. (17253)")
		   (string-equal *cdmt-newc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (progn (message "Expecting an untouched cpio-dired buffer. (17263)")
		   (string-match *cdmt-newc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (progn (message "Expecting an unchanged catalog. (17980)")
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-newc-cpio-dired-unmark-backward ()
  "Test cpio-dired-unmark-backward.
cpio-dired-unmark-backward is not yet implemented -- expect an error."
  (should-error (cpio-dired-unmark-backward)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-unmark-backward ()
  "Test cpio-dired-unmark-backward.
cpio-dired-unmark-backward is not yet implemented -- expect an error."
  (should-error (cpio-dired-unmark-backward)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-up-directory ()
  "Test cpio-dired-up-directory.
cpio-dired-up-directory is not yet implemented -- expect an error."
  (should-error (cpio-dired-up-directory)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-upcase ()
  "Test cpio-dired-upcase.
cpio-dired-upcase is not yet implemented -- expect an error."
  (should-error (cpio-dired-upcase)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-view-archive () ;✓
  "Test the function of M-x cpio-view-archive."
  (let ((test-name "cdmt-newc-cpio-dired-view-archive")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(cpio-archive-window)
	(cpio-dired-window))

    (cdmt-reset 'make)
    
    (setq cpio-dired-window (get-buffer-window (get-buffer cpio-dired-buffer)))
    (should (window-live-p cpio-dired-window))
    (setq cpio-archive-window (get-buffer-window (get-buffer cpio-archive-buffer)))
    ;; (should (not (window-live-p cpio-dired-window)))
    (should (eq nil cpio-archive-window))
    
    (cpio-dired-view-archive)
    
    (setq cpio-dired-window (get-buffer-window (get-buffer cpio-dired-buffer)))
    ;; (should (not (window-live-p cpio-dired-window)))
    (should (eq nil cpio-dired-window))
    (setq cpio-archive-window (get-buffer-window (get-buffer cpio-archive-buffer)))
    (should (window-live-p cpio-archive-window))
    
    (cpio-view-dired-style-buffer)
    
    (setq cpio-dired-window (get-buffer-window (get-buffer cpio-dired-buffer)))
    (should (window-live-p cpio-dired-window))
    (setq cpio-archive-window (get-buffer-window (get-buffer cpio-archive-buffer)))
    ;; (should (not (window-live-p cpio-archive-window)))
    (should (eq nil cpio-archive-window))))

(ert-deftest cdmt-newc-cpio-dired-view-entry ()
  "Test cpio-dired-view-entry.
cpio-dired-view-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-view-entry)
		:type 'error))

(ert-deftest cdmt-newc-cpio-epa-dired-do-decrypt ()
  "Test cpio-epa-dired-do-decrypt.
cpio-epa-dired-do-decrypt is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-decrypt)
		:type 'error))

(ert-deftest cdmt-newc-cpio-epa-dired-do-encrypt ()
  "Test cpio-epa-dired-do-encrypt.
cpio-epa-dired-do-encrypt is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-encrypt)
		:type 'error))

(ert-deftest cdmt-newc-cpio-epa-dired-do-sign ()
  "Test cpio-epa-dired-do-sign.
cpio-epa-dired-do-sign is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-sign)
		:type 'error))

(ert-deftest cdmt-newc-cpio-epa-dired-do-verify ()
  "Test cpio-epa-dired-do-verify.
cpio-epa-dired-do-verify is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-verify)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-delete-tag ()
  "Test cpio-image-dired-delete-tag.
cpio-image-dired-delete-tag is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-delete-tag)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-dired-comment-entries ()
  "Test cpio-image-dired-dired-comment-entries.
cpio-image-dired-dired-comment-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-comment-entries)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-dired-display-external ()
  "Test cpio-image-dired-dired-display-external.
cpio-image-dired-dired-display-external is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-display-external)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-dired-display-image ()
  "Test cpio-image-dired-dired-display-image.
cpio-image-dired-dired-display-image is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-display-image)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-dired-edit-comment-and-tags ()
  "Test cpio-image-dired-dired-edit-comment-and-tags.
cpio-image-dired-dired-edit-comment-and-tags is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-edit-comment-and-tags)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-dired-toggle-marked-thumbs ()
  "Test cpio-image-dired-dired-toggle-marked-thumbs.
cpio-image-dired-dired-toggle-marked-thumbs is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-toggle-marked-thumbs)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-display-thumb ()
  "Test cpio-image-dired-display-thumb.
cpio-image-dired-display-thumb is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumb)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-display-thumbs ()
  "Test cpio-image-dired-display-thumbs.
cpio-image-dired-display-thumbs is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumbs)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-display-thumbs-append ()
  "Test cpio-image-dired-display-thumbs-append.
cpio-image-dired-display-thumbs-append is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumbs-append)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-jump-thumbnail-buffer ()
  "Test cpio-image-dired-jump-thumbnail-buffer.
cpio-image-dired-jump-thumbnail-buffer is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-jump-thumbnail-buffer)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-mark-tagged-entries ()
  "Test cpio-image-dired-mark-tagged-entries.
cpio-image-dired-mark-tagged-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-mark-tagged-entries)
		:type 'error))

(ert-deftest cdmt-newc-cpio-image-dired-tag-entries ()
  "Test cpio-image-dired-tag-entries.
cpio-image-dired-tag-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-tag-entries)
		:type 'error))

;;;;;;;; (ert-deftest cdmt-newc-cpio-quit-window () ;✓
;;;;;;;;   "Test cpio-quit-window.
;;;;;;;; cpio-quit-window is not yet implemented -- expect an error."
;;;;;;;;   (let ((test-name "cdmt-newc-cpio-dired-quit-window")
;;;;;;;;         (cpio-archive-buffer)
;;;;;;;;         (cpio-archive-buffer-contents)
;;;;;;;;         (cpio-dired-buffer)
;;;;;;;;         (cpio-dired-buffer-contents)
;;;;;;;; 	(cpio-archive-window)
;;;;;;;; 	(cpio-dired-window)
;;;;;;;;         )
;;;;;;;;     (cdmt-reset 'make)

;;;;;;;;     (setq cpio-dired-window (get-buffer-window (get-buffer cpio-dired-buffer)))
;;;;;;;;     (should (window-live-p cpio-dired-window))
;;;;;;;;     (setq cpio-archive-window (get-buffer-window (get-buffer cpio-archive-buffer)))
;;;;;;;;     ;; (should (not (window-live-p cpio-dired-window)))
;;;;;;;;     (should (eq nil cpio-archive-window))

;;;;;;;; This causes an error under ERT.
;;;;;;;;     (cpio-quit-window)

;;;;;;;;     (setq cpio-dired-window (get-buffer-window (get-buffer cpio-dired-buffer)))
;;;;;;;;     (should (eq nil cpio-dired-window))
;;;;;;;;     (setq cpio-archive-window (get-buffer-window (get-buffer cpio-archive-buffer)))
;;;;;;;;     (should (eq nil cpio-archive-window))))

(ert-deftest cdmt-newc-revert-buffer ()
  "Test revert-buffer.
revert-buffer is not yet implemented -- expect an error."
  (should-error (revert-buffer)
		:type 'error))

(ert-deftest cdmt-newc-cpio-dired-create-directory ()
  "Test cpio-dired-create-directory."
  (let ((test-name "cdmt-newc-cpio-dired-view-archive")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(cpio-archive-window)
	(cpio-dired-window))
    
    (cdmt-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (cdmt-tidy-up-catalog))
	   (cpio-dired-create-directory "newDirectory")
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (cdmt-tidy-up-catalog)))
    
    (should (progn (message "Expecting a cpio archive with newDirectory, a new directory.")
		   (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
a	(( filename ))

a

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
b	(( filename ))

b

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000004	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000002	(( namesize ))
00000000	(( chksum   ))
c	(( filename ))

c

070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000005	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000003	(( namesize ))
00000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000006	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000004	(( namesize ))
00000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000007	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000005	(( namesize ))
00000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000008	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000006	(( namesize ))
00000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000002	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
00000008	(( namesize ))
00000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070701	(( magic    ))
DEADBEEF	(( ino      ))
000041ED	(( mode     ))
000003E8	(( uid      ))
000003E8	(( gid      ))
00000001	(( nlink    ))
DEADBEEF	(( mtime    ))
00000000	(( filesize ))
DEADBEEF	(( dev maj  ))
DEADBEEF	(( dev min  ))
DEADBEEF	(( rdev maj ))
DEADBEEF	(( rdev min ))
0000000D	(( namesize ))
00000000	(( chksum   ))
newDirectory	(( filename ))
\\0
" cpio-archive-buffer-contents)))

    (should (progn (message "Expecting a cpio dired buffer with newDirectory, a new directory.")
		   (string-match  "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
  drwxr-xr-x   1  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory
" cpio-dired-buffer-contents)))

    (should (progn (message "Expecting a catalog with a new directory called »newDirectory«.")
		   (string-match "((¨newDirectory¨ .
\\s-+»»1 16877 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 13 0 ¨newDirectory¨«
\\s-+#<marker at 2197 in alphabet_small.newc.cpio> #<marker at 2321 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨a¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨a¨«
\\s-+#<marker at 1 in alphabet_small.newc.cpio> #<marker at 113 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨aa¨«
\\s-+#<marker at 117 in alphabet_small.newc.cpio> #<marker at 233 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨aaa¨«
\\s-+#<marker at 241 in alphabet_small.newc.cpio> #<marker at 357 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaaa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨aaaa¨«
\\s-+#<marker at 365 in alphabet_small.newc.cpio> #<marker at 481 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨aaaaa¨«
\\s-+#<marker at 489 in alphabet_small.newc.cpio> #<marker at 605 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨aaaaa.d¨ .
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨aaaaa.d¨«
\\s-+#<marker at 613 in alphabet_small.newc.cpio> #<marker at 733 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨b¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨b¨«
\\s-+#<marker at 733 in alphabet_small.newc.cpio> #<marker at 845 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨bb¨«
\\s-+#<marker at 849 in alphabet_small.newc.cpio> #<marker at 965 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨bbb¨«
\\s-+#<marker at 973 in alphabet_small.newc.cpio> #<marker at 1089 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbbb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨bbbb¨«
\\s-+#<marker at 1097 in alphabet_small.newc.cpio> #<marker at 1213 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨bbbbb¨«
\\s-+#<marker at 1221 in alphabet_small.newc.cpio> #<marker at 1337 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨bbbbb.d¨ .
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨bbbbb.d¨«
\\s-+#<marker at 1345 in alphabet_small.newc.cpio> #<marker at 1465 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨c¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 ¨c¨«
\\s-+#<marker at 1465 in alphabet_small.newc.cpio> #<marker at 1577 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨cc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 ¨cc¨«
\\s-+#<marker at 1581 in alphabet_small.newc.cpio> #<marker at 1697 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨ccc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 ¨ccc¨«
\\s-+#<marker at 1705 in alphabet_small.newc.cpio> #<marker at 1821 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨cccc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 ¨cccc¨«
\\s-+#<marker at 1829 in alphabet_small.newc.cpio> #<marker at 1945 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc¨ .
\\s-+»»[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 ¨ccccc¨«
\\s-+#<marker at 1953 in alphabet_small.newc.cpio> #<marker at 2069 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«)
 (¨ccccc.d¨ .
\\s-+»»[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
\\s-+([[:digit:]]+ [[:digit:]]+)
\\s-+0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 ¨ccccc.d¨«
\\s-+#<marker at 2077 in alphabet_small.newc.cpio> #<marker at 2197 in alphabet_small.newc.cpio> cpio-mode-entry-unmodified«))
" cpio-catalog-contents-after)))
    
    (cdmt-test-save *cdmt-archive-format*)))


;;
;; Run tests
;;

(ert "^cdmt-newc-cpio-")

;;; cpio-dired-test.el ends here.
