;; -*- coding: utf-8 -*-
;;; cpio-dired-test.el --- Tests of cpio-dired-mode.
;	$Id: cpio-dired-test.el,v 1.1.2.27 2018/04/26 12:54:56 doug Exp $	

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

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 
(load (concat default-directory "cpio.el"))


;; 
;; Vars
;; 

(defvar *cdmt-small-archive* "test_data/alphabet/alphabet_small.newc.cpio"
  "A small archive used for testing.")
(setq *cdmt-small-archive* "test_data/alphabet/alphabet_small.newc.cpio")
(defvar *cdmt-large-archive* "test_data/alphabet/alphabet.newc.cpio"
  "A large archive used for testing.")
(setq *cdmt-large-archive* "test_data/alphabet/alphabet.newc.cpio")

(defvar *cdmt-untouched-small-archive* "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0"
  "The contents of the untouched small archive.")
(setq *cdmt-untouched-small-archive* "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0")

(defvar *cdmt-untouched-small-dired-buffer* "CPIO archive: alphabet_small.newc.cpio:

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
(setq *cdmt-untouched-small-dired-buffer* "CPIO archive: alphabet_small.newc.cpio:

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


(defvar *cdmt-untouched-large-archive-buffer* "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0"
  "Contents of the untouched large cpio archive buffer.")
(setq *cdmt-untouched-large-archive-buffer* "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0")

(defvar *cdmt-untouched-large-dired-buffer* "CPIO archive: alphabet.newc.cpio:

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
(setq *cdmt-untouched-large-dired-buffer* "CPIO archive: alphabet.newc.cpio:

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
.+")



;; 
;; Library
;; 

(defun cdmt-reset (&optional make large)
  "Reset the current cpio-dired environment.
If MAKE is non-nil, then run 'make newc' as part of the reset."
  (let ((fname "cdmt-reset")
	(archive-name)
	(archive-names (list *cdmt-small-archive*
			     *cdmt-large-archive*)))
    ;; (error "%s() is not yet implemented" fname)
    (cd run-dir)
    (mapc (lambda (an)
	    (setq cpio-archive-buffer (find-file-noselect an))
	    (if (buffer-live-p (get-buffer cpio-archive-buffer))
		(with-current-buffer cpio-archive-buffer
		  (revert-buffer nil 'noconfirm))
	      (condition-case v
		  (kill-buffer cpio-dired-buffer)
		(error (message "%s(): no dired buffer to kill." fname)))))
	  archive-names)

    (cd run-dir)

    (if make
	(shell-command "cd test_data/alphabet ; make newc" nil nil))	

    (setq archive-name (if large 
			   *cdmt-large-archive*
			 *cdmt-small-archive*))

    (delete-other-windows)
    (with-current-buffer (setq cpio-archive-buffer (find-file-noselect archive-name))
      (if (string-match "/test_data/.+/test_data/" (buffer-file-name))
	  (error "Bogus archive!"))
      (cpio-mode))
    (setq cpio-dired-buffer (switch-to-buffer (cpio-dired-buffer-name archive-name)))
    ))

(defun cdmt-filter-archive-contents (archive-contents)
  "Make the given ARCHIVE-CONTENTS fully printable and readable."
  (let ((fname "cdmt-filter-archive-contents")
	(char-map (list (cons "\0" "\\0")))
	)
    ;; (error "%s() is not yet implemented" fname)
    (setq archive-contents (cdmt-reformat-newc-headers archive-contents))
    (mapc (lambda (cm)
	    (let ((from (car cm))
		  (to (cdr cm)))
	      (setq archive-contents (cdmt-global-sub from to archive-contents))))
	  char-map)
    archive-contents
    ))

(defun cdmt-reformat-newc-headers (archive-contents)
  "Reformat the cpio newc entry headers in the given ARCHIVE-CONTENTS
So that they are human readable.
CAVEATS: \(1\) If ARCHIVE-CONTENTS contains entries that contain entry headers,
then those will also be reformatted.
\(2\) The entry names are taken to be a sequence of printable characters.
So, if NULLs have been converted to printable characters,
then the entry names will be incorrect."
  (let ((fname "cdmt-reformat-newc-headers")
	)
    ;; (error "%s() is not yet implemented" fname)
    (while (string-match *cpio-newc-header-re* archive-contents)
      (setq archive-contents (concat (substring archive-contents 0 (match-beginning 0))
				     (concat (match-string *cpio-newc-magic-re-idx*    archive-contents) "\t(( magic    ))\n")
				     (concat "DEADBEEF"                                                  "\t(( ino      ))\n")
				     (concat (match-string *cpio-newc-mode-re-idx*     archive-contents) "\t(( mode     ))\n")
				     (concat (match-string *cpio-newc-uid-re-idx*      archive-contents) "\t(( uid      ))\n")
				     (concat (match-string *cpio-newc-gid-re-idx*      archive-contents) "\t(( gid      ))\n")
				     (concat (match-string *cpio-newc-nlink-re-idx*    archive-contents) "\t(( nlink    ))\n")
				     ;; Note that the mod time can change.
				     (concat "DEADBEEF"                                                  "\t(( mtime    ))\n")
				     (concat (match-string *cpio-newc-filesize-re-idx* archive-contents) "\t(( filesize ))\n")
				     (concat "DEADBEEF"                                                  "\t(( dev maj  ))\n")
				     (concat "DEADBEEF"                                                  "\t(( dev min  ))\n")
				     (concat "DEADBEEF"                                                  "\t(( rdev maj ))\n")
				     (concat "DEADBEEF"                                                  "\t(( rdev min ))\n")
				     (concat (match-string *cpio-newc-namesize-re-idx* archive-contents) "\t(( namesize ))\n")
				     (concat (match-string *cpio-newc-chksum-re-idx*   archive-contents) "\t(( chksum   ))\n")
				     (concat (match-string *cpio-newc-filename-re-idx* archive-contents) "\t(( filename ))\n")
				     (substring archive-contents (match-end 0)))))
    archive-contents))

(defun cdmt-global-sub (from-str to-str string)
  "Globally substitute TO-STR for FROM-STR in STRING and return the new string.
In principal, FROM-STR can be a regular expression."
  (let ((fname "cdmt-global-sub")
	)
    ;; (error "%s() is not yet implemented" fname)
    (while (string-match from-str string)
      (setq string (replace-match to-str nil t string 0)))
    string
    ))

;; 
;; Commands
;; 

;;
;; The contents of these tests were generated
;; from the dired-mode-map definition
;; (where commands are tagged as done).
;; A few keyboard macros modified those results.
;; The following commands were used to create the skeletons below

(defun cdmt-unfinished-command ()
  "Create a stub test for the next unfinished command."
  (interactive)
  (let ((fname "cdmt-unfinished-command")
	(test-declaration-string "cdmt-")
	(defined-command-regexp ".+) ;✓$")
	(command-name)
	(where)
	)
    ;; (error "%s() is not yet implemented" fname)
    (cond ((catch 'found-it
	     (while (search-forward test-declaration-string (point-max) t)
	       (setq where (match-end 0))
	       (unless (looking-at-p defined-command-regexp)
		 (setq command-name (buffer-substring-no-properties where (1- (line-end-position))))
		 (throw 'found-it t)))
	     nil)
	   (goto-char (1- (line-end-position)))
	   (delete-char 1)
	   (insert         " ()\n")
	   (insert (format "  \"Test %s.\n" command-name))
	   (insert (format "%s is not yet implemented -- expect an error.\"\n" command-name))
	   (insert (format "  (should-error (%s)\n" command-name))
	   (insert         "     :type 'error))\n")
	   t)
	  (t nil))))

(defun cdmt-all-unfinished-commands ()
  "Write stub tests for all unfinished commands following point."
  (interactive)
  (let ((fname "cdmt-all-unfinished-commands")
	)
    ;; (error "%s() is not yet implemented" fname)
    (while (cdmt-unfinished-command))))

(defun cdmt-finished-command ()
  "Write a stub test for a finished command."
  (interactive)
  (let ((fname "cdmt-finished-command")
	(finished-command-regexp ") ;✓$")
	(command-name)
	)
    (cond ((re-search-forward finished-command-regexp (point-max) t)
	   (beginning-of-line)
	   (re-search-forward "cdmt-" (line-end-position))
	   (setq where (match-end 0))
	   (end-of-line)
	   (backward-char 4)
	   (setq command-name (buffer-substring-no-properties where (point)))

	   (insert " (")
	   (end-of-line)
	   (insert "\n")
	   (insert (format "  \"Test the function M-x cpio-%s.\"\n" command-name))
	   (insert         "  (shell-command \"cd test_data/alphabet ; make newc\" nil nil)\n")
	   (insert (format "  (let ((test-name \"cdmt-%s\")\n" command-name))
	   (insert         "        (cpio-archive-buffer)\n")
	   (insert         "        (cpio-archive-buffer-contents)\n")
	   (insert         "        (cpio-dired-buffer)\n")
	   (insert         "        (cpio-dired-buffer-contents)\n")
	   (insert         "        )\n")
	   (insert         "    (cdmt-reset)\n")
	   (insert         "\n")
	   (insert (format "    (%s)\n" command-name))
	   (insert         "PREPARATION\n")
	   (insert         "\n")
	   (insert         "    (setq cpio-archive-buffer-contents\n")
	   (insert         "          (cdmt-filter-archive-contents\n")
	   (insert         "            (with-current-buffer cpio-archive-buffer\n")
	   (insert         "              (buffer-substring-no-properties (point-min) (point-max))))\n")
	   (insert         "    (should (string-match \"\" cpio-archive-buffer-contents))\n")
	   (insert         "    (setq cpio-dired-buffer-contents\n")
	   (insert         "          (with-current-buffer cpio-dired-buffer\n")
	   (insert         "            (buffer-substring-no-properties (point-min) (point-max))))\n")
	   (insert         "    (should (string-equal cpio-dired-buffer-contents \"\"))\n")
	   (insert         "\n")
	   (insert         "    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.\n")
	   (insert         "    ))\n")
	   t)
	  (t nil))
    ))

(defun cdmt-all-finished-commands ()
  "Build stub tests for all the finished commands."
  (interactive)
  (let ((fname "cdmt-all-finished-commands")
	)
    ;; (error "%s() is not yet implemented" fname)
    (while (cdmt-finished-command))
    ))

(defun cdmt-ediff-results ()
  "Compare the results of a (string-match) on the current line in a buffer of ERT results.
The results are always presented as (string-match EXPECTED-RE ACTUAL) on that line."
  (interactive)
  (let ((fname "cdmt-ediff-results")
	(expected)
	(actual)
	(start)
	)
    ;; (error "%s() is not yet implemented" fname)
    (beginning-of-line)
    (re-search-forward "string-\\(match\\|equal\\) " (line-end-position))

    (setq start (1+ (point)))
    (forward-sexp)
    (setq expected (buffer-substring-no-properties start (1- (point))))

    (forward-char 1)

    (setq start (1+ (point)))
    (forward-sexp)
    (setq actual  (buffer-substring-no-properties start (1- (point))))
    
    (with-current-buffer (get-buffer-create "expected")
      (erase-buffer)
      (insert expected)
      (goto-char (point-min))
      (while (search-forward "\\n" (point-max) t)
	(replace-match "\n\n"))
      (goto-char (point-min)))
    
    (with-current-buffer (get-buffer-create "actual")
      (erase-buffer)
      (insert actual)
      (goto-char (point-min))
      (while (search-forward "\\n" (point-max) t)
	(replace-match "\n\n"))
      (goto-char (point-min)))
    
    ;; (pop-to-buffer "expected")
    ;; (switch-to-buffer "actual")

    (ediff-buffers "actual" "expected")
    ))


(defun cdmt-do-cpio-id (where archive)
  "Run cpio(1) on the given ARCHIVE and report the results.
WHERE Should be a line number."
  (let* ((fname "cdmt-do-cpio-id")
	 (buf-name "*cpio output*")
	 (buf (get-buffer buf-name))
	 (make-directory "qwerqwer"))
    ;; (error "%s() is not yet implemented" fname)
    (if (file-exists-p "qwerqwer")
	(should (and (format "remove didn't work (line %d)" where)
		     (call-process "rm" nil nil nil "-rf" "qwerqwer"))))
    (make-directory "qwerqwer")
    (should (and (format "cd didn't work (line %d)" where)
		 (cd "qwerqwer")))
    (if (buffer-live-p buf)
	(with-current-buffer buf-name (erase-buffer)))
    (should (and (format "cpio on saved archive failed (line %d)." where)
		 (= 0 (call-process "cpio" (concat "../" archive) buf nil "-id"))))
    (should (and "Checking cpio output."
		 (with-current-buffer buf-name
		   (string-match "[[:digit:]]+ blocks\n" (buffer-substring-no-properties (point-min) (point-max))))))
    (kill-buffer buf)
    (cd "..")))


;; 
;; Tests
;; 

;; N.B. cdmt- = cpio-dired-mode-test-

(defvar run-dir default-directory)

(custom-set-variables (list 'cpio-try-names nil))

;; All tests use M-x cpio-dired-kill.
(ert-deftest cdmt-cpio-dired-kill () ;✓
  "Test the function of M-x cpio-dired-kill."
  (let ((test-name "cdmt-cpio-dired-kill")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	)
    (cdmt-reset 'make)

    (cpio-dired-kill)

    (should (and "Dired style buffer should not be live."
		 (not (buffer-live-p cpio-dired-buffer))))
    (should (and "Archive buffer should not be live."
		 (not (buffer-live-p cpio-archive-buffer))))
    ))

(ert-deftest cdmt-cpio-dired-do-isearch ()
  "Test cpio-dired-do-isearch.
cpio-dired-do-isearch is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-isearch)
		:type 'error))

(ert-deftest cdmt-cpio-dired-do-isearch-regexp ()
  "Test cpio-dired-do-isearch-regexp.
cpio-dired-do-isearch-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-isearch-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-isearch-entry-names ()
  "Test cpio-dired-isearch-entry-names.
cpio-dired-isearch-entry-names is not yet implemented -- expect an error."
  (should-error (cpio-dired-isearch-entry-names)
     :type 'error))

(ert-deftest cdmt-cpio-dired-isearch-entry-names-regexp ()
  "Test cpio-dired-isearch-entry-names-regexp.
cpio-dired-isearch-entry-names-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-isearch-entry-names-regexp)
     :type 'error))

;;;;;;;; This gets an end-of-buffer error under ERT.
;;;;;;;; (ert-deftest cdmt-cpio-dired-save-archive-0 () ;✓
;;;;;;;;   "Test the function of M-x cpio-dired-save-archive."
;;;;;;;;   (let ((test-name "cdmt-cpio-dired-save-archive")
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
    
;;;;;;;;     (should (and "Archive buffer should be modified."
;;;;;;;; 		 (buffer-modified-p cpio-archive-buffer)))
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
    
;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min)(point)) (file-name-nondirectory *cdmt-small-archive*))
    
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

;;;;;;;;     (should (and "The dired style buffer should not have changed."
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

;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))
    
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

;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

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

;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

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

;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

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

;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

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

;;;;;;;;     ;; (cdmt-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-small-archive*))

;;;;;;;;     (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
;;;;;;;;     ))

(ert-deftest cdmt-cpio-describe-mode ()
  "Test cpio-describe-mode.
cpio-describe-mode is not yet implemented -- expect an error."
  (should-error (cpio-describe-mode)
     :type 'error))

(ert-deftest cdmt-cpio-dired-add-entry ()
  "Test cpio-dired-add-entry.
cpio-dired-add-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-add-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-change-marks ()
  "Test cpio-dired-change-marks.
cpio-dired-change-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-change-marks)
     :type 'error))

(ert-deftest cdmt-cpio-dired-clean-directory ()
  "Test cpio-dired-clean-directory.
cpio-dired-clean-directory is not yet implemented -- expect an error."
  (should-error (cpio-dired-clean-directory)
     :type 'error))

(ert-deftest cdmt-cpio-dired-copy-entry-name-as-kill ()
  "Test cpio-dired-copy-entry-name-as-kill.
cpio-dired-copy-entry-name-as-kill is not yet implemented -- expect an error."
  (should-error (cpio-dired-copy-entry-name-as-kill)
     :type 'error))

(ert-deftest cdmt-cpio-dired-diff ()
  "Test cpio-dired-diff) ;.
cpio-dired-diff) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-diff) ;)
     :type 'error))

(ert-deftest cdmt-cpio-dired-display-entry () ;✓
  "Test the function of M-x cpio-dired-display-entry."
  (let ((test-name "cdmt-cpio-dired-display-entry")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-contents-buffer-name)
	(cpio-contents-buffer)
	(cpio-contents-buffer-string)
	(cpio-contents-window)
	(entry-name)
        )
    (cdmt-reset 'make)

    (set-buffer (setq cpio-dired-buffer (get-buffer (cpio-dired-buffer-name *cdmt-small-archive*))))

    (progn (setq entry-name "aaa")
	   (goto-char (point-min))
	   (re-search-forward " aaa$" (point-max))
	   (cpio-dired-display-entry)
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Viewing an entry should not change the archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Viewing an entry should not change the dired-style buffer."
		 (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "The contents buffer should not be null."
		 (not (null cpio-contents-buffer))))
    (should (and "The contents buffer should be live."
		 (buffer-live-p cpio-contents-buffer)))
    (should (and "Check the entry's contents buffer."
		 (string-equal cpio-contents-buffer-string "\naaa\n\n")))
    (should (and "The entry's contents' window should be live."
		 (window-live-p cpio-contents-window)))

    (cdmt-reset)
    
    (progn (setq entry-name "ccc")
	   (goto-char (point-min))
	   (re-search-forward " ccc$" (point-max))
	   (cpio-dired-display-entry)
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Checking the archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Checking the dired-style buffer."
		 (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
    (should (not (null cpio-contents-buffer)))
    (should (buffer-live-p cpio-contents-buffer))
    (should (string-equal cpio-contents-buffer-string "\nccc\n\n"))
    (should (window-live-p cpio-contents-window))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-async-shell-command ()
  "Test cpio-dired-do-async-shell-command) ;.
cpio-dired-do-async-shell-command) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-async-shell-command)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-chgrp () ;✓
  "Test the function of M-x cpio-dired-do-chgrp."
  (let ((test-name "cdmt-cpio-dired-do-chgrp")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (dired-next-line *cpio-dired-head-offset*)
	   (setq unread-command-events (listify-key-sequence "9999\n"))
	   (cpio-dired-do-chgrp 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000004	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000002	(( namesize ))\n00000000	(( chksum   ))\na	(( filename ))\n\na\n\n070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000005	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000003	(( namesize ))\n00000000	(( chksum   ))\naa	(( filename ))\n\\0\\0\\0\naa\n\n\\0\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n0000270F	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000006	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000004	(( namesize ))\n00000000	(( chksum   ))\naaa	(( filename ))\n\\0\\0\naaa\n\n\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000007	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000005	(( namesize ))\n00000000	(( chksum   ))\naaaa	(( filename ))\n\\0\naaaa\n\n\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000008	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000006	(( namesize ))\n00000000	(( chksum   ))\naaaaa	(( filename ))\n\naaaaa\n\n070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000041ED	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000002	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000000	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000008	(( namesize ))\n00000000	(( chksum   ))\naaaaa.d	(( filename ))\n\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000004	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000002	(( namesize ))\n00000000	(( chksum   ))\nb	(( filename ))\n\nb\n\n070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000005	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000003	(( namesize ))\n00000000	(( chksum   ))\nbb	(( filename ))\n\\0\\0\\0\nbb\n\n\\0\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000006	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000004	(( namesize ))\n00000000	(( chksum   ))\nbbb	(( filename ))\n\\0\\0\nbbb\n\n\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000007	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000005	(( namesize ))\n00000000	(( chksum   ))\nbbbb	(( filename ))\n\\0\nbbbb\n\n\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000008	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000006	(( namesize ))\n00000000	(( chksum   ))\nbbbbb	(( filename ))\n\nbbbbb\n\n070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000041ED	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000002	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000000	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000008	(( namesize ))\n00000000	(( chksum   ))\nbbbbb.d	(( filename ))\n\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000004	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000002	(( namesize ))\n00000000	(( chksum   ))\nc	(( filename ))\n\nc\n\n070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000005	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000003	(( namesize ))\n00000000	(( chksum   ))\ncc	(( filename ))\n\\0\\0\\0\ncc\n\n\\0\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000006	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000004	(( namesize ))\n00000000	(( chksum   ))\nccc	(( filename ))\n\\0\\0\nccc\n\n\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000007	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000005	(( namesize ))\n00000000	(( chksum   ))\ncccc	(( filename ))\n\\0\ncccc\n\n\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000081A4	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000008	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000006	(( namesize ))\n00000000	(( chksum   ))\nccccc	(( filename ))\n\nccccc\n\n070701	(( magic    ))\nDEADBEEF	(( ino      ))\n000041ED	(( mode     ))\n000003E8	(( uid      ))\n000003E8	(( gid      ))\n00000002	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000000	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n00000008	(( namesize ))\n00000000	(( chksum   ))\nccccc.d	(( filename ))\n\\0\\0070701	(( magic    ))\nDEADBEEF	(( ino      ))\n00000000	(( mode     ))\n00000000	(( uid      ))\n00000000	(( gid      ))\n00000001	(( nlink    ))\nDEADBEEF	(( mtime    ))\n00000000	(( filesize ))\nDEADBEEF	(( dev maj  ))\nDEADBEEF	(( dev min  ))\nDEADBEEF	(( rdev maj ))\nDEADBEEF	(( rdev min ))\n0000000B	(( namesize ))\n00000000	(( chksum   ))\nTRAILER!!!	(( filename ))\n\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  9999        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents))

    (cdmt-reset)
    
    (progn (setq unread-command-events (listify-key-sequence "8888\n"))
	   (cpio-dired-do-chgrp 4))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
000003E8	(( uid      ))
000022B8	(( gid      ))
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
000022B8	(( gid      ))
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
000022B8	(( gid      ))
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
000022B8	(( gid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  8888        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  8888        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  8888        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  8888        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents))

    (cdmt-reset)

    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777\n"))
	   (cpio-dired-do-chgrp 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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
00001E61	(( gid      ))
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
00001E61	(( gid      ))
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
00001E61	(( gid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:A\\(?:pr\\|ug\\)\\|Dec\\|Feb\\|J\\(?:an\\|u[ln]\\)\\|Ma[ry]\\|Nov\\|Oct\\|Sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-chmod ()
  "Test cpio-dired-do-chmod.
cpio-dired-do-chmod is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-chmod)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-chown () ;✓
  "Test the function of M-x cpio-do-chown."
  (let ((test-name "cdmt-cpio-dired-do-chown")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )

    (cdmt-reset 'make)

    (progn (setq unread-command-events (listify-key-sequence "9999\n"))
	   (cpio-dired-do-chown 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal cpio-archive-buffer-contents "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
0000270F	(( uid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0"))

    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (cdmt-reset)
    
    (progn (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "8888\n"))
	   (cpio-dired-do-chown 4))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal cpio-archive-buffer-contents "070701	(( magic    ))
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
000022B8	(( uid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0"))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  8888  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
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
" cpio-dired-buffer-contents))

    (cdmt-reset)

    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777\n"))
	   (cpio-dired-do-chown 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal cpio-archive-buffer-contents "070701	(( magic    ))
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
00001E61	(( uid      ))
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
00001E61	(( uid      ))
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
00001E61	(( uid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0"))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  7777  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  7777  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  7777  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-chown-1 ()
  "Test the change-owner-user function of M-x cpio-dired-do-chown."
  (let ((test-name "cdmt-cpio-dired-do-chown")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (setq unread-command-events (listify-key-sequence "9999:1111\n"))
	   (cpio-dired-do-chown 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
DEADBEEF	(( ino      ))
000081A4	(( mode     ))
0000270F	(( uid      ))
00000457	(( gid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))

    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (cdmt-reset)
    
    (progn (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "8888:2222\n"))
	   (cpio-dired-do-chown 4))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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
000022B8	(( uid      ))
000008AE	(( gid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (cdmt-reset)

    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777:3333\n"))
	   (cpio-dired-do-chown 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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
00001E61	(( uid      ))
00000D05	(( gid      ))
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
00001E61	(( uid      ))
00000D05	(( gid      ))
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
00001E61	(( uid      ))
00000D05	(( gid      ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  7777  3333        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  7777  3333        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  7777  3333        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-compress ()
  "Test cpio-dired-do-compress.
cpio-dired-do-compress is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-compress)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-copy-0 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )

    (cdmt-reset 'make)

    (progn (setq unread-command-events (listify-key-sequence "d\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max))))))
;;    (should (and "Checking that entry »a« has been copied to »d«." 
	(should	 (string-equal "070701	(( magic    ))
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

" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (and "Checking that there is an entry »d« in the dired style buffer."
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
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
" cpio-dired-buffer-contents)))
    ))

(ert-deftest cdmt-cpio-dired-do-copy-1 () ;✓
  "Test the function of M-x cpio-do-copy."
  :expected-result :failed
  (let ((test-name "cdmt-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )

    (cdmt-reset 'make)

    (progn (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
	   (cpio-dired-do-copy 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    ;; HEREHERE I think that this expected value is incorrect.
    ;; It implies an archive with two entries
    ;; whose padding does not end after a (mod ... *cpio-padding-modulus*) = 0
    ;; character position.
    ;; However, cpio(1GNU) seems to cope with it.
    ;; Note the use of the term character position, since cpio counts characters
    ;; starting at 0, while emacs starts counting (point) at 1.
    (should (and "Checking for »aaa«, »aaaa«, »aaaaa«, »aaaaa« copied to newDirectory in the archive."
		 (string-equal "" cpio-archive-buffer-contents)))

    (should (and "Checking for the presence of »newDirectory/aaa«, »newDirectory/aaaa«, »newDirectory/aaaaa«, »newDirectory/aaaaa«."
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
    ))

(ert-deftest cdmt-cpio-dired-do-copy-2 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )

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
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "Expecting an archive with each 3 letter entry copied to newDirectory-1."
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

\\0\\0" cpio-archive-buffer-contents)))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/aaa
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))
(ert-deftest cdmt-cpio-dired-do-copy-3 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )

    (cdmt-reset 'make)

    (progn (cpio-dired-mark-entries-regexp "...")
	   (setq unread-command-events (listify-key-sequence "newDirectory-3\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    ;; HEREHERE Once again, I think this archive has some incorrect
    ;; entry padding.
    (should (and "Expecting an archive with each entry named with at least 3 letters copied to newDirectory-3."
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

\\0\\0" cpio-archive-buffer-contents)))
    
    (should (and "Expecting all entries named with at least 3 letters to have copis in newDirectory-3."
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
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/ccccc.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/ccccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/ccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/bbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-3/aaa
" cpio-dired-buffer-contents)))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-copy-regexp ()
  "Test cpio-dired-do-copy-regexp.
cpio-dired-do-copy-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-copy-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-delete () ;✓
  "Test the function of M-x cpio-dired-do-delete."
  (let ((test-name "cdmt-cpio-dired-do-delete")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-do-delete 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))


    (progn (dired-next-line 2)
	   (cpio-dired-do-delete 4))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))


    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "\n"))
	   (cpio-dired-do-delete 1))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-flagged-delete ()
  "Test cpio-dired-do-flagged-delete.
cpio-dired-do-flagged-delete is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-flagged-delete)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-hardlink ()
  "Test cpio-dired-do-hardlink.
cpio-dired-do-hardlink is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-hardlink)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-hardlink-regexp ()
  "Test cpio-dired-do-hardlink-regexp.
cpio-dired-do-hardlink-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-hardlink-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-kill-lines ()
  "Test cpio-dired-do-kill-lines.
cpio-dired-do-kill-lines is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-kill-lines)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-print ()
  "Test cpio-dired-do-print.
cpio-dired-do-print is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-print)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-query-replace-regexp ()
  "Test cpio-dired-do-query-replace-regexp.
cpio-dired-do-query-replace-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-query-replace-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-redisplay ()
  "Test cpio-dired-do-redisplay.
cpio-dired-do-redisplay is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-redisplay)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-rename () ;✓
  (let ((test-name "cdmt-cpio-dired-do-rename")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)
    
    (progn (setq unread-command-events (listify-key-sequence "d\n"))
	   (cpio-dired-do-rename 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an archive with »a« renamed to »d«."
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

    (should (and "Expecting a dired buffer with no entry »a«, but an entry »d«."
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
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
" cpio-dired-buffer-contents)))

    (progn (cpio-dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
	   (cpio-dired-do-rename 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an archive with entries »aaaa«, »aaaaa«, »aaaaa.d« and »b« moved to »newDirectory«."
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
0000000F	(( namesize ))
00000000	(( chksum   ))
newDirectory/b	(( filename ))
\\0\\0\\0
b

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
00000015	(( namesize ))
00000000	(( chksum   ))
newDirectory/aaaaa.d	(( filename ))
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
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory/aaaaa	(( filename ))
\\0\\0\\0
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
00000012	(( namesize ))
00000000	(( chksum   ))
newDirectory/aaaa	(( filename ))

aaaa

\\0" cpio-archive-buffer-contents)))
    
    (should (and "Expecting a dired style buffer with entries »aaaa«, »aaaaa«, »aaaaa.d« and »b« moved to »newDirectory«."
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
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/b
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaa
" cpio-dired-buffer-contents)))

    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "newDirectory-1\n"))
	   (cpio-dired-do-rename 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an archive with entries named by 3 characters in newdirectory-1."
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
0000000F	(( namesize ))
00000000	(( chksum   ))
newDirectory/b	(( filename ))
\\0\\0\\0
b

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
00000015	(( namesize ))
00000000	(( chksum   ))
newDirectory/aaaaa.d	(( filename ))
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
00000013	(( namesize ))
00000000	(( chksum   ))
newDirectory/aaaaa	(( filename ))
\\0\\0\\0
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
00000012	(( namesize ))
00000000	(( chksum   ))
newDirectory/aaaa	(( filename ))

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

\\0\\0" cpio-archive-buffer-contents)))

    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/b
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory/aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} newDirectory-1/aaa
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-do-rename-regexp ()
  "Test cpio-dired-do-rename-regexp.
cpio-dired-do-rename-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-rename-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-search () ;HEREHERE ()
  "Test cpio-dired-do-search) ;HEREHERE.
cpio-dired-do-search) ;HEREHER is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-search)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-symlink ()
  "Test cpio-dired-do-symlink.
cpio-dired-do-symlink is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-symlink)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-symlink-regexp ()
  "Test cpio-dired-do-symlink-regexp.
cpio-dired-do-symlink-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-symlink-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-do-touch ()
  "Test cpio-dired-do-touch.
cpio-dired-do-touch is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-touch)
     :type 'error))

(ert-deftest cdmt-cpio-dired-downcase ()
  "Test cpio-dired-downcase.
cpio-dired-downcase is not yet implemented -- expect an error."
  (should-error (cpio-dired-downcase)
     :type 'error))

(ert-deftest cdmt-cpio-dired-extract-all ()
  "Test cpio-dired-extract-all.
cpio-dired-extract-all is not yet implemented -- expect an error."
  (should-error (cpio-dired-extract-all)
     :type 'error))

(ert-deftest cdmt-cpio-dired-extract-entries ()
  "Test cpio-dired-extract-entries.
cpio-dired-extract-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-extract-entries)
     :type 'error))

(ert-deftest cdmt-cpio-dired-find-alternate-entry ()
  "Test cpio-dired-find-alternate-entry.
cpio-dired-find-alternate-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-find-alternate-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-find-entry () ;✓
  "Test the function of M-x cpio-find-entry.
Expect errors about killed buffers.
They reflect an outstanding bug in cpio-affiliated buffers."

  (let ((test-name "cdmt-cpio-dired-find-entry")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-contents-window)
	(entry-name)
	(past-entries ())
        )
    (cdmt-reset 'make)
    
    (progn (setq entry-name "aaa")
	   (goto-char (point-min))
	   (re-search-forward (concat " " entry-name "$"))
	   (cpio-dired-display-entry)
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (not (null cpio-contents-buffer)))
    (should (buffer-live-p cpio-contents-buffer))
    (should (string-equal cpio-contents-buffer-string (concat "\n" entry-name "\n\n")))
    (should (window-live-p cpio-contents-window))

    (push entry-name past-entries)
    
    (switch-to-buffer cpio-dired-buffer)

    (progn (setq entry-name "ccc")
	   (goto-char (point-min))
	   (re-search-forward (concat " " entry-name "$"))
	   (cpio-dired-display-entry)
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (not (null cpio-contents-buffer)))
    (should (buffer-live-p cpio-contents-buffer))
    (should (string-equal cpio-contents-buffer-string "\nccc\n\n"))
    (should (window-live-p cpio-contents-window))

    ;; Now make sure that any past entries are still there.
    (mapc (lambda (en)
	   (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name entry-name)))
	   (setq cpio-contents-buffer-string (with-current-buffer cpio-contents-buffer
					       (buffer-substring-no-properties (point-min)
									       (point-max))))
	   (should (not (null cpio-contents-buffer)))
	   (should (buffer-live-p cpio-contents-buffer))
	   (should (string-equal cpio-contents-buffer-string (concat "\n" entry-name "\n\n")))
	   (should (window-live-p cpio-contents-window))
	   )
	  past-entries)

    ;; Affiliated buffers don't get killed when the parent does yet.
    (push entry-name past-entries)
    (mapc (lambda (en)
	    (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name en)))
	    (if (buffer-live-p cpio-contents-buffer)
		(kill-buffer cpio-contents-buffer)))
	  past-entries)

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-find-entry-other-window ()
  "Test cpio-dired-find-entry-other-window.
cpio-dired-find-entry-other-window is not yet implemented -- expect an error."
  (should-error (cpio-dired-find-entry-other-window)
     :type 'error))

(ert-deftest cdmt-cpio-dired-flag-auto-save-entries () ;✓
  "Test the function of M-x cpio-dired-flag-auto-save-entries."
  (let ((test-name "cdmt-cpio-dired-flag-auto-save-entries")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (beginning-of-line)
	   (while (re-search-forward " \\(.\\)$" (point-max) t)
	     (setq unread-command-events (listify-key-sequence (concat "#" (match-string-no-properties 1) "\n")))
	     (cpio-dired-do-copy 1))
	   (cpio-dired-flag-auto-save-entries))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal "070701	(( magic    ))
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

" cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-flag-backup-entries ()
  "Test cpio-dired-flag-backup-entries.
cpio-dired-flag-backup-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-backup-entries)
     :type 'error))

(ert-deftest cdmt-cpio-dired-flag-entries-regexp ()
  "Test cpio-dired-flag-entries-regexp.
cpio-dired-flag-entries-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-entries-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-flag-entry-deletion () ;✓
  "Test the function of M-x cpio-flag-entry-deletion."
  (let ((test-name "cdmt-cpio-dired-flag-entry-deletion")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-flag-entry-deletion 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))


    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (progn (dired-next-line 2)
	   (cpio-dired-flag-entry-deletion 4))

    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-flag-garbage-entries ()
  "Test cpio-dired-flag-garbage-entries.
cpio-dired-flag-garbage-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-garbage-entries)
     :type 'error))

(ert-deftest cdmt-cpio-dired-goto-entry ()
  "Test cpio-dired-goto-entry.
cpio-dired-goto-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-goto-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-hide-all ()
  "Test cpio-dired-hide-all.
cpio-dired-hide-all is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-all)
     :type 'error))

(ert-deftest cdmt-cpio-dired-hide-details-mode ()
  "Test cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing.
cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing)
     :type 'error))

(ert-deftest cdmt-cpio-dired-hide-subdir ()
  "Test cpio-dired-hide-subdir) ;.
cpio-dired-hide-subdir) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-subdir)
     :type 'error))

(ert-deftest cdmt-cpio-dired-mark () ;✓
  "Test the function of M-x cpio-dired-mark."
  (let ((test-name "cdmt-cpio-dired-mark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-mark 1)
    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (progn (dired-next-line 2)
	   (cpio-dired-mark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal cpio-archive-buffer-contents *cdmt-untouched-small-archive*))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-mark-directories ()
  "Test cpio-dired-mark-directories.
cpio-dired-mark-directories is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-directories)
     :type 'error))

(ert-deftest cdmt-cpio-dired-mark-entries-containing-regexp ()
  "Test cpio-dired-mark-entries-containing-regexp.
cpio-dired-mark-entries-containing-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-entries-containing-regexp)
     :type 'error))

(ert-deftest cdmt-cpio-dired-mark-entries-regexp () ;✓
  (let ((test-name "cdmt-cpio-dired-mark-entries-regexp")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset)

    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.newc.cpio:

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
" cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))


(ert-deftest cdmt-cpio-dired-mark-executables ()
  "Test cpio-dired-mark-executables.
cpio-dired-mark-executables is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-executables)
     :type 'error))

(ert-deftest cdmt-cpio-dired-mark-subdir-entries ()
  "Test cpio-dired-mark-subdir-entries.
cpio-dired-mark-subdir-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-subdir-entries)
     :type 'error))

(ert-deftest cdmt-cpio-dired-mark-symlinks ()
  "Test cpio-dired-mark-symlinks.
cpio-dired-mark-symlinks is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-symlinks)
     :type 'error))

(ert-deftest cdmt-cpio-dired-mouse-find-entry-other-window ()
  "Test cpio-dired-mouse-find-entry-other-window.
cpio-dired-mouse-find-entry-other-window is not yet implemented -- expect an error."
  (should-error (cpio-dired-mouse-find-entry-other-window)
     :type 'error))

(ert-deftest cdmt-cpio-dired-next-dirline () ;✓
  "Test the function of M-x cpio-dired-next-dirline."
  (let ((test-name "cdmt-cpio-dired-next-dirline")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(entry-name)
        )
    (cdmt-reset 'make 'large)

    (progn (cpio-dired-next-dirline 1)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    (should (and "The current entry should be aaaaa.d"
		 (string-equal "aaaaa.d" entry-name)))
    ;; (should (and "The archive should be untouched."
;;    (should "Expecting an untouched large archive buffer."
    (should (string-equal *cdmt-untouched-large-archive-buffer* cpio-archive-buffer-contents))
    (should (and "The dired style buffer should be untouched."
		 (string-match *cdmt-untouched-large-dired-buffer* cpio-dired-buffer-contents)))

    (progn (cpio-dired-next-dirline 2)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "The current entry should be ccccc.d"
		 (string-equal "ccccc.d" entry-name)))
    (should (and "The archive buffer should be untouched. (1)"
		 (string-equal *cdmt-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (1)"
		 (string-match *cdmt-untouched-large-dired-buffer* cpio-dired-buffer-contents)))

    (progn (cpio-dired-next-dirline 4)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "The current entry should be ggggg.d"
		 (string-equal "ggggg.d" entry-name)))
    (should (and "The archive buffer should be untouched. (2)"
		 (string-equal *cdmt-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer shouold be untouched (2)"
		 (string-match *cdmt-untouched-large-dired-buffer* cpio-dired-buffer-contents)))

    (progn (cpio-dired-next-dirline 8)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "The current entry should be ooooo.d."
		 (string-equal "ooooo.d" entry-name)))
    (should (and "The archive buffer should be untouched. (3)"
		 (string-equal *cdmt-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (3)"
		 (string-match *cdmt-untouched-large-dired-buffer* cpio-dired-buffer-contents)))

    (progn (cpio-dired-next-dirline 16)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "The current entry should be zzzzz.d."
		 (string-equal "zzzzz.d" entry-name)))
    (should (and "The archive buffer should be untouched. (4)"
		 (string-equal *cdmt-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (4)"
		 (string-match *cdmt-untouched-large-dired-buffer* cpio-dired-buffer-contents)))

    (progn (cpio-dired-next-dirline 1)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "The current entry should still be zzzzz.d."
		 (string-equal "zzzzz.d" entry-name)))
    (should (and "The archive buffer should be untouched. (5)"
		 (string-equal *cdmt-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (5)"
		 (string-match *cdmt-untouched-large-dired-buffer* cpio-dired-buffer-contents)))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-next-line () ;✓
  "Test the function of M-x cpio-dired-next-line."
  (let ((test-name "cdmt-cpio-dired-next-line")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(entry-name)
        )
    (cdmt-reset 'make)

    (progn (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (string-equal "a" entry-name))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))

    (progn (cpio-dired-next-line 2)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal "aaa" entry-name))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))

    (progn (cpio-dired-next-line 4)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal "b" entry-name))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))

    (progn (cpio-dired-next-line 100)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (equal nil entry-name))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-next-marked-entry ()
  "Test cpio-dired-next-marked-entry.
cpio-dired-next-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-next-marked-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-next-marked-entry ()
  "Test cpio-dired-next-marked-entry.
cpio-dired-next-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-next-marked-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-next-subdir ()
  "Test the function of M-x cpio-next-subdir."
  (should-error (cpio-dired-next-marked-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-prev-marked-entry ()
  "Test cpio-dired-prev-marked-entry.
cpio-dired-prev-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-prev-marked-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-prev-marked-entry ()
  "Test cpio-dired-prev-marked-entry.
cpio-dired-prev-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-prev-marked-entry)
     :type 'error))

(ert-deftest cdmt-cpio-dired-prev-subdir ()
  "Test the function of M-x cpio-dired-prev-subdir."
  (should-error (cpio-dired-previous-line)
     :type 'error))

(ert-deftest cdmt-cpio-dired-previous-line () ;✓
  (let ((test-name "cdmt-cpio-dired-previous-line")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(where)
        )
    (cdmt-reset)

    (progn (goto-char (point-max))
	   (cpio-dired-previous-line 1)
	   (setq where (point)))

    (should (= where 1155))
    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-match *cdmt-untouched-small-archive-buffer* cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))


    (progn (cpio-dired-previous-line 2)
	   (setq where (point)))

    (should (= where 1019))
    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-match *cdmt-untouched-small-archive-buffer* cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents 
	  (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))

    (progn (cpio-dired-previous-line 4)
	   (setq where (point)))

    (should (= where 774))
    (setq cpio-archive-buffer-contents
	  (cdmt-filter-archive-contents
	   (with-current-buffer cpio-archive-buffer
	     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-match *cdmt-untouched-small-archive-buffer* cpio-archive-buffer-contents))
    (setq cpio-dired-buffer-contents
          (with-current-buffer cpio-dired-buffer
            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))


(ert-deftest cdmt-cpio-dired-previous-line ()
  "Test cpio-dired-previous-line.
cpio-dired-previous-line is not yet implemented -- expect an error."
  (should-error (cpio-dired-previous-line)
     :type 'error))

(ert-deftest cdmt-cpio-dired-previous-line ()
  "Test cpio-dired-previous-line.
cpio-dired-previous-line is not yet implemented -- expect an error."
  (should-error (cpio-dired-previous-line)
     :type 'error))

(ert-deftest cdmt-cpio-dired-show-entry-type ()
  "Test cpio-dired-show-entry-type.
cpio-dired-show-entry-type is not yet implemented -- expect an error."
  (should-error (cpio-dired-show-entry-type)
     :type 'error))

(ert-deftest cdmt-cpio-dired-sort-toggle-or-edit ()
  "Test cpio-dired-sort-toggle-or-edit.
cpio-dired-sort-toggle-or-edit is not yet implemented -- expect an error."
  (should-error (cpio-dired-sort-toggle-or-edit)
     :type 'error))

;; I'm not sure how to test this.
;; (ert-deftest cdmt-cpio-dired-summary () ;✓
;;   "Test the function of M-x cpio-dired-summary."
;;   (shell-command "cd test_data/alphabet ; make newc" nil nil)
;;   (let ((test-name "cdmt-cpio-dired-summary")
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
;;     (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
;;     ))

(ert-deftest cdmt-cpio-dired-toggle-marks ()
  "Test cpio-dired-toggle-marks.
cpio-dired-toggle-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-marks)
     :type 'error))

(ert-deftest cdmt-cpio-dired-toggle-marks ()
  "Test cpio-dired-toggle-marks.
cpio-dired-toggle-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-marks)
     :type 'error))

(ert-deftest cdmt-cpio-dired-toggle-read-only ()
  "Test cpio-dired-toggle-read-only.
cpio-dired-toggle-read-only is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-read-only)
     :type 'error))

(ert-deftest cdmt-cpio-dired-toggle-read-only ()
  "Test cpio-dired-toggle-read-only.
cpio-dired-toggle-read-only is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-read-only)
     :type 'error))

(ert-deftest cdmt-cpio-dired-undo ()
  "Test cpio-dired-undo.
cpio-dired-undo is not yet implemented -- expect an error."
  (should-error (cpio-dired-undo)
     :type 'error))

(ert-deftest cdmt-cpio-dired-unmark () ;✓
  "Test the function of M-x cpio-dired-unmark."
  (let ((test-name "cdmt-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-unmark 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "Expecting an untouched small archive."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with every entry except the first marked."
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

    (progn (cpio-dired-next-line 2)
	   (cpio-dired-unmark 2)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    
    (should (and "Expecting an untouched small archive."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecing a dired bugger with all but two entries marked."
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

    (progn (dired-next-line 4)
	   (cpio-dired-unmark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an untouched small archive."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with another 4 entries unmarked."
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

    (progn (dired-next-line 4)
	   (cpio-dired-unmark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))
    (should (and "Expecting an untouched archive."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with yet the last entry unmarked."
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

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-unmark-all-entries ()
  "Test cpio-dired-unmark-all-entries."
  (let ((test-name "cdmt-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-mark-entries-regexp ".")
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a variety of marks in a dired-style buffer."
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

    (progn (cpio-dired-unmark-all-entries "" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with no marks."
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

    (progn (cpio-dired-mark-entries-regexp ".")
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a variety of marks in a dired-style buffer."
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

    (progn (cpio-dired-unmark-all-entries "B" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with no B marks."
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

    (progn (cpio-dired-unmark-all-entries "F" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with neither B nor F marks."
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
   ))

(ert-deftest cdmt-cpio-dired-unmark-all-marks ()
  "Test cpio-dired-unmark-all-marks."
  (let ((test-name "cdmt-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-mark-entries-regexp ".")
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a variety of marks in a dired-style buffer."
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
  
    (progn (cpio-dired-unmark-all-marks)
	   (setq cpio-archive-buffer-contents
		 (cdmt-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with no marks."
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
    ))

(ert-deftest cdmt-cpio-dired-unmark-all-marks () ;✓
  "Test the function of M-x cpio-unmark-all-marks."
  (let ((test-name "cdmt-cpio-dired-unmark-all-marks")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
        )
    (cdmt-reset 'make)

    (progn (cpio-dired-mark 2)
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
		   (buffer-substring-no-properties (point-min) (point-max)))))

    (should (string-equal *cdmt-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match *cdmt-untouched-small-dired-buffer* cpio-dired-buffer-contents))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-unmark-backward ()
  "Test cpio-dired-unmark-backward.
cpio-dired-unmark-backward is not yet implemented -- expect an error."
  (should-error (cpio-dired-unmark-backward)
     :type 'error))

(ert-deftest cdmt-cpio-dired-unmark-backward ()
  "Test cpio-dired-unmark-backward.
cpio-dired-unmark-backward is not yet implemented -- expect an error."
  (should-error (cpio-dired-unmark-backward)
     :type 'error))

(ert-deftest cdmt-cpio-dired-up-directory ()
  "Test cpio-dired-up-directory.
cpio-dired-up-directory is not yet implemented -- expect an error."
  (should-error (cpio-dired-up-directory)
     :type 'error))

(ert-deftest cdmt-cpio-dired-upcase ()
  "Test cpio-dired-upcase.
cpio-dired-upcase is not yet implemented -- expect an error."
  (should-error (cpio-dired-upcase)
     :type 'error))

(ert-deftest cdmt-cpio-dired-view-archive () ;✓
  "Test the function of M-x cpio-view-archive."
  (let ((test-name "cdmt-cpio-dired-view-archive")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-archive-window)
	(cpio-dired-window)
        )
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
    (should (eq nil cpio-archive-window))

    (kill-buffer cpio-dired-buffer) ; This should kill the archive buffer too.
    ))

(ert-deftest cdmt-cpio-dired-view-entry ()
  "Test cpio-dired-view-entry.
cpio-dired-view-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-view-entry)
     :type 'error))

(ert-deftest cdmt-cpio-epa-dired-do-decrypt ()
  "Test cpio-epa-dired-do-decrypt.
cpio-epa-dired-do-decrypt is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-decrypt)
     :type 'error))

(ert-deftest cdmt-cpio-epa-dired-do-encrypt ()
  "Test cpio-epa-dired-do-encrypt.
cpio-epa-dired-do-encrypt is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-encrypt)
     :type 'error))

(ert-deftest cdmt-cpio-epa-dired-do-sign ()
  "Test cpio-epa-dired-do-sign.
cpio-epa-dired-do-sign is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-sign)
     :type 'error))

(ert-deftest cdmt-cpio-epa-dired-do-verify ()
  "Test cpio-epa-dired-do-verify.
cpio-epa-dired-do-verify is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-verify)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-delete-tag ()
  "Test cpio-image-dired-delete-tag.
cpio-image-dired-delete-tag is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-delete-tag)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-dired-comment-entries ()
  "Test cpio-image-dired-dired-comment-entries.
cpio-image-dired-dired-comment-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-comment-entries)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-dired-display-external ()
  "Test cpio-image-dired-dired-display-external.
cpio-image-dired-dired-display-external is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-display-external)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-dired-display-image ()
  "Test cpio-image-dired-dired-display-image.
cpio-image-dired-dired-display-image is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-display-image)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-dired-edit-comment-and-tags ()
  "Test cpio-image-dired-dired-edit-comment-and-tags.
cpio-image-dired-dired-edit-comment-and-tags is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-edit-comment-and-tags)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-dired-toggle-marked-thumbs ()
  "Test cpio-image-dired-dired-toggle-marked-thumbs.
cpio-image-dired-dired-toggle-marked-thumbs is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-toggle-marked-thumbs)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-display-thumb ()
  "Test cpio-image-dired-display-thumb.
cpio-image-dired-display-thumb is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumb)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-display-thumbs ()
  "Test cpio-image-dired-display-thumbs.
cpio-image-dired-display-thumbs is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumbs)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-display-thumbs-append ()
  "Test cpio-image-dired-display-thumbs-append.
cpio-image-dired-display-thumbs-append is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumbs-append)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-jump-thumbnail-buffer ()
  "Test cpio-image-dired-jump-thumbnail-buffer.
cpio-image-dired-jump-thumbnail-buffer is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-jump-thumbnail-buffer)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-mark-tagged-entries ()
  "Test cpio-image-dired-mark-tagged-entries.
cpio-image-dired-mark-tagged-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-mark-tagged-entries)
     :type 'error))

(ert-deftest cdmt-cpio-image-dired-tag-entries ()
  "Test cpio-image-dired-tag-entries.
cpio-image-dired-tag-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-tag-entries)
     :type 'error))

(ert-deftest cdmt-cpio-mouse-face ()
  "Test cpio-mouse-face.
cpio-mouse-face is not yet implemented -- expect an error."
  (should-error (cpio-mouse-face)
     :type 'error))

;;;;;;;; (ert-deftest cdmt-cpio-quit-window () ;✓
;;;;;;;;   "Test cpio-quit-window.
;;;;;;;; cpio-quit-window is not yet implemented -- expect an error."
;;;;;;;;   (let ((test-name "cdmt-cpio-dired-quit-window")
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

(ert-deftest cdmt-revert-buffer ()
  "Test revert-buffer.
revert-buffer is not yet implemented -- expect an error."
  (should-error (revert-buffer)
     :type 'error))

;;
;; Run tests
;;

;; (ert "cdmt-cpio-dired-display-entry")
;; (ert "cdmt-cpio-dired-save-archive")
(ert "^cdmt-")

;;; cpio-dired-test.el ends here.

