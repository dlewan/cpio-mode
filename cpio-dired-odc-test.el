;	$Id: cpio-dired-odc-test.el,v 1.2 2018/05/21 21:21:16 doug Exp $	
;; -*- coding: utf-8 -*-
;;; cpio-dired-test.el --- Tests of cpio-dired-mode.
;	$Id: cpio-dired-odc-test.el,v 1.2 2018/05/21 21:21:16 doug Exp $	

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
(load (concat default-directory "cpio.el"))


;; 
;; Vars
;; 

(defvar *cdmt-odc-small-archive* "test_data/alphabet/alphabet_small.odc.cpio"
  "A small archive used for testing.")
(setq *cdmt-odc-small-archive* "test_data/alphabet/alphabet_small.odc.cpio")
(defvar *cdmt-odc-large-archive* "test_data/alphabet/alphabet.odc.cpio"
  "A large archive used for testing.")
(setq *cdmt-odc-large-archive* "test_data/alphabet/alphabet.odc.cpio")

(defvar *cdmt-odc-untouched-small-archive* "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))
\\0\\0\\0
aa

\\0\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))
\\0\\0
aaa

\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))
\\0
aaaa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))
\\0\\0\\0
bb

\\0\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))
\\0\\0
bbb

\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))
\\0
bbbb

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))
\\0\\0\\0
cc

\\0\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))
\\0\\0
ccc

\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))
\\0
cccc

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
\\0\\0070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
"
  "The contents of the untouched small archive.")
(setq *cdmt-odc-untouched-small-archive* "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
")

(defvar *cdmt-odc-untouched-small-dired-buffer* "CPIO archive: alphabet_small.odc.cpio:

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
(setq *cdmt-odc-untouched-small-dired-buffer* "CPIO archive: alphabet_small.odc.cpio:

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

(defvar *cdmt-odc-untouched-small-catalog* "((\"a\" .
  [[43252341448 33188 1000 1000 1
	 (23281 65535)
	 4 253 0 0 0 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 113 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" .
  [[43252341474 33188 1000 1000 1
	 (23281 65535)
	 5 253 0 0 0 3 0 \"aa\"]
   #<marker at 117 in alphabet_small.odc.cpio> #<marker at 233 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" .
  [[43252341508 33188 1000 1000 1
	 (23281 65535)
	 6 253 0 0 0 4 0 \"aaa\"]
   #<marker at 241 in alphabet_small.odc.cpio> #<marker at 357 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" .
  [[43252341511 33188 1000 1000 1
	 (23281 65535)
	 7 253 0 0 0 5 0 \"aaaa\"]
   #<marker at 365 in alphabet_small.odc.cpio> #<marker at 481 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" .
  [[43252341512 33188 1000 1000 1
	 (23281 65535)
	 8 253 0 0 0 6 0 \"aaaaa\"]
   #<marker at 489 in alphabet_small.odc.cpio> #<marker at 605 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa.d\" .
  [[43252341515 16877 1000 1000 2
	 (23268 65535)
	 0 253 0 0 0 8 0 \"aaaaa.d\"]
   #<marker at 613 in alphabet_small.odc.cpio> #<marker at 733 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" .
  [[43252341513 33188 1000 1000 1
	 (23281 65535)
	 4 253 0 0 0 2 0 \"b\"]
   #<marker at 733 in alphabet_small.odc.cpio> #<marker at 845 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" .
  [[43252341514 33188 1000 1000 1
	 (23281 65535)
	 5 253 0 0 0 3 0 \"bb\"]
   #<marker at 849 in alphabet_small.odc.cpio> #<marker at 965 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" .
  [[43252341516 33188 1000 1000 1
	 (23281 65535)
	 6 253 0 0 0 4 0 \"bbb\"]
   #<marker at 973 in alphabet_small.odc.cpio> #<marker at 1089 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" .
  [[43252341517 33188 1000 1000 1
	 (23281 65535)
	 7 253 0 0 0 5 0 \"bbbb\"]
   #<marker at 1097 in alphabet_small.odc.cpio> #<marker at 1213 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" .
  [[43252341518 33188 1000 1000 1
	 (23281 65535)
	 8 253 0 0 0 6 0 \"bbbbb\"]
   #<marker at 1221 in alphabet_small.odc.cpio> #<marker at 1337 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb.d\" .
  [[43252341601 16877 1000 1000 2
	 (23268 65535)
	 0 253 0 0 0 8 0 \"bbbbb.d\"]
   #<marker at 1345 in alphabet_small.odc.cpio> #<marker at 1465 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" .
  [[43252341519 33188 1000 1000 1
	 (23281 65535)
	 4 253 0 0 0 2 0 \"c\"]
   #<marker at 1465 in alphabet_small.odc.cpio> #<marker at 1577 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" .
  [[43252341600 33188 1000 1000 1
	 (23281 65535)
	 5 253 0 0 0 3 0 \"cc\"]
   #<marker at 1581 in alphabet_small.odc.cpio> #<marker at 1697 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" .
  [[43252341602 33188 1000 1000 1
	 (23281 65535)
	 6 253 0 0 0 4 0 \"ccc\"]
   #<marker at 1705 in alphabet_small.odc.cpio> #<marker at 1821 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" .
  [[43252341603 33188 1000 1000 1
	 (23281 65535)
	 7 253 0 0 0 5 0 \"cccc\"]
   #<marker at 1829 in alphabet_small.odc.cpio> #<marker at 1945 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" .
  [[43252341604 33188 1000 1000 1
	 (23281 65535)
	 8 253 0 0 0 6 0 \"ccccc\"]
   #<marker at 1953 in alphabet_small.odc.cpio> #<marker at 2069 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc.d\" .
  [[43252341607 16877 1000 1000 2
	 (23268 65535)
	 0 253 0 0 0 8 0 \"ccccc.d\"]
   #<marker at 2077 in alphabet_small.odc.cpio> #<marker at 2197 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
"
  "An string representing an untouched catalog.")
(setq *cdmt-odc-untouched-small-catalog* "((\"a\" .
  [[43252341448 33188 1000 1000 1
	 (23281 65535)
	 4 253 0 0 0 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 113 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" .
  [[43252341474 33188 1000 1000 1
	 (23281 65535)
	 5 253 0 0 0 3 0 \"aa\"]
   #<marker at 117 in alphabet_small.odc.cpio> #<marker at 233 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" .
  [[43252341508 33188 1000 1000 1
	 (23281 65535)
	 6 253 0 0 0 4 0 \"aaa\"]
   #<marker at 241 in alphabet_small.odc.cpio> #<marker at 357 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" .
  [[43252341511 33188 1000 1000 1
	 (23281 65535)
	 7 253 0 0 0 5 0 \"aaaa\"]
   #<marker at 365 in alphabet_small.odc.cpio> #<marker at 481 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" .
  [[43252341512 33188 1000 1000 1
	 (23281 65535)
	 8 253 0 0 0 6 0 \"aaaaa\"]
   #<marker at 489 in alphabet_small.odc.cpio> #<marker at 605 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa.d\" .
  [[43252341515 16877 1000 1000 2
	 (23268 65535)
	 0 253 0 0 0 8 0 \"aaaaa.d\"]
   #<marker at 613 in alphabet_small.odc.cpio> #<marker at 733 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" .
  [[43252341513 33188 1000 1000 1
	 (23281 65535)
	 4 253 0 0 0 2 0 \"b\"]
   #<marker at 733 in alphabet_small.odc.cpio> #<marker at 845 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" .
  [[43252341514 33188 1000 1000 1
	 (23281 65535)
	 5 253 0 0 0 3 0 \"bb\"]
   #<marker at 849 in alphabet_small.odc.cpio> #<marker at 965 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" .
  [[43252341516 33188 1000 1000 1
	 (23281 65535)
	 6 253 0 0 0 4 0 \"bbb\"]
   #<marker at 973 in alphabet_small.odc.cpio> #<marker at 1089 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" .
  [[43252341517 33188 1000 1000 1
	 (23281 65535)
	 7 253 0 0 0 5 0 \"bbbb\"]
   #<marker at 1097 in alphabet_small.odc.cpio> #<marker at 1213 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" .
  [[43252341518 33188 1000 1000 1
	 (23281 65535)
	 8 253 0 0 0 6 0 \"bbbbb\"]
   #<marker at 1221 in alphabet_small.odc.cpio> #<marker at 1337 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb.d\" .
  [[43252341601 16877 1000 1000 2
	 (23268 65535)
	 0 253 0 0 0 8 0 \"bbbbb.d\"]
   #<marker at 1345 in alphabet_small.odc.cpio> #<marker at 1465 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" .
  [[43252341519 33188 1000 1000 1
	 (23281 65535)
	 4 253 0 0 0 2 0 \"c\"]
   #<marker at 1465 in alphabet_small.odc.cpio> #<marker at 1577 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" .
  [[43252341600 33188 1000 1000 1
	 (23281 65535)
	 5 253 0 0 0 3 0 \"cc\"]
   #<marker at 1581 in alphabet_small.odc.cpio> #<marker at 1697 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" .
  [[43252341602 33188 1000 1000 1
	 (23281 65535)
	 6 253 0 0 0 4 0 \"ccc\"]
   #<marker at 1705 in alphabet_small.odc.cpio> #<marker at 1821 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" .
  [[43252341603 33188 1000 1000 1
	 (23281 65535)
	 7 253 0 0 0 5 0 \"cccc\"]
   #<marker at 1829 in alphabet_small.odc.cpio> #<marker at 1945 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" .
  [[43252341604 33188 1000 1000 1
	 (23281 65535)
	 8 253 0 0 0 6 0 \"ccccc\"]
   #<marker at 1953 in alphabet_small.odc.cpio> #<marker at 2069 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc.d\" .
  [[43252341607 16877 1000 1000 2
	 (23268 65535)
	 0 253 0 0 0 8 0 \"ccccc.d\"]
   #<marker at 2077 in alphabet_small.odc.cpio> #<marker at 2197 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
")

(defvar *cdmt-odc-untouched-large-archive-buffer* "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
d	(( filename ))

d

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
dd	(( filename ))

dd

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ddd	(( filename ))

ddd

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
dddd	(( filename ))

dddd

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ddddd	(( filename ))

ddddd

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ddddd.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
e	(( filename ))

e

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ee	(( filename ))

ee

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
eee	(( filename ))

eee

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
eeee	(( filename ))

eeee

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
eeeee	(( filename ))

eeeee

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
eeeee.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
f	(( filename ))

f

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ff	(( filename ))

ff

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
fff	(( filename ))

fff

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
ffff	(( filename ))

ffff

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
fffff	(( filename ))

fffff

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
fffff.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
g	(( filename ))

g

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
gg	(( filename ))

gg

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ggg	(( filename ))

ggg

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
gggg	(( filename ))

gggg

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ggggg	(( filename ))

ggggg

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ggggg.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
h	(( filename ))

h

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
hh	(( filename ))

hh

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
hhh	(( filename ))

hhh

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
hhhh	(( filename ))

hhhh

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
hhhhh	(( filename ))

hhhhh

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
hhhhh.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
i	(( filename ))

i

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ii	(( filename ))

ii

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
iii	(( filename ))

iii

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
iiii	(( filename ))

iiii

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
iiiii	(( filename ))

iiiii

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
iiiii.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
j	(( filename ))

j

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
jj	(( filename ))

jj

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
jjj	(( filename ))

jjj

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
jjjj	(( filename ))

jjjj

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
jjjjj	(( filename ))

jjjjj

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
jjjjj.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
k	(( filename ))

k

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
kk	(( filename ))

kk

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
kkk	(( filename ))

kkk

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
kkkk	(( filename ))

kkkk

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
kkkkk	(( filename ))

kkkkk

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
kkkkk.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
l	(( filename ))

l

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ll	(( filename ))

ll

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
lll	(( filename ))

lll

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
llll	(( filename ))

llll

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
lllll	(( filename ))

lllll

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
lllll.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
m	(( filename ))

m

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
mm	(( filename ))

mm

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
mmm	(( filename ))

mmm

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
mmmm	(( filename ))

mmmm

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
mmmmm	(( filename ))

mmmmm

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
mmmmm.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
n	(( filename ))

n

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
nn	(( filename ))

nn

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
nnn	(( filename ))

nnn

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
nnnn	(( filename ))

nnnn

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
nnnnn	(( filename ))

nnnnn

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
nnnnn.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
o	(( filename ))

o

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
oo	(( filename ))

oo

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ooo	(( filename ))

ooo

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
oooo	(( filename ))

oooo

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ooooo	(( filename ))

ooooo

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ooooo.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
p	(( filename ))

p

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
pp	(( filename ))

pp

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ppp	(( filename ))

ppp

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
pppp	(( filename ))

pppp

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ppppp	(( filename ))

ppppp

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ppppp.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
q	(( filename ))

q

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
qq	(( filename ))

qq

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
qqq	(( filename ))

qqq

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
qqqq	(( filename ))

qqqq

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
qqqqq	(( filename ))

qqqqq

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
qqqqq.d	(( filename ))
070707	(( magic    ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
r	(( filename ))

r

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
rr	(( filename ))

rr

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
rrr	(( filename ))

rrr

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
rrrr	(( filename ))

rrrr

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
rrrrr	(( filename ))

rrrrr

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
rrrrr.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
s	(( filename ))

s

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ss	(( filename ))

ss

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
sss	(( filename ))

sss

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
ssss	(( filename ))

ssss

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
sssss	(( filename ))

sssss

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
sssss.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
t	(( filename ))

t

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
tt	(( filename ))

tt

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ttt	(( filename ))

ttt

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
tttt	(( filename ))

tttt

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ttttt	(( filename ))

ttttt

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ttttt.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
u	(( filename ))

u

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
uu	(( filename ))

uu

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
uuu	(( filename ))

uuu

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
uuuu	(( filename ))

uuuu

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
uuuuu	(( filename ))

uuuuu

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
uuuuu.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
v	(( filename ))

v

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
vv	(( filename ))

vv

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
vvv	(( filename ))

vvv

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
vvvv	(( filename ))

vvvv

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
vvvvv	(( filename ))

vvvvv

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
vvvvv.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
w	(( filename ))

w

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ww	(( filename ))

ww

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
www	(( filename ))

www

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
wwww	(( filename ))

wwww

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
wwwww	(( filename ))

wwwww

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
wwwww.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
x	(( filename ))

x

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
xx	(( filename ))

xx

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
xxx	(( filename ))

xxx

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
xxxx	(( filename ))

xxxx

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
xxxxx	(( filename ))

xxxxx

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
xxxxx.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
y	(( filename ))

y

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
yy	(( filename ))

yy

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
yyy	(( filename ))

yyy

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
yyyy	(( filename ))

yyyy

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
yyyyy	(( filename ))

yyyyy

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
yyyyy.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
z	(( filename ))

z

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
zz	(( filename ))

zz

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
zzz	(( filename ))

zzz

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
zzzz	(( filename ))

zzzz

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
zzzzz	(( filename ))

zzzzz

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
zzzzz.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))

"
  "Contents of the untouched large cpio archive buffer.")
(setq *cdmt-odc-untouched-large-archive-buffer* "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
d	(( filename ))

d

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
dd	(( filename ))

dd

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ddd	(( filename ))

ddd

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
dddd	(( filename ))

dddd

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ddddd	(( filename ))

ddddd

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ddddd.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
e	(( filename ))

e

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ee	(( filename ))

ee

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
eee	(( filename ))

eee

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
eeee	(( filename ))

eeee

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
eeeee	(( filename ))

eeeee

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
eeeee.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
f	(( filename ))

f

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ff	(( filename ))

ff

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
fff	(( filename ))

fff

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
ffff	(( filename ))

ffff

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
fffff	(( filename ))

fffff

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
fffff.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
g	(( filename ))

g

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
gg	(( filename ))

gg

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ggg	(( filename ))

ggg

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
gggg	(( filename ))

gggg

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ggggg	(( filename ))

ggggg

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ggggg.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
h	(( filename ))

h

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
hh	(( filename ))

hh

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
hhh	(( filename ))

hhh

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
hhhh	(( filename ))

hhhh

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
hhhhh	(( filename ))

hhhhh

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
hhhhh.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
i	(( filename ))

i

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ii	(( filename ))

ii

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
iii	(( filename ))

iii

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
iiii	(( filename ))

iiii

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
iiiii	(( filename ))

iiiii

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
iiiii.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
j	(( filename ))

j

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
jj	(( filename ))

jj

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
jjj	(( filename ))

jjj

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
jjjj	(( filename ))

jjjj

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
jjjjj	(( filename ))

jjjjj

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
jjjjj.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
k	(( filename ))

k

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
kk	(( filename ))

kk

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
kkk	(( filename ))

kkk

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
kkkk	(( filename ))

kkkk

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
kkkkk	(( filename ))

kkkkk

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
kkkkk.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
l	(( filename ))

l

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ll	(( filename ))

ll

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
lll	(( filename ))

lll

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
llll	(( filename ))

llll

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
lllll	(( filename ))

lllll

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
lllll.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
m	(( filename ))

m

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
mm	(( filename ))

mm

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
mmm	(( filename ))

mmm

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
mmmm	(( filename ))

mmmm

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
mmmmm	(( filename ))

mmmmm

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
mmmmm.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
n	(( filename ))

n

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
nn	(( filename ))

nn

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
nnn	(( filename ))

nnn

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
nnnn	(( filename ))

nnnn

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
nnnnn	(( filename ))

nnnnn

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
nnnnn.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
o	(( filename ))

o

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
oo	(( filename ))

oo

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ooo	(( filename ))

ooo

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
oooo	(( filename ))

oooo

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ooooo	(( filename ))

ooooo

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ooooo.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
p	(( filename ))

p

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
pp	(( filename ))

pp

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ppp	(( filename ))

ppp

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
pppp	(( filename ))

pppp

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ppppp	(( filename ))

ppppp

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ppppp.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
q	(( filename ))

q

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
qq	(( filename ))

qq

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
qqq	(( filename ))

qqq

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
qqqq	(( filename ))

qqqq

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
qqqqq	(( filename ))

qqqqq

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
qqqqq.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
r	(( filename ))

r

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
rr	(( filename ))

rr

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
rrr	(( filename ))

rrr

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
rrrr	(( filename ))

rrrr

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
rrrrr	(( filename ))

rrrrr

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
rrrrr.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
s	(( filename ))

s

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ss	(( filename ))

ss

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
sss	(( filename ))

sss

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
ssss	(( filename ))

ssss

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
sssss	(( filename ))

sssss

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
sssss.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
t	(( filename ))

t

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
tt	(( filename ))

tt

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ttt	(( filename ))

ttt

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
tttt	(( filename ))

tttt

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ttttt	(( filename ))

ttttt

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ttttt.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
u	(( filename ))

u

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
uu	(( filename ))

uu

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
uuu	(( filename ))

uuu

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
uuuu	(( filename ))

uuuu

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
uuuuu	(( filename ))

uuuuu

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
uuuuu.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
v	(( filename ))

v

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
vv	(( filename ))

vv

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
vvv	(( filename ))

vvv

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
vvvv	(( filename ))

vvvv

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
vvvvv	(( filename ))

vvvvv

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
vvvvv.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
w	(( filename ))

w

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
ww	(( filename ))

ww

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
www	(( filename ))

www

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
wwww	(( filename ))

wwww

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
wwwww	(( filename ))

wwwww

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
wwwww.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
x	(( filename ))

x

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
xx	(( filename ))

xx

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
xxx	(( filename ))

xxx

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
xxxx	(( filename ))

xxxx

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
xxxxx	(( filename ))

xxxxx

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
xxxxx.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
y	(( filename ))

y

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
yy	(( filename ))

yy

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
yyy	(( filename ))

yyy

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
yyyy	(( filename ))

yyyy

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
yyyyy	(( filename ))

yyyyy

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
yyyyy.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
z	(( filename ))

z

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
zz	(( filename ))

zz

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
zzz	(( filename ))

zzz

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
zzzz	(( filename ))

zzzz

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
zzzzz	(( filename ))

zzzzz

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
zzzzz.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
")

(defvar *cdmt-odc-untouched-large-dired-buffer* "CPIO archive: alphabet.odc.cpio:

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
(setq *cdmt-odc-untouched-large-dired-buffer* "CPIO archive: alphabet.odc.cpio:

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



;; 
;; Library
;; 

(shell-command "cd test_data/alphabet ; make odc" nil nil)

(defun cdmt-odc-reset (&optional make large)
  "Reset the current cpio-dired environment.
If MAKE is non-nil, then run 'make odc' as part of the reset."
  (let ((fname "cdmt-odc-reset")
	(archive-name)
	(archive-names (list *cdmt-odc-small-archive*
			     *cdmt-odc-large-archive*)))
    (cd run-dir)
    (mapc (lambda (an)
	    (setq cpio-archive-buffer (find-file-noselect an))
	    (if (and (file-exists-p an)
		     (buffer-live-p (get-buffer cpio-archive-buffer)))
		(with-current-buffer cpio-archive-buffer
		  (revert-buffer nil 'noconfirm))
	      (condition-case v
		  (kill-buffer cpio-dired-buffer)
		(error (message "%s(): no dired buffer to kill." fname)))))
	  archive-names)

    (cd run-dir)

    (if make
	(shell-command "cd test_data/alphabet ; make odc" nil nil))	

    (setq archive-name (if large 
			   *cdmt-odc-large-archive*
			 *cdmt-odc-small-archive*))

    (delete-other-windows)
    (with-current-buffer (setq cpio-archive-buffer (find-file-noselect archive-name))
      (if (string-match "/test_data/.+/test_data/" (buffer-file-name))
	  (error "Bogus archive!"))
      (cpio-mode))
    (setq cpio-dired-buffer (switch-to-buffer (cpio-dired-buffer-name archive-name)))))

(defun cdmt-odc-filter-archive-contents (archive-contents)
  "Make the given ARCHIVE-CONTENTS fully printable and readable."
  (let ((fname "cdmt-odc-filter-archive-contents")
	(char-map (list (cons "\0" "\\0"))))
    (setq archive-contents (cdmt-odc-reformat-odc-headers archive-contents))
    (mapc (lambda (cm)
	    (let ((from (car cm))
		  (to (cdr cm)))
	      (setq archive-contents (cdmt-odc-global-sub from to archive-contents))))
	  char-map)
    archive-contents))

(defun cdmt-odc-reformat-odc-headers (archive-contents)
  "Reformat the cpio odc entry headers in the given ARCHIVE-CONTENTS
So that they are human readable.
CAVEATS: \(1\) If ARCHIVE-CONTENTS contains entries that contain entry headers,
then those will also be reformatted.
\(2\) The entry names are taken to be a sequence of printable characters.
So, if NULLs have been converted to printable characters,
then the entry names will be incorrect."
  (let ((fname "cdmt-odc-reformat-odc-headers"))
    (while (string-match *cpio-odc-header-re* archive-contents)
      (setq archive-contents (concat (substring archive-contents 0 (match-beginning 0))
				     (concat (match-string *cpio-odc-magic-re-idx*    archive-contents) "\t(( magic    ))\n")
				     (concat "DEADBE"                                                   "\t(( ino      ))\n")
				     (concat (match-string *cpio-odc-mode-re-idx*     archive-contents) "\t(( mode     ))\n")
				     (concat (match-string *cpio-odc-uid-re-idx*      archive-contents) "\t(( uid      ))\n")
				     (concat (match-string *cpio-odc-gid-re-idx*      archive-contents) "\t(( gid      ))\n")
				     (concat (match-string *cpio-odc-nlink-re-idx*    archive-contents) "\t(( nlink    ))\n")
				     ;; Note that the mod time can change.
				     (concat "DEADBE"                                                   "\t(( mtime    ))\n")
				     (concat (match-string *cpio-odc-filesize-re-idx* archive-contents) "\t(( filesize ))\n")
				     (concat "DEADBE"                                                   "\t(( dev maj  ))\n")
				     (concat "DEADBE"                                                   "\t(( dev min  ))\n")
				     (concat "DEADBE"                                                   "\t(( rdev maj ))\n")
				     (concat "DEADBE"                                                   "\t(( rdev min ))\n")
				     (concat (match-string *cpio-odc-namesize-re-idx* archive-contents) "\t(( namesize ))\n")
				     (concat "000000"                                                   "\t(( chksum   ))\n")
				     (concat (match-string *cpio-odc-filename-re-idx* archive-contents) "\t(( filename ))\n")
				     (substring archive-contents (match-end 0)))))
    (concat archive-contents "\n")))

(defun cdmt-odc-global-sub (from-str to-str string)
  "Globally substitute TO-STR for FROM-STR in STRING and return the new string.
In principal, FROM-STR can be a regular expression."
  (let ((fname "cdmt-odc-global-sub"))
    (while (string-match from-str string)
      (setq string (replace-match to-str nil t string 0)))
    string))

;; 
;; Commands
;; 

;;
;; The contents of these tests were generated
;; from the dired-mode-map definition
;; (where commands are tagged as done).
;; A few keyboard macros modified those results.
;; The following commands were used to create the skeletons below

(defun cdmt-odc-unfinished-command ()
  "Create a stub test for the next unfinished command."
  (interactive)
  (let ((fname "cdmt-odc-unfinished-command")
	(test-declaration-string "cdmt-odc-")
	(defined-command-regexp ".+) ;✓$")
	(command-name)
	(where))
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

(defun cdmt-odc-all-unfinished-commands ()
  "Write stub tests for all unfinished commands following point."
  (interactive)
  (let ((fname "cdmt-odc-all-unfinished-commands"))
    (while (cdmt-odc-unfinished-command))))

(defun cdmt-odc-finished-command ()
  "Write a stub test for a finished command."
  (interactive)
  (let ((fname "cdmt-odc-finished-command")
	(finished-command-regexp ") ;✓$")
	(command-name))
    (cond ((re-search-forward finished-command-regexp (point-max) t)
	   (beginning-of-line)
	   (re-search-forward "cdmt-odc-" (line-end-position))
	   (setq where (match-end 0))
	   (end-of-line)
	   (backward-char 4)
	   (setq command-name (buffer-substring-no-properties where (point)))

	   (insert " (")
	   (end-of-line)
	   (insert "\n")
	   (insert (format "  \"Test the function M-x cpio-%s.\"\n" command-name))
	   (insert         "  (shell-command \"cd test_data/alphabet ; make odc\" nil nil)\n")
	   (insert (format "  (let ((test-name \"cdmt-odc-%s\")\n" command-name))
	   (insert         "        (cpio-archive-buffer)\n")
	   (insert         "        (cpio-archive-buffer-contents)\n")
	   (insert         "        (cpio-dired-buffer)\n")
	   (insert         "        (cpio-dired-buffer-contents)\n")
	   (insert         "        )\n")
	   (insert         "    (cdmt-odc-reset)\n")
	   (insert         "\n")
	   (insert (format "    (%s)\n" command-name))
	   (insert         "PREPARATION\n")
	   (insert         "\n")
	   (insert         "    (setq cpio-archive-buffer-contents\n")
	   (insert         "          (cdmt-odc-filter-archive-contents\n")
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
	  (t nil))))

(defun cdmt-odc-all-finished-commands ()
  "Build stub tests for all the finished commands."
  (interactive)
  (let ((fname "cdmt-odc-all-finished-commands"))
    (while (cdmt-odc-finished-command))))

(defun cdmt-odc-ediff-results ()
  "Compare the results of a (string-match) on the current line in a buffer of ERT results.
The results are always presented as (string-match EXPECTED-RE ACTUAL) on that line."
  (interactive)
  (let ((fname "cdmt-odc-ediff-results")
	(expected)
	(actual)
	(start))
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

    (ediff-buffers "actual" "expected")))


(defun cdmt-odc-do-cpio-id (where archive)
  "Run cpio(1) on the given ARCHIVE and report the results.
WHERE Should be a line number."
  (let* ((fname "cdmt-odc-do-cpio-id")
	 (buf-name "*cpio output*")
	 (buf (get-buffer buf-name))
	 (make-directory "qwerqwer"))
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

(defun cdmt-odc-sweep ()
  "Sweep the cpio-dired-test.el buffer and make changes
to help make tests pass correctly."
  (interactive)
  (let ((fname "cdmt-odc-sweep"))
    (cdmt-odc-sweep-ids)
    (cdmt-odc-sweep-times)))

(defun cdmt-odc-sweep-ids ()
  "Replace UIDs and GIDs = [[:digit:]]+ by a better RE."
  (let ((fname "cdmt-odc-sweep-ids"))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward "\\s-+\\(1000\\)\\s-" (point-max) t)
	  (replace-match "[[:digit:]]+" 'fixed-case 'literal nil 1))))))

(defun cdmt-odc-sweep-times ()
  "Replace times (date-times) with a better RE."
  (let ((fname "cdmt-odc-sweep-times"))
    (save-excursion 
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward "\\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}" (point-max) t)
	  (replace-match "\\\\\\\\(?:a\\\\\\\\(?:pr\\\\\\\\|ug\\\\\\\\)\\\\\\\\|dec\\\\\\\\|feb\\\\\\\\|j\\\\\\\\(?:an\\\\\\\\|u[ln]\\\\\\\\)\\\\\\\\|ma[ry]\\\\\\\\|nov\\\\\\\\|oct\\\\\\\\|sep\\\\\\\\) [[:digit:]]\\\\\\\\{2\\\\\\\\} [[:digit:]]\\\\\\\\{2\\\\\\\\}:[[:digit:]]\\\\\\\\{2\\\\\\\\}" nil nil nil 0))))))

;; This keyboard macro proved useful in working on chmod.
;; Here's the setup:
;; ┌──────────┬───────────┐
;; │ Filtered │ dired     │
;; │ archive  │ style     │
;; │ buffer.  │ buffer    │
;; │ point at ├───────────┤
;; │ point-min│ *scratch* │
;; │          │           │
;; └──────────┴───────────┘
;; FYI cm- = chmod macro
(defvar cm-archive-window)
(defvar cm-dired-window)
(defvar scratch-window)
(defun cm-setup ()
  "Prepare the window setup for (cm)."
  (interactive)
  (let ((fname "cm-setup"))
    (delete-other-windows)
    (switch-to-buffer "actual")
    (setq cm-archive-window (selected-window))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer "CPIO archive: alphabet_small.odc.cpio")
    (cpio-dired-move-to-first-entry)
    (setq cm-dired-window (selected-window))
    (split-window)
    (other-window 1)
    (switch-to-buffer "*scratch*")
    (setq scratch-window (selected-window))
    (other-window 1)
    (goto-char (point-min))))
(defun cm ()
  "Run a verification of the next mode in the archive."
  (interactive)
  (let ((fname "cm")
	(mode-hex-string)
	(entry-name))
    (search-forward "magic")
    (recenter-top-bottom 0)
    (search-forward "mode")
    (setq mode-hex-string (buffer-substring-no-properties
			   (line-beginning-position)
			   (+ 8 (line-beginning-position))))
    (search-forward "filename")
    (beginning-of-line)
    (looking-at "[[:graph:]]+")
    (setq entry-name (match-string 0))
    (other-window 2)
    (insert (format "%s:\t%06o\n" entry-name (string-to-number mode-hex-string 16)))
    (other-window 2)
    (dired-next-line 1)
    (other-window 2)))

(defun cdmt-odc-sweep-catalog ()
  "Clean up a copy of the cpio catalog,
as presented in an 'actual' buffer (see `cdmt-odc-ediff-results')
for use in a string-match-p."
  ;; If this were written in good LISP,
  ;; then it would be a (mapcar)
  ;; over a list of (cons RE replacement)
  ;; with a (replace-match).
  ;; However, I think the comments are helpful.
  (interactive)
  (let ((fname "cdmt-odc-sweep-catalog"))
    ;; Opening [.
    (goto-char (point-min))
    (while (re-search-forward "\\[" (point-max) t)
      (replace-match "\\\\["))
    ;; Closing ].
    (goto-char (point-min))
    (while (search-forward "]" (point-max) t)
      (replace-match "\\\\]"))
    ;; '.'
    (goto-char (point-min))
    (while (search-forward "." (point-max) t)
      (replace-match "\\\\."))
    ;; ino
    (goto-char (point-min))
    (while (re-search-forward "\\\\[\\\\[[[:digit:]]+" (point-max) t)
      (replace-match "\\\\[\\\\[[[:digit:]]+"))
    ;; UID/GID
    (goto-char (point-min))
    (while (re-search-forward "\\<1000\\>" (point-max) t)
      (replace-match "[[:digit:]]+"))
    ;; mtime
    (goto-char (point-min))
    (while (re-search-forward "([[:digit:]]+ [[:digit:]]+)" (point-max) t)
      (replace-match "([[:digit:]]+ [[:digit:]]+)"))
    ;; devs
    (goto-char (point-min))
    (while (re-search-forward "\\(\\s-[[:digit:]]+\\) [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+" (point-max) t)
      (replace-match "\\1 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+"))
    ;; Double new lines.
    (goto-char (point-min))
    (while (search-forward "\n\n" (point-max) t)
      (replace-match "\n"))
    ;; Finally, make the result a string.
    (goto-char (point-min))
    (insert "\"")
    (goto-char (point-max))
    (insert "\"")
    ;; Grab the whole buffer.
    (copy-region-as-kill (point-min) (point-max))))

(defun cdmt-odc-test-save (&optional large)
  "A generic test to run at the end of every test
to check that the saved archive seems sane."
  (cd run-dir)
  (let* ((fname "cdmt-odc-test-save")
	 (alphabet-dir (concat default-directory "test_data/alphabet"))
	 (test-buffer-dir (concat alphabet-dir "/asdfasdf"))
	 (test-buffer)
	 (directory default-directory)
	 (dired-buffer (current-buffer))
	 (archive (if large "../alphabet.odc.cpio"
		    "../alphabet_small.odc.cpio")))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    
    (if (file-exists-p test-buffer-dir)
	(call-process "rm" nil nil nil "-rf" test-buffer-dir))
    (if (file-exists-p test-buffer-dir)
	(error "%s(): Removing %s failed."))
    (with-current-buffer cpio-archive-buffer
      (cpio-dired-save-archive))
    (make-directory test-buffer-dir 'parents)

    (cd test-buffer-dir)
    (with-current-buffer (find-file-noselect test-buffer-dir)
      (call-process "cpio" archive nil nil "-id")
      (mapc (lambda (en)
	      ;; No, this isn't bullet proof or even correct.
	      ;; It's just a sanity check; it's certainly not complete.
	      (should (file-exists-p (car en))))
	    (with-current-buffer cpio-archive-buffer
	      (cpio-catalog))))
    (cd directory)))

(defun cdmt-odc-sweep-hex ()
  "Convert the newc hex fields to odc octal fields."
  (interactive)
  (let ((fname "cdmt-odc-sweep-hex")
	(value)
	(replacement)
	)
    ;; (error "%s() is not yet implemented" fname)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:xdigit:]]\\{8\\}" (point-max) t)
	(setq value (string-to-number (match-string 0) 16))
	(setq replacement (format "%06o" value))
	(replace-match replacement)))
    ))


;; 
;; Tests
;; 

;; N.B. cdmt-odc- = cpio-dired-mode-test-

(defvar run-dir default-directory)

(custom-set-variables (list 'cpio-try-names nil))

;; All tests use M-x cpio-dired-kill.
(ert-deftest cdmt-odc-cpio-dired-kill () ;✓
  "Test the function of M-x cpio-dired-kill."
  (let ((test-name "cdmt-odc-cpio-dired-kill")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents))
    (cdmt-odc-reset 'make)

    (cpio-dired-kill)

    (should (and "Dired style buffer should not be live."
		 (not (buffer-live-p cpio-dired-buffer))))
    (should (and "Archive buffer should not be live."
		 (not (buffer-live-p cpio-archive-buffer))))))

(ert-deftest cdmt-odc-cpio-dired-do-isearch ()
  "Test cpio-dired-do-isearch.
cpio-dired-do-isearch is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-isearch)
		:type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-isearch-regexp ()
  "Test cpio-dired-do-isearch-regexp.
cpio-dired-do-isearch-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-isearch-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-isearch-entry-names ()
  "Test cpio-dired-isearch-entry-names.
cpio-dired-isearch-entry-names is not yet implemented -- expect an error."
  (should-error (cpio-dired-isearch-entry-names)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-isearch-entry-names-regexp ()
  "Test cpio-dired-isearch-entry-names-regexp.
cpio-dired-isearch-entry-names-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-isearch-entry-names-regexp)
     :type 'error))

;;;;;;;; This gets an end-of-buffer error under ERT.
;;;;;;;; (ert-deftest cdmt-odc-cpio-dired-save-archive-0 () ;✓
;;;;;;;;   "Test the function of M-x cpio-dired-save-archive."
;;;;;;;;   (let ((test-name "cdmt-odc-cpio-dired-save-archive")
;;;;;;;;         (cpio-archive-buffer)
;;;;;;;; 	(cpio-archive-buffer-contents-before)
;;;;;;;;         (cpio-archive-buffer-contents)
;;;;;;;;         (cpio-dired-buffer)
;;;;;;;;         (cpio-dired-buffer-contents-before)
;;;;;;;;         (cpio-dired-buffer-contents)
;;;;;;;;         )
;;;;;;;;     (cdmt-odc-reset 'make)

;;;;;;;;     (progn (goto-char (point-min))
;;;;;;;; 	   (re-search-forward " aa$" (point-max))
;;;;;;;; 	   (cpio-dired-do-delete 1)
;;;;;;;; 	   (setq cpio-archive-buffer-contents-before
;;;;;;;; 		 (cdmt-odc-filter-archive-contents (with-current-buffer cpio-archive-buffer
;;;;;;;; 						 (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents-before (with-current-buffer cpio-dired-buffer
;;;;;;;; 						     (buffer-substring-no-properties (point-min) (point-max)))))
    
;;;;;;;;     (should (and "Archive buffer should be modified."
;;;;;;;; 		 (buffer-modified-p cpio-archive-buffer)))
;;;;;;;;     (should (and "Archive buffer should be missing exactly the entry for aa."
;;;;;;;; 		 (string-equal "070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaa	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
00000;;;;;;;; 000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
00000;;;;;;;; 000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
00000;;;;;;;; 000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; b	(( filename ))

;;;;;;;; b

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
00000;;;;;;;; 000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
00000;;;;;;;; 000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bbb

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbbb

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 000000	(( mode     ))
;;;;;;;; 000000	(( uid      ))
;;;;;;;; 000000	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000013	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents-before)))
;;;;;;;;     (should (and "Checking dired-style buffer before saving."
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;;;;;;; 		 (cdmt-odc-filter-archive-contents 
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))
    
;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min)(point)) (file-name-nondirectory *cdmt-odc-small-archive*))
    
;;;;;;;;     (should (and "Archive buffer should no longer be modified."
;;;;;;;; 		 (not (buffer-modified-p cpio-archive-buffer))))
;;;;;;;;     (should (and "Checking the archive buffer after saving."
;;;;;;;; 		 (string-equal "070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaa	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; b	(( filename ))

;;;;;;;; b

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bbb

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbbb

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 000000	(( mode     ))
;;;;;;;; 000000	(( uid      ))
;;;;;;;; 000000	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000013	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents)))

;;;;;;;;     (should (and "Checking the dired-style buffer after saving."
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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

;;;;;;;;     (cdmt-odc-reset)

;;;;;;;;     (progn (goto-char (point-min))
;;;;;;;; 	   (re-search-forward " aaaa$" (point-max))
;;;;;;;; 	   (setq unread-command-events (listify-key-sequence "dddd\n"))
;;;;;;;; 	   (cpio-dired-do-rename 1)
;;;;;;;; 	   (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-odc-filter-archive-contents
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-odc-small-archive*))
    
;;;;;;;;     (should (and "Expecting the standard archive with aaaa moved to ddddd."
;;;;;;;; 		 (string-equal "070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; b	(( filename ))

;;;;;;;; b

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bbb

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbbb

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))

;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; dddd	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 000000	(( mode     ))
;;;;;;;; 000000	(( uid      ))
;;;;;;;; 000000	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000013	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents)))

;;;;;;;;     (should (and "Expecting a dired style buffer without aaaa."
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    
;;;;;;;;     (cdmt-odc-reset)
    
;;;;;;;;     (progn (goto-char (point-min))
;;;;;;;; 	   (re-search-forward " b$" (point-max))
;;;;;;;; 	   (cpio-dired-mark 4)
;;;;;;;; 	   (setq unread-command-events (listify-key-sequence "d\n"))
;;;;;;;; 	   (cpio-dired-do-rename 1)
;;;;;;;; 	   (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-odc-filter-archive-contents
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-odc-small-archive*))

;;;;;;;;     (should (and "Expecting a small archive with d/b, d/bb, d/bbb, d/bbbb."
;;;;;;;; 		 (string-equal "070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaa	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; aaa

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccc	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; ccc

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; dddd	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000007	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; d/bbbb	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; bbbb

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; d/bbb	(( filename ))

;;;;;;;; bbb

;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; d/bb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bb

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000004	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; d/b	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; b

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 000000	(( mode     ))
;;;;;;;; 000000	(( uid      ))
;;;;;;;; 000000	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000013	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents)))

;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-odc-small-archive*))

;;;;;;;;     (should (and "Looking for a small dired-style buffer with d/b, d/bb, d/bbb, d/bbbb"
;;;;;;;; 		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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

;;;;;;;;     (cdmt-odc-reset)

;;;;;;;;     (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
;;;;;;;; 	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
;;;;;;;; 	   ;; HEREHERE This rename does something wrong.
;;;;;;;; 	   (cpio-dired-do-rename 1)
;;;;;;;; 	   (cpio-dired-save-archive)
;;;;;;;; 	   (setq cpio-archive-buffer-contents
;;;;;;;; 		 (cdmt-odc-filter-archive-contents
;;;;;;;; 		  (with-current-buffer cpio-archive-buffer
;;;;;;;; 		    (buffer-substring-no-properties (point-min) (point-max)))))
;;;;;;;; 	   (setq cpio-dired-buffer-contents
;;;;;;;; 		 (with-current-buffer cpio-dired-buffer
;;;;;;;; 		   (buffer-substring-no-properties (point-min) (point-max)))))

;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-odc-small-archive*))

;;;;;;;;     (should (string-equal "070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; a	(( filename ))

;;;;;;;; a

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa	(( filename ))

;;;;;;;; aaaaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; aaaaa.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb	(( filename ))

;;;;;;;; bbbbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; bbbbb.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000002	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; c	(( filename ))

;;;;;;;; c

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000003	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cc	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; cc

;;;;;;;; \\0\\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; cccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; cccc

;;;;;;;; \\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000010	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000006	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc	(( filename ))

;;;;;;;; ccccc

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 040755	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000002	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000010	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; ccccc.d	(( filename ))
;;;;;;;; \\0\\0070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000005	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; dddd	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000007	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000022	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; newDirectory/bbbb	(( filename ))

;;;;;;;; bbbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000021	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; newDirectory/bbb	(( filename ))
;;;;;;;; \\0
;;;;;;;; bbb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000005	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000020	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; newDirectory/bb	(( filename ))
;;;;;;;; \\0\\0
;;;;;;;; bb

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000004	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000017	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; newDirectory/b	(( filename ))
;;;;;;;; \\0\\0\\0
;;;;;;;; b

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000021	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; newDirectory/ccc	(( filename ))
;;;;;;;; \\0
;;;;;;;; ccc

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 100644	(( mode     ))
;;;;;;;; 001750	(( uid      ))
;;;;;;;; 001750	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000006	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000021	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; newDirectory/aaa	(( filename ))
;;;;;;;; \\0
;;;;;;;; aaa

;;;;;;;; 070707	(( magic    ))
;;;;;;;; DEADBE	(( ino      ))
;;;;;;;; 000000	(( mode     ))
;;;;;;;; 000000	(( uid      ))
;;;;;;;; 000000	(( gid      ))
;;;;;;;; 000001	(( nlink    ))
;;;;;;;; DEADBE	(( mtime    ))
;;;;;;;; 00000000000	(( filesize ))
;;;;;;;; DEADBE	(( dev maj  ))
;;;;;;;; DEADBE	(( dev min  ))
;;;;;;;; DEADBE	(( rdev maj ))
;;;;;;;; DEADBE	(( rdev min ))
;;;;;;;; 000013	(( namesize ))
;;;;;;;; 000000	(( chksum   ))
;;;;;;;; TRAILER!!!	(( filename ))
;;;;;;;; \\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0" cpio-archive-buffer-contents))

;;;;;;;;     (should (= 0 1))

;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-odc-small-archive*))

;;;;;;;;     (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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

;;;;;;;;     ;; (cdmt-odc-do-cpio-id (count-lines (point-min) (point-max)) (file-name-nondirectory *cdmt-odc-small-archive*))

;;;;;;;;     ))

(ert-deftest cdmt-odc-cpio-describe-mode ()
  "Test cpio-describe-mode.
cpio-describe-mode is not yet implemented -- expect an error."
  (should-error (cpio-describe-mode)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-add-entry ()
  "Test cpio-dired-add-entry.
cpio-dired-add-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-add-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-change-marks ()
  "Test cpio-dired-change-marks.
cpio-dired-change-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-change-marks)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-clean-directory ()
  "Test cpio-dired-clean-directory.
cpio-dired-clean-directory is not yet implemented -- expect an error."
  (should-error (cpio-dired-clean-directory)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-copy-entry-name-as-kill ()
  (should-error (cpio-dired-copy-entry-name-as-kill 1)
		:type 'error))
  
(ert-deftest NOT-YET-cdmt-odc-cpio-dired-copy-entry-name-as-kill ()
  "Test cpio-dired-copy-entry-name-as-kill.
cpio-dired-copy-entry-name-as-kill is not yet implemented -- expect an error."
  (let ((test-name "cdmt-odc-cpio-dired-copy-entry-name-as-kill")
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
    (cdmt-odc-reset 'make)
    
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

(ert-deftest cdmt-odc-cpio-dired-diff ()
  "Test cpio-dired-diff) ;.
cpio-dired-diff) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-diff) ;)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-display-entry () ;✓
  "Test the function of M-x cpio-dired-display-entry."
  (let ((test-name "cdmt-odc-cpio-dired-display-entry")
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
    (cdmt-odc-reset 'make)

    (set-buffer (setq cpio-dired-buffer (get-buffer (cpio-dired-buffer-name *cdmt-odc-small-archive*))))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq entry-name "aaa")
	   (goto-char (point-min))
	   (cpio-dired-goto-entry entry-name)

	   (cpio-dired-display-entry)

	   ;; (cpio-dired-display-entry) changes the current buffer.
	   (with-current-buffer cpio-dired-buffer
	     (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name entry-name)))
	     (setq cpio-contents-buffer-string (with-current-buffer cpio-contents-buffer
						 (buffer-substring-no-properties (point-min)
										 (point-max))))
	     (setq cpio-contents-window (get-buffer-window cpio-contents-buffer))
	     (setq cpio-archive-buffer-contents
		   (cdmt-odc-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))
	     (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog))))))

    (with-current-buffer cpio-dired-buffer
;;      (should (and "Viewing an entry should not change the archive buffer."
	(should	   (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
      (should (and "Viewing an entry should not change the dired-style buffer."
		   (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
      (should (and "The contents buffer should not be null."
		   (not (null cpio-contents-buffer))))
      (should (and "The contents buffer should be live."
		   (buffer-live-p cpio-contents-buffer)))
      (should (and "Check the entry's contents buffer."
		   (string-equal cpio-contents-buffer-string "\naaa\n\n")))
      (should (and "The entry's contents' window should be live."
		   (window-live-p cpio-contents-window)))
      (should (and "Expecting no change to the catalog."
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

    (cdmt-odc-reset)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq entry-name "ccc")
	   (goto-char (point-min))
	   (re-search-forward " ccc$" (point-max))
	   (cpio-dired-display-entry)
	   
	   (with-current-buffer cpio-dired-buffer
	     (setq cpio-contents-buffer (get-buffer (cpio-contents-buffer-name entry-name)))
	     (setq cpio-contents-buffer-string (with-current-buffer cpio-contents-buffer
						 (buffer-substring-no-properties (point-min)
										 (point-max))))
	     (setq cpio-contents-window (get-buffer-window cpio-contents-buffer))
	     (setq cpio-archive-buffer-contents
		   (cdmt-odc-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))
	     (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog))))))

    (with-current-buffer cpio-dired-buffer
      (should (and "Checking the archive buffer."
		   (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
      (should (and "Checking the dired-style buffer."
		   (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents)))
      (should (not (null cpio-contents-buffer)))
      (should (buffer-live-p cpio-contents-buffer))
      (should (string-equal cpio-contents-buffer-string "\nccc\n\n"))
      (should (window-live-p cpio-contents-window))
      (should (and "Expecting no change to the catalog."
		   (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))))

(ert-deftest cdmt-odc-cpio-dired-do-async-shell-command ()
  "Test cpio-dired-do-async-shell-command) ;.
cpio-dired-do-async-shell-command) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-async-shell-command)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-chgrp () ;✓
  "Test the function of M-x cpio-dired-do-chgrp."
  (let ((test-name "cdmt-odc-cpio-dired-do-chgrp")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-move-to-first-entry)
	   (setq unread-command-events (listify-key-sequence "9999\n"))
	   (cpio-dired-do-chgrp 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    (should (and "Expecting an unchanged archive. (8814)"
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting 'a' to have group 9999."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 9999 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
"
 cpio-catalog-contents-after))

    (cdmt-odc-reset)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq unread-command-events (listify-key-sequence "8888\n"))
	   (cpio-dired-do-chgrp 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "The archive buffer doesn't change until saving."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting the first 4 entries to have group 8888."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-match"((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 8888 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-reset)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777\n"))
	   (cpio-dired-do-chgrp 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "The archive is not changed until saved. (8894)"
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting \`...\' to have group 7777."
		  (string-match "CPIO archive: alphabet_small.odc.cpio:

  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aa
  -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaa
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} aaaaa.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} b
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bb
  -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbb
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} bbbbb.d
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} c
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        5 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cc
  -rw-r--r--   1  [[:digit:]]+  7777        6 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        7 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} cccc
  -rw-r--r--   1  [[:digit:]]+  [[:digit:]]+        8 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc
  drwxr-xr-x   2  [[:digit:]]+  [[:digit:]]+        0 \\(?:a\\(?:pr\\|ug\\)\\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} ccccc.d
" cpio-dired-buffer-contents)))
    (should (and "Expecting \`...\' to have group 7777."
		 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 7777 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 7777 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ 7777 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))
    
    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-chmod ()
  "Test cpio-dired-do-chmod."
  (let ((test-name "cmt-cpio-dired-do-chmod")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    
    (cdmt-odc-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq unread-command-events (listify-key-sequence "0755\n"))
	   (cpio-dired-do-chmod 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "Expecting the first entry to have mode -rwxr-xr-x."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

  -rwxr-xr-x   1  [[:digit:]]+  [[:digit:]]+        4 \\(?:a\\(?:pr\\|ug\\)\|dec\\|feb\\|j\\(?:an\\|u[ln]\\)\\|ma[ry]\\|nov\\|oct\\|sep\\) [[:digit:]]\\{2\\} [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\} a
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
    (should (and "Expecting a mode of 0755 on the first entry."
		 (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents)))

        (should (and "Expecting a mode of 0755 (33261) on the first entry."
		     (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "0600\n"))
	   (cpio-dired-do-chmod 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting aaa, aaaa, aaaaa to have mode -rw------."
		 "Expecting aaaaa.d to have mode dr--------."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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

    (should (and "Expecting an unchanged archive buffer."
		 "The archive is not modified until saving."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting aaa, aaaa, aaaaa to have mode 0100600 (33152)."
		 "Expecting aaaaa.d to have mode 040600 (16768)."
		 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16768 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))
	
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "0660\n"))
	   (cpio-dired-do-chmod)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting ... to have mode -rw-rw----."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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

    (should (and "Expecting an untouched archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (and "Expecting ... to have mode 0100660 (33200)."
		 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16768 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-save-archive)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
;;    (should (and "Expecting all the above mode changes in the archive buffer."
;; 		 "• a has mode 0100755 (100755)."
;; 		 "• aaa, aaaa, aaaaa have mode 0100600 (100600)."
;; 		 "• aaaaa.d has mode 040600 (040600)."
;; 		 "• ... have mode 0660 (100660 for files or 040660 for directories)."
		 (should (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100660	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100600	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100600	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040600	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100660	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100660	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents))
    (should (and "Expecting all the above mode changes in the dired buffer."
 		 "• a has mode -rwxr-xr-x"
 		 "• aaa, aaaa, aaaaa, aaaaa.d have mode -rw-r--r--"
 		 "• ... have mode -rw-rw----"
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting all the above mode changes in the catalog."
;;		 "• a has mode 33261."
;;		 "• aaa, aaaa, aaaaa have mode 33152."
;;		 "• aaaaa.d has mode 16768."
;;		 "• ... have mode 33200 for files."
;;		 "                16816 for directories."
	(should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33261 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33152 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16768 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33200 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-chown () ;✓
  "Test the function of M-x cpio-do-chown."
  (let ((test-name "cdmt-odc-cpio-dired-do-chown")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    
    (cdmt-odc-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq unread-command-events (listify-key-sequence "9999\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "The archive buffer is not modified until saved. (10741)"
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (and "Expecting a cpio-dired buffer with the owner of 'a' being 9999."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (and "The owner of 'a' should be 9999."
		 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 9999 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))
    (cdmt-odc-reset)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "8888\n"))
	   (cpio-dired-do-chown 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "The archive buffer is not modified until saved. (11111)"
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (and "Expecting 4 entries with owner 8888."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 8888 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 8888 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 335 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 8888 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 8888 [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-reset)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    (should (and "The archive buffer is not modified until saved. (10818)"
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (and "Expecting \`...\' to be owned by 7777."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
" cpio-dired-buffer-contents)))
    (should (and "Expecting ... to be owned by 7777."
		 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 7777 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 7777 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 7777 [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-chown-1 ()
  "Test the change-owner-user function of M-x cpio-dired-do-chown."
  (let ((test-name "cdmt-odc-cpio-dired-do-chown")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq unread-command-events (listify-key-sequence "9999:1111\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))

    (should (and "Expecting entry 'a' to have owner 9999 and group 1111."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting entry 'a' to have owner 9999 and group 1111."
    (should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 9999 1111 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-reset)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "8888:2222\n"))
	   (cpio-dired-do-chown 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an untouched archive. (9918)"
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting 4 entries with owner 8888 and group 2222."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting 4 entries with owner 8888 and group 2222."
    (should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 8888 2222 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 8888 2222 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 335 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 8888 2222 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 8888 2222 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-reset)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "7777:3333\n"))
	   (cpio-dired-do-chown 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an untouched archive. (9958)"
		 (string-equal *cdmt-odc-untouched-small-archive*  cpio-archive-buffer-contents)))
    (should (and "Expecting \`...\' to have owner 7777 and group 3333."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
" cpio-dired-buffer-contents)))
;;    (should (and "Expecting ... to have owner 7777 and group 3333."
    (should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 7777 3333 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 7777 3333 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 7777 3333 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-compress ()
  "Test cpio-dired-do-compress.
cpio-dired-do-compress is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-compress)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-copy-0 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-odc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq unread-command-events (listify-key-sequence "d\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
;;  (should (and "Checking that entry »a« has been copied to »d«." 
    (should (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
d	(( filename ))

a


" cpio-archive-buffer-contents))
    (should (and "Checking that there is an entry »d« in the dired style buffer."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting to see an entry »d«."
	(should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"d\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"d\"]
   #<marker at 1543 in alphabet_small.odc.cpio> #<marker at 1621 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-copy-1 () ;✓
  "Test the function of M-x cpio-do-copy."
  :expected-result :failed
  (let ((test-name "cdmt-odc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
	   (cpio-dired-do-copy 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    ;; HEREHERE I think that this expected value is incorrect.
    ;; It implies an archive with two entries
    ;; whose padding does not end after a (mod ... *cpio-padding-modulus*) = 0
    ;; character position.
    ;; However, cpio(1GNU) seems to cope with it.
    ;; Note the use of the term character position, since cpio counts characters
    ;; starting at 0, while emacs starts counting (point) at 1.
    (should (and "Checking for »aaa«, »aaaa«, »aaaaa«, »aaaaa« copied to newDirectory in the archive."
		 (string-equal "uNlIkElY" cpio-archive-buffer-contents)))

    (should (and "Checking for the presence of »newDirectory/aaa«, »newDirectory/aaaa«, »newDirectory/aaaaa«, »newDirectory/aaaaa«."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-copy-2 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-odc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-odc-reset 'make)

    (progn (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "newDirectory-1\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "Expecting an archive with each 3 letter entry copied to newDirectory-1."
		 (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000023	(( namesize ))
000000	(( chksum   ))
newDirectory-1/ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000023	(( namesize ))
000000	(( chksum   ))
newDirectory-1/bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000023	(( namesize ))
000000	(( chksum   ))
newDirectory-1/aaa	(( filename ))

aaa


" cpio-archive-buffer-contents)))
    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting to see ... entries in newDirectory-1."
	(should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-1/ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-1/ccc\"]
   #<marker at 1543 in alphabet_small.odc.cpio> #<marker at 1638 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-1/bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-1/bbb\"]
   #<marker at 1644 in alphabet_small.odc.cpio> #<marker at 1739 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-1/aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-1/aaa\"]
   #<marker at 1745 in alphabet_small.odc.cpio> #<marker at 1840 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-copy-3 () ;✓
  "Test the function of M-x cpio-do-copy."
  (let ((test-name "cdmt-odc-cpio-dired-do-copy")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "...")
	   (setq unread-command-events (listify-key-sequence "newDirectory-3\n"))
	   (cpio-dired-do-copy 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "Expecting an archive with each entry named with at least 3 letters copied to newDirectory-3."
		 (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000027	(( namesize ))
000000	(( chksum   ))
newDirectory-3/ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000025	(( namesize ))
000000	(( chksum   ))
newDirectory-3/ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000024	(( namesize ))
000000	(( chksum   ))
newDirectory-3/cccc	(( filename ))

cccc

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000023	(( namesize ))
000000	(( chksum   ))
newDirectory-3/ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000027	(( namesize ))
000000	(( chksum   ))
newDirectory-3/bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000025	(( namesize ))
000000	(( chksum   ))
newDirectory-3/bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000024	(( namesize ))
000000	(( chksum   ))
newDirectory-3/bbbb	(( filename ))

bbbb

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000023	(( namesize ))
000000	(( chksum   ))
newDirectory-3/bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000027	(( namesize ))
000000	(( chksum   ))
newDirectory-3/aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000025	(( namesize ))
000000	(( chksum   ))
newDirectory-3/aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000024	(( namesize ))
000000	(( chksum   ))
newDirectory-3/aaaa	(( filename ))

aaaa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000023	(( namesize ))
000000	(( chksum   ))
newDirectory-3/aaa	(( filename ))

aaa


" cpio-archive-buffer-contents)))
    
    (should (and "Expecting all entries named with at least 3 letters to have copies in newDirectory-3."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting all entries named with at least 3 letters to have copies in newDirectory-3."
	(should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 23 0 \"newDirectory-3/ccccc\.d\"]
   #<marker at 1543 in alphabet_small.odc.cpio> #<marker at 1642 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 \"newDirectory-3/ccccc\"]
   #<marker at 1642 in alphabet_small.odc.cpio> #<marker at 1739 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 20 0 \"newDirectory-3/cccc\"]
   #<marker at 1747 in alphabet_small.odc.cpio> #<marker at 1843 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-3/ccc\"]
   #<marker at 1851 in alphabet_small.odc.cpio> #<marker at 1946 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 23 0 \"newDirectory-3/bbbbb\.d\"]
   #<marker at 1952 in alphabet_small.odc.cpio> #<marker at 2051 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 \"newDirectory-3/bbbbb\"]
   #<marker at 2051 in alphabet_small.odc.cpio> #<marker at 2148 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 20 0 \"newDirectory-3/bbbb\"]
   #<marker at 2156 in alphabet_small.odc.cpio> #<marker at 2252 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-3/bbb\"]
   #<marker at 2260 in alphabet_small.odc.cpio> #<marker at 2355 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 23 0 \"newDirectory-3/aaaaa\.d\"]
   #<marker at 2361 in alphabet_small.odc.cpio> #<marker at 2460 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 \"newDirectory-3/aaaaa\"]
   #<marker at 2460 in alphabet_small.odc.cpio> #<marker at 2557 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 20 0 \"newDirectory-3/aaaa\"]
   #<marker at 2565 in alphabet_small.odc.cpio> #<marker at 2661 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-3/aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-3/aaa\"]
   #<marker at 2669 in alphabet_small.odc.cpio> #<marker at 2764 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-copy-regexp ()
  "Test cpio-dired-do-copy-regexp.
cpio-dired-do-copy-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-copy-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-delete () ;✓
  "Test the function of M-x cpio-dired-do-delete."
  (let ((test-name "cdmt-odc-cpio-dired-do-delete")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-do-delete 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

;;    (should (and "Expecting entry »a« to be deleted."
    (should	 (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents))

    (should (and "Expecting entry »a« to be deleted."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (and "Expecting a catalog with entry »a« deleted."
		 (string-match "((\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 80 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 85 in alphabet_small.odc.cpio> #<marker at 165 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 171 in alphabet_small.odc.cpio> #<marker at 252 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 259 in alphabet_small.odc.cpio> #<marker at 341 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 349 in alphabet_small.odc.cpio> #<marker at 433 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 433 in alphabet_small.odc.cpio> #<marker at 511 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 594 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 599 in alphabet_small.odc.cpio> #<marker at 679 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 685 in alphabet_small.odc.cpio> #<marker at 766 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 773 in alphabet_small.odc.cpio> #<marker at 855 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 863 in alphabet_small.odc.cpio> #<marker at 947 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 947 in alphabet_small.odc.cpio> #<marker at 1025 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1108 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1113 in alphabet_small.odc.cpio> #<marker at 1193 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1199 in alphabet_small.odc.cpio> #<marker at 1280 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1287 in alphabet_small.odc.cpio> #<marker at 1369 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1377 in alphabet_small.odc.cpio> #<marker at 1461 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 2)
	   (cpio-dired-do-delete 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents))

    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting a catalog with entries"
;;		 "    »aaaa«, »aaaaa«, »aaaaa.d« and »b« deleted."
	(should	 (string-match "((\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 80 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 85 in alphabet_small.odc.cpio> #<marker at 165 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 171 in alphabet_small.odc.cpio> #<marker at 250 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 255 in alphabet_small.odc.cpio> #<marker at 335 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 422 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 429 in alphabet_small.odc.cpio> #<marker at 511 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 519 in alphabet_small.odc.cpio> #<marker at 603 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 603 in alphabet_small.odc.cpio> #<marker at 681 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 685 in alphabet_small.odc.cpio> #<marker at 764 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 769 in alphabet_small.odc.cpio> #<marker at 849 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 936 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 943 in alphabet_small.odc.cpio> #<marker at 1025 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1033 in alphabet_small.odc.cpio> #<marker at 1117 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
"
cpio-catalog-contents-after))

  (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "\n"))
	   (cpio-dired-do-delete 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
000000	(( mode     ))
000000	(( uid      ))
000000	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000013	(( namesize ))
000000	(( chksum   ))
TRAILER!!!	(( filename ))
\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0
" cpio-archive-buffer-contents))

    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting a catalog with further entries \`...\' deleted."
	(should	 (string-match "((\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 80 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 85 in alphabet_small.odc.cpio> #<marker at 164 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 169 in alphabet_small.odc.cpio> #<marker at 250 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 257 in alphabet_small.odc.cpio> #<marker at 339 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 347 in alphabet_small.odc.cpio> #<marker at 431 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 509 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 513 in alphabet_small.odc.cpio> #<marker at 592 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 678 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 685 in alphabet_small.odc.cpio> #<marker at 767 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 775 in alphabet_small.odc.cpio> #<marker at 859 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-flagged-delete ()
  "Test cpio-dired-do-flagged-delete.
cpio-dired-do-flagged-delete is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-flagged-delete)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-hardlink ()
  "Test cpio-dired-do-hardlink.
cpio-dired-do-hardlink is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-hardlink)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-hardlink-regexp ()
  "Test cpio-dired-do-hardlink-regexp.
cpio-dired-do-hardlink-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-hardlink-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-kill-lines ()
  "Test cpio-dired-do-kill-lines.
cpio-dired-do-kill-lines is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-kill-lines)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-print ()
  "Test cpio-dired-do-print.
cpio-dired-do-print is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-print)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-query-replace-regexp ()
  "Test cpio-dired-do-query-replace-regexp.
cpio-dired-do-query-replace-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-query-replace-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-redisplay ()
  "Test cpio-dired-do-redisplay.
cpio-dired-do-redisplay is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-redisplay)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-rename () ;✓
  (let ((test-name "cdmt-odc-cpio-dired-do-rename")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-odc-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq unread-command-events (listify-key-sequence "d\n"))
	   (cpio-dired-do-rename 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an untouched archive."
		 "The archive gets updated on save."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (and "Expecting a dired buffer with no entry »a«, but an entry »d«."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    
;;    (should (and "Expecting catalog with first entry »d«."
    (should	 (string-match "((\"d\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"d\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
"
 cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-line 2)
	   (setq unread-command-events (listify-key-sequence "newDirectory\n"))
	   (cpio-dired-do-rename 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an as yet unchanged archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    
    (should (and "Expecting a dired style buffer with entries »aaaa«, »aaaaa«, »aaaaa.d« and »b« moved to »newDirectory«."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
	
;;    (should (and "Expecting a catalog with the above changes."
    (should (string-match "((\"d\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"d\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 17 0 \"newDirectory/aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 18 0 \"newDirectory/aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory/aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 \"newDirectory/aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq unread-command-events (listify-key-sequence "newDirectory-1\n"))
	   (cpio-dired-do-rename 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an as yet unchanged archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))

    (should (and "Expecting a dired buffer with \`...\' all under newDirectory-1."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting a catalog with \`...\' entries in newDirectory-1."
		 (should		 (string-match "((\"d\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"d\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 17 0 \"newDirectory/aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 18 0 \"newDirectory/aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory/aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory/aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 21 0 \"newDirectory/aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-1/bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-1/bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"newDirectory-1/ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 19 0 \"newDirectory-1/ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-do-rename-regexp ()
  "Test cpio-dired-do-rename-regexp.
cpio-dired-do-rename-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-rename-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-search () ;HEREHERE ()
  "Test cpio-dired-do-search) ;HEREHERE.
cpio-dired-do-search) ;HEREHERE is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-search)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-symlink ()
  "Test cpio-dired-do-symlink.
cpio-dired-do-symlink is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-symlink)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-symlink-regexp ()
  "Test cpio-dired-do-symlink-regexp.
cpio-dired-do-symlink-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-symlink-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-do-touch ()
  "Test cpio-dired-do-touch.
cpio-dired-do-touch is not yet implemented -- expect an error."
  (should-error (cpio-dired-do-touch)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-downcase ()
  "Test cpio-dired-downcase.
cpio-dired-downcase is not yet implemented -- expect an error."
  (should-error (cpio-dired-downcase)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-extract-all ()
  "Test cpio-dired-extract-all.
cpio-dired-extract-all is not yet implemented -- expect an error."
  (should-error (cpio-dired-extract-all)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-extract-entries ()
  "Test cpio-dired-extract-entries.
cpio-dired-extract-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-extract-entries)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-find-alternate-entry ()
  "Test cpio-dired-find-alternate-entry.
cpio-dired-find-alternate-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-find-alternate-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-find-entry () ;✓
  "Test the function of M-x cpio-find-entry.
Expect errors about killed buffers.
They reflect an outstanding bug in cpio-affiliated buffers."

  (let ((test-name "cdmt-odc-cpio-dired-find-entry")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-contents-window)
	(entry-name)
	(past-entries ()))
    (cdmt-odc-reset 'make)
    
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
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
		   (cdmt-odc-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))
	     (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog))))))


    (with-current-buffer cpio-dired-buffer
      (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
      (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
      (should (not (null cpio-contents-buffer)))
      (should (buffer-live-p cpio-contents-buffer))
      (should (string-equal cpio-contents-buffer-string (concat "\n" entry-name "\n\n")))
      (should (window-live-p cpio-contents-window))
      (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

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
		   (cdmt-odc-filter-archive-contents
		    (with-current-buffer cpio-archive-buffer
		      (buffer-substring-no-properties (point-min) (point-max)))))
	     (setq cpio-dired-buffer-contents
		   (with-current-buffer cpio-dired-buffer
		     (buffer-substring-no-properties (point-min) (point-max))))))

    (with-current-buffer cpio-dired-buffer
      (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
      (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
      (should (not (null cpio-contents-buffer)))
      (should (buffer-live-p cpio-contents-buffer))
      (should (string-equal cpio-contents-buffer-string "\nccc\n\n"))
      (should (window-live-p cpio-contents-window))
      (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

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

(ert-deftest cdmt-odc-cpio-dired-find-entry-other-window ()
  "Test cpio-dired-find-entry-other-window.
cpio-dired-find-entry-other-window is not yet implemented -- expect an error."
  (should-error (cpio-dired-find-entry-other-window)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-flag-auto-save-entries () ;✓
  "Test the function of M-x cpio-dired-flag-auto-save-entries."
  (let ((test-name "cdmt-odc-cpio-dired-flag-auto-save-entries")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (beginning-of-line)
	   (while (re-search-forward " \\(.\\)$" (point-max) t)
	     (setq unread-command-events (listify-key-sequence (concat "#" (match-string-no-properties 1) "\n")))
	     (cpio-dired-do-copy 1))
	   (cpio-dired-flag-auto-save-entries)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

;;     (should (and "Expecting an archive with autosave entries"
;;		  "for each single character entry."
;;		  "(The copy used to create them must update the archive.)"
    (should	  (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
#a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
#b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
#c	(( filename ))

c


" cpio-archive-buffer-contents))

    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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

    (should (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"#a\" .
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 64768 0 0 0 3 0 \"#a\"]
   #<marker at 1543 in alphabet_small.odc.cpio> #<marker at 1622 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"#b\" .
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 64768 0 0 0 3 0 \"#b\"]
   #<marker at 1626 in alphabet_small.odc.cpio> #<marker at 1705 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"#c\" .
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 64768 0 0 0 3 0 \"#c\"]
   #<marker at 1709 in alphabet_small.odc.cpio> #<marker at 1788 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-flag-backup-entries ()
  "Test cpio-dired-flag-backup-entries.
cpio-dired-flag-backup-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-backup-entries)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-flag-entries-regexp ()
  "Test cpio-dired-flag-entries-regexp.
cpio-dired-flag-entries-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-flag-entries-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-flag-entry-deletion () ;✓
  "Test the function of M-x cpio-flag-entry-deletion."
  (let ((test-name "cdmt-odc-cpio-dired-flag-entry-deletion")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-flag-entry-deletion 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 2)
	   (cpio-dired-flag-entry-deletion 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-flag-garbage-entries ()
  "Test cpio-dired-flag-garbage-entries."
  (let ((test-name "cdmt-odc-cpio-dired-flag-entry-deletion")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(entry-name "aa"))

    (cdmt-odc-reset 'make)
    
    (progn (setq cpio-dired-catalog-contents-before (cpio-catalog))
	   (cpio-dired-goto-entry entry-name)
	   (mapc (lambda (s)		;suffix
		   (setq unread-command-events (listify-key-sequence (concat entry-name "." s "\n")))
		   (cpio-dired-do-copy 1))
		 (list "aux" "bak" "dvi" "log" "orig" "rej" "toc"))
	   (cpio-dired-flag-garbage-entries)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
;;    (should (and "Expecting an archive with entries for suffixes"
    ;; "    aux bak dvi log orig reg toc."
    (should	 (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000007	(( namesize ))
000000	(( chksum   ))
aa.aux	(( filename ))

aa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000007	(( namesize ))
000000	(( chksum   ))
aa.bak	(( filename ))

aa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000007	(( namesize ))
000000	(( chksum   ))
aa.dvi	(( filename ))

aa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000007	(( namesize ))
000000	(( chksum   ))
aa.log	(( filename ))

aa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aa.orig	(( filename ))

aa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000007	(( namesize ))
000000	(( chksum   ))
aa.rej	(( filename ))

aa

\\0070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000007	(( namesize ))
000000	(( chksum   ))
aa.toc	(( filename ))

aa

\\0
" cpio-archive-buffer-contents))

    (should (and "Expecting a dired-style buffer with marked entries"
		 "    for the suffixes"
		 "    aux bak dvi log orig reg toc."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
;;    (should (and "Expecting a catalog with entries with the suffixes"
;; 		 "    aux bak dvi log orig reg toc."
    (should	 (string-match "((\"a\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\.d\" \.
  \[\[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.aux\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 \"aa\.aux\"]
   #<marker at 1543 in alphabet_small.odc.cpio> #<marker at 1626 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.bak\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 \"aa\.bak\"]
   #<marker at 1632 in alphabet_small.odc.cpio> #<marker at 1715 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.dvi\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 \"aa\.dvi\"]
   #<marker at 1721 in alphabet_small.odc.cpio> #<marker at 1804 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.log\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 \"aa\.log\"]
   #<marker at 1810 in alphabet_small.odc.cpio> #<marker at 1893 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.orig\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aa\.orig\"]
   #<marker at 1899 in alphabet_small.odc.cpio> #<marker at 1983 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.rej\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 \"aa\.rej\"]
   #<marker at 1989 in alphabet_small.odc.cpio> #<marker at 2072 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\.toc\" \.
  \[\[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 7 0 \"aa\.toc\"]
   #<marker at 2078 in alphabet_small.odc.cpio> #<marker at 2161 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after))

    (cdmt-odc-test-save)))

(ert-deftest cdmt-odc-cpio-dired-goto-entry ()
  "Test cpio-dired-goto-entry.
cpio-dired-goto-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-goto-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-hide-all ()
  "Test cpio-dired-hide-all.
cpio-dired-hide-all is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-all)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-hide-details-mode ()
  "Test cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing.
cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-hide-subdir ()
  "Test cpio-dired-hide-subdir) ;.
cpio-dired-hide-subdir) ; is not yet implemented -- expect an error."
  (should-error (cpio-dired-hide-subdir)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-mark () ;✓
  "Test the function of M-x cpio-dired-mark."
  (let ((test-name "cdmt-odc-cpio-dired-mark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 2)
	   (cpio-dired-mark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal cpio-archive-buffer-contents *cdmt-odc-untouched-small-archive*))
    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-mark-directories ()
  "Test cpio-dired-mark-directories.
cpio-dired-mark-directories is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-directories)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-mark-entries-containing-regexp ()
  "Test cpio-dired-mark-entries-containing-regexp.
cpio-dired-mark-entries-containing-regexp is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-entries-containing-regexp)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-mark-entries-regexp () ;✓
  (let ((test-name "cdmt-odc-cpio-dired-mark-entries-regexp")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp "\\`...\\'")
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-mark-executables ()
  "Test cpio-dired-mark-executables.
cpio-dired-mark-executables is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-executables)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-mark-subdir-entries ()
  "Test cpio-dired-mark-subdir-entries.
cpio-dired-mark-subdir-entries is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-subdir-entries)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-mark-symlinks ()
  "Test cpio-dired-mark-symlinks.
cpio-dired-mark-symlinks is not yet implemented -- expect an error."
  (should-error (cpio-dired-mark-symlinks)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-mouse-find-entry-other-window ()
  "Test cpio-dired-mouse-find-entry-other-window.
cpio-dired-mouse-find-entry-other-window is not yet implemented -- expect an error."
  (should-error (cpio-dired-mouse-find-entry-other-window)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-next-dirline () ;✓
  "Test the function of M-x cpio-dired-next-dirline."
  (let ((test-name "cdmt-odc-cpio-dired-next-dirline")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(entry-name))
    (cdmt-odc-reset 'make 'large)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-dirline 1)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "The current entry should be aaaaa.d"
		 (string-equal "aaaaa.d" entry-name)))
    ;;(should (and "Expecting an untouched large archive buffer."
	(should	 (string-equal *cdmt-odc-untouched-large-archive-buffer* cpio-archive-buffer-contents))
    (should (and "The dired style buffer should be untouched."
		 (string-match *cdmt-odc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "Expecting an unchanged catalog."
		 (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-dirline 2)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "The current entry should be ccccc.d"
		 (string-equal "ccccc.d" entry-name)))
    (should (and "The archive buffer should be untouched. (1)"
		 (string-equal *cdmt-odc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (1)"
		 (string-match *cdmt-odc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "Expecting an unchanged catalog."
		 (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-dirline 4)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "The current entry should be ggggg.d"
		 (string-equal "ggggg.d" entry-name)))
    (should (and "The archive buffer should be untouched. (2)"
		 (string-equal *cdmt-odc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer shouold be untouched (2)"
		 (string-match *cdmt-odc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "Expecting an unchanged catalog."
		 (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-dirline 8)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "The current entry should be ooooo.d."
		 (string-equal "ooooo.d" entry-name)))
    (should (and "The archive buffer should be untouched. (3)"
		 (string-equal *cdmt-odc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (3)"
		 (string-match *cdmt-odc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "Expecting an unchanged catalog."
		 (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-dirline 16)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "The current entry should be zzzzz.d."
		 (string-equal "zzzzz.d" entry-name)))
    (should (and "The archive buffer should be untouched. (4)"
		 (string-equal *cdmt-odc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (4)"
		 (string-match *cdmt-odc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "Expecting an unchanged catalog."
		 (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-dirline 1)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "The current entry should still be zzzzz.d."
		 (string-equal "zzzzz.d" entry-name)))
    (should (and "The archive buffer should be untouched. (5)"
		 (string-equal *cdmt-odc-untouched-large-archive-buffer* cpio-archive-buffer-contents)))
    (should (and "The dired style buffer should be untouched. (5)"
		 (string-match *cdmt-odc-untouched-large-dired-buffer* cpio-dired-buffer-contents)))
    (should (and "Expecting an unchanged catalog."
		 (string-equal cpio-catalog-contents-before cpio-catalog-contents-after)))))

(ert-deftest cdmt-odc-cpio-dired-next-line () ;✓
  "Test the function of M-x cpio-dired-next-line."
  (let ((test-name "cdmt-odc-cpio-dired-next-line")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(entry-name))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (string-equal "a" entry-name))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-line 2)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal "aaa" entry-name))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-line 4)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal "b" entry-name))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-line 100)
	   (setq entry-name (cpio-dired-get-entry-name))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (equal nil entry-name))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-next-marked-entry ()
  "Test cpio-dired-next-marked-entry.
cpio-dired-next-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-next-marked-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-next-marked-entry ()
  "Test cpio-dired-next-marked-entry.
cpio-dired-next-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-next-marked-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-next-subdir ()
  "Test the function of M-x cpio-next-subdir."
  (should-error (cpio-dired-next-marked-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-prev-marked-entry ()
  "Test cpio-dired-prev-marked-entry.
cpio-dired-prev-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-prev-marked-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-prev-marked-entry ()
  "Test cpio-dired-prev-marked-entry.
cpio-dired-prev-marked-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-prev-marked-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-prev-subdir ()
  "Test the function of M-x cpio-dired-prev-subdir."
  (should-error (cpio-dired-previous-line)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-previous-line () ;✓
  (let ((test-name "cdmt-odc-cpio-dired-previous-line")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(where))
    (cdmt-odc-reset)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (goto-char (point-max))
	   (cpio-dired-previous-line 1)
	   (setq where (point))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (= where 1155))
    (should (string-match *cdmt-odc-untouched-small-archive-buffer* cpio-archive-buffer-contents))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-previous-line 2)
	   (setq where (point))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents 
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (= where 1019))
    (should (string-match *cdmt-odc-untouched-small-archive-buffer* cpio-archive-buffer-contents))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-previous-line 4)
	   (setq where (point))
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (= where 774))
    (should (string-match *cdmt-odc-untouched-small-archive-buffer* cpio-archive-buffer-contents))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-previous-line ()
  "Test cpio-dired-previous-line.
cpio-dired-previous-line is not yet implemented -- expect an error."
  (should-error (cpio-dired-previous-line)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-previous-line ()
  "Test cpio-dired-previous-line.
cpio-dired-previous-line is not yet implemented -- expect an error."
  (should-error (cpio-dired-previous-line)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-show-entry-type ()
  "Test cpio-dired-show-entry-type.
cpio-dired-show-entry-type is not yet implemented -- expect an error."
  (should-error (cpio-dired-show-entry-type)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-sort-toggle-or-edit ()
  "Test cpio-dired-sort-toggle-or-edit.
cpio-dired-sort-toggle-or-edit is not yet implemented -- expect an error."
  (should-error (cpio-dired-sort-toggle-or-edit)
     :type 'error))

;; I'm not sure how to test this.
;; (ert-deftest cdmt-odc-cpio-dired-summary () ;✓
;;   "Test the function of M-x cpio-dired-summary."
;;   (shell-command "cd test_data/alphabet ; make odc" nil nil)
;;   (let ((test-name "cdmt-odc-cpio-dired-summary")
;;         (cpio-archive-buffer (find-file-noselect  *cdmt-odc-small-archive*))
;;         (cpio-archive-buffer-contents)
;;         (cpio-dired-buffer)
;;         (cpio-dired-buffer-contents)
;;         )
;;     (with-current-buffer cpio-archive-buffer
;;       (cpio-mode))
;;     (setq cpio-dired-buffer (get-buffer-create (cpio-dired-buffer-name *cdmt-odc-small-archive*)))
;; 
;;     (should (string-equal (with-output-to-string
;; 			    (cpio-dired-summary))
;; 
;;     ))

(ert-deftest cdmt-odc-cpio-dired-toggle-marks ()
  "Test cpio-dired-toggle-marks.
cpio-dired-toggle-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-marks)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-toggle-marks ()
  "Test cpio-dired-toggle-marks.
cpio-dired-toggle-marks is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-marks)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-toggle-read-only ()
  "Test cpio-dired-toggle-read-only.
cpio-dired-toggle-read-only is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-read-only)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-toggle-read-only ()
  "Test cpio-dired-toggle-read-only.
cpio-dired-toggle-read-only is not yet implemented -- expect an error."
  (should-error (cpio-dired-toggle-read-only)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-undo ()
  "Test cpio-dired-undo.
cpio-dired-undo is not yet implemented -- expect an error."
  (should-error (cpio-dired-undo)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-unmark () ;✓
  "Test the function of M-x cpio-dired-unmark."
  (let ((test-name "cdmt-odc-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-unmark 1)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "Expecting an untouched small archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with every entry except the first marked."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-next-line 2)
	   (cpio-dired-unmark 2)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))
    
    (should (and "Expecting an untouched small archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecing a dired bugger with all but two entries marked."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 4)
	   (cpio-dired-unmark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an untouched small archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with another 4 entries unmarked."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (dired-next-line 4)
	   (cpio-dired-unmark 4)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an untouched archive."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with yet the last entry unmarked."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-unmark-all-entries ()
  "Test cpio-dired-unmark-all-entries."
  (let ((test-name "cdmt-odc-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-move-to-first-entry)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?A)
	   (cpio-dired-mark-this-entry ?B) (cpio-dired-mark-this-entry ?B)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E)
	   (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a variety of marks in a dired-style buffer."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-unmark-all-entries "" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with no marks."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-move-to-first-entry)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?A)
	   (cpio-dired-mark-this-entry ?B) (cpio-dired-mark-this-entry ?B)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E)
	   (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a variety of marks in a dired-style buffer."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-unmark-all-entries "B" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with no B marks."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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

    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-unmark-all-entries "F" nil)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with neither B nor F marks."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-unmark-all-marks ()
  "Test cpio-dired-unmark-all-marks."
  (let ((test-name "cdmt-odc-cpio-dired-unmark")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))
    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-mark-entries-regexp ".")
	   (cpio-dired-move-to-first-entry)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?A)
	   (cpio-dired-mark-this-entry ?B) (cpio-dired-mark-this-entry ?B)
	   (cpio-dired-next-line 2)
	   (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E) (cpio-dired-mark-this-entry ?E)
	   (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F) (cpio-dired-mark-this-entry ?F)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a variety of marks in a dired-style buffer."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))
  
    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-unmark-all-marks)
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting an unchanged archive buffer."
		 (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents)))
    (should (and "Expecting a dired-style buffer with no marks."
		 (string-match "CPIO archive: alphabet_small.odc.cpio:

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
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-unmark-all-marks () ;✓
  "Test the function of M-x cpio-unmark-all-marks."
  (let ((test-name "cdmt-odc-cpio-dired-unmark-all-marks")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after))

    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
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
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (string-equal *cdmt-odc-untouched-small-archive* cpio-archive-buffer-contents))
    (should (string-match *cdmt-odc-untouched-small-dired-buffer* cpio-dired-buffer-contents))
    (should (string-equal cpio-catalog-contents-before cpio-catalog-contents-after))))

(ert-deftest cdmt-odc-cpio-dired-unmark-backward ()
  "Test cpio-dired-unmark-backward.
cpio-dired-unmark-backward is not yet implemented -- expect an error."
  (should-error (cpio-dired-unmark-backward)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-unmark-backward ()
  "Test cpio-dired-unmark-backward.
cpio-dired-unmark-backward is not yet implemented -- expect an error."
  (should-error (cpio-dired-unmark-backward)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-up-directory ()
  "Test cpio-dired-up-directory.
cpio-dired-up-directory is not yet implemented -- expect an error."
  (should-error (cpio-dired-up-directory)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-upcase ()
  "Test cpio-dired-upcase.
cpio-dired-upcase is not yet implemented -- expect an error."
  (should-error (cpio-dired-upcase)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-view-archive () ;✓
  "Test the function of M-x cpio-view-archive."
  (let ((test-name "cdmt-odc-cpio-dired-view-archive")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(cpio-archive-window)
	(cpio-dired-window))
    (cdmt-odc-reset 'make)

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

(ert-deftest cdmt-odc-cpio-dired-view-entry ()
  "Test cpio-dired-view-entry.
cpio-dired-view-entry is not yet implemented -- expect an error."
  (should-error (cpio-dired-view-entry)
     :type 'error))

(ert-deftest cdmt-odc-cpio-epa-dired-do-decrypt ()
  "Test cpio-epa-dired-do-decrypt.
cpio-epa-dired-do-decrypt is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-decrypt)
     :type 'error))

(ert-deftest cdmt-odc-cpio-epa-dired-do-encrypt ()
  "Test cpio-epa-dired-do-encrypt.
cpio-epa-dired-do-encrypt is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-encrypt)
     :type 'error))

(ert-deftest cdmt-odc-cpio-epa-dired-do-sign ()
  "Test cpio-epa-dired-do-sign.
cpio-epa-dired-do-sign is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-sign)
     :type 'error))

(ert-deftest cdmt-odc-cpio-epa-dired-do-verify ()
  "Test cpio-epa-dired-do-verify.
cpio-epa-dired-do-verify is not yet implemented -- expect an error."
  (should-error (cpio-epa-dired-do-verify)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-delete-tag ()
  "Test cpio-image-dired-delete-tag.
cpio-image-dired-delete-tag is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-delete-tag)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-dired-comment-entries ()
  "Test cpio-image-dired-dired-comment-entries.
cpio-image-dired-dired-comment-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-comment-entries)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-dired-display-external ()
  "Test cpio-image-dired-dired-display-external.
cpio-image-dired-dired-display-external is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-display-external)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-dired-display-image ()
  "Test cpio-image-dired-dired-display-image.
cpio-image-dired-dired-display-image is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-display-image)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-dired-edit-comment-and-tags ()
  "Test cpio-image-dired-dired-edit-comment-and-tags.
cpio-image-dired-dired-edit-comment-and-tags is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-edit-comment-and-tags)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-dired-toggle-marked-thumbs ()
  "Test cpio-image-dired-dired-toggle-marked-thumbs.
cpio-image-dired-dired-toggle-marked-thumbs is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-dired-toggle-marked-thumbs)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-display-thumb ()
  "Test cpio-image-dired-display-thumb.
cpio-image-dired-display-thumb is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumb)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-display-thumbs ()
  "Test cpio-image-dired-display-thumbs.
cpio-image-dired-display-thumbs is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumbs)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-display-thumbs-append ()
  "Test cpio-image-dired-display-thumbs-append.
cpio-image-dired-display-thumbs-append is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-display-thumbs-append)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-jump-thumbnail-buffer ()
  "Test cpio-image-dired-jump-thumbnail-buffer.
cpio-image-dired-jump-thumbnail-buffer is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-jump-thumbnail-buffer)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-mark-tagged-entries ()
  "Test cpio-image-dired-mark-tagged-entries.
cpio-image-dired-mark-tagged-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-mark-tagged-entries)
     :type 'error))

(ert-deftest cdmt-odc-cpio-image-dired-tag-entries ()
  "Test cpio-image-dired-tag-entries.
cpio-image-dired-tag-entries is not yet implemented -- expect an error."
  (should-error (cpio-image-dired-tag-entries)
     :type 'error))

(ert-deftest cdmt-odc-cpio-mouse-face ()
  "Test cpio-mouse-face.
cpio-mouse-face is not yet implemented -- expect an error."
  (should-error (cpio-mouse-face)
     :type 'error))

;;;;;;;; (ert-deftest cdmt-odc-cpio-quit-window () ;✓
;;;;;;;;   "Test cpio-quit-window.
;;;;;;;; cpio-quit-window is not yet implemented -- expect an error."
;;;;;;;;   (let ((test-name "cdmt-odc-cpio-dired-quit-window")
;;;;;;;;         (cpio-archive-buffer)
;;;;;;;;         (cpio-archive-buffer-contents)
;;;;;;;;         (cpio-dired-buffer)
;;;;;;;;         (cpio-dired-buffer-contents)
;;;;;;;; 	(cpio-archive-window)
;;;;;;;; 	(cpio-dired-window)
;;;;;;;;         )
;;;;;;;;     (cdmt-odc-reset 'make)

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

(ert-deftest cdmt-odc-revert-buffer ()
  "Test revert-buffer.
revert-buffer is not yet implemented -- expect an error."
  (should-error (revert-buffer)
     :type 'error))

(ert-deftest cdmt-odc-cpio-dired-create-directory ()
  "Test cpio-dired-create-directory."
  (let ((test-name "cdmt-odc-cpio-dired-view-archive")
        (cpio-archive-buffer)
        (cpio-archive-buffer-contents)
        (cpio-dired-buffer)
        (cpio-dired-buffer-contents)
	(cpio-catalog-contents-before)
	(cpio-catalog-contents-after)
	(cpio-archive-window)
	(cpio-dired-window))

    (cdmt-odc-reset 'make)

    (progn (setq cpio-catalog-contents-before (format "%s" (pp (cpio-catalog))))
	   (cpio-dired-create-directory "newDirectory")
	   (setq cpio-archive-buffer-contents
		 (cdmt-odc-filter-archive-contents
		  (with-current-buffer cpio-archive-buffer
		    (buffer-substring-no-properties (point-min) (point-max)))))
	   (setq cpio-dired-buffer-contents
		 (with-current-buffer cpio-dired-buffer
		   (buffer-substring-no-properties (point-min) (point-max))))
	   (setq cpio-catalog-contents-after (format "%s" (pp (cpio-catalog)))))

    (should (and "Expecting a cpio archive with newDirectory, a new directory."
		 (string-equal "070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
a	(( filename ))

a

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
aa	(( filename ))

aa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
aaa	(( filename ))

aaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
aaaa	(( filename ))

aaaa

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
aaaaa	(( filename ))

aaaaa

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
aaaaa.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
b	(( filename ))

b

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
bb	(( filename ))

bb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
bbb	(( filename ))

bbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
bbbb	(( filename ))

bbbb

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
bbbbb	(( filename ))

bbbbb

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
bbbbb.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000004	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000002	(( namesize ))
000000	(( chksum   ))
c	(( filename ))

c

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000005	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000003	(( namesize ))
000000	(( chksum   ))
cc	(( filename ))

cc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000006	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000004	(( namesize ))
000000	(( chksum   ))
ccc	(( filename ))

ccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000007	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000005	(( namesize ))
000000	(( chksum   ))
cccc	(( filename ))

cccc

070707	(( magic    ))
DEADBE	(( ino      ))
100644	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000010	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000006	(( namesize ))
000000	(( chksum   ))
ccccc	(( filename ))

ccccc

070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000002	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000010	(( namesize ))
000000	(( chksum   ))
ccccc.d	(( filename ))
070707	(( magic    ))
DEADBE	(( ino      ))
040755	(( mode     ))
001750	(( uid      ))
001750	(( gid      ))
000001	(( nlink    ))
DEADBE	(( mtime    ))
00000000000	(( filesize ))
DEADBE	(( dev maj  ))
DEADBE	(( dev min  ))
DEADBE	(( rdev maj ))
DEADBE	(( rdev min ))
000015	(( namesize ))
000000	(( chksum   ))
newDirectory	(( filename ))

" cpio-archive-buffer-contents)))
    (should (and "Expecting a cpio dired buffer with newDirectory, a new directory."
		 (string-match  "CPIO archive: alphabet_small.odc.cpio:

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
    (should (and "Expecting a catalog with a new directory called »newDirectory«."
		 (string-match "((\"newDirectory\" \\.
  [[1 16877 [[:digit:]]+ [[:digit:]]+ 1
      ([[:digit:]]+ [[:digit:]]+)
      0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 13 0 \"newDirectory\"]
   #<marker at 1543 in alphabet_small.odc.cpio> #<marker at 1632 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"a\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"a\"]
   #<marker at 1 in alphabet_small.odc.cpio> #<marker at 79 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aa\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"aa\"]
   #<marker at 83 in alphabet_small.odc.cpio> #<marker at 162 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaa\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"aaa\"]
   #<marker at 167 in alphabet_small.odc.cpio> #<marker at 247 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaa\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"aaaa\"]
   #<marker at 253 in alphabet_small.odc.cpio> #<marker at 334 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"aaaaa\"]
   #<marker at 341 in alphabet_small.odc.cpio> #<marker at 423 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"aaaaa\\.d\" \\.
  [[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"aaaaa\\.d\"]
   #<marker at 431 in alphabet_small.odc.cpio> #<marker at 515 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"b\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"b\"]
   #<marker at 515 in alphabet_small.odc.cpio> #<marker at 593 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bb\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"bb\"]
   #<marker at 597 in alphabet_small.odc.cpio> #<marker at 676 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbb\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"bbb\"]
   #<marker at 681 in alphabet_small.odc.cpio> #<marker at 761 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbb\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"bbbb\"]
   #<marker at 767 in alphabet_small.odc.cpio> #<marker at 848 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"bbbbb\"]
   #<marker at 855 in alphabet_small.odc.cpio> #<marker at 937 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"bbbbb\\.d\" \\.
  [[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"bbbbb\\.d\"]
   #<marker at 945 in alphabet_small.odc.cpio> #<marker at 1029 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"c\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 4 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 2 0 \"c\"]
   #<marker at 1029 in alphabet_small.odc.cpio> #<marker at 1107 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cc\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 5 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 3 0 \"cc\"]
   #<marker at 1111 in alphabet_small.odc.cpio> #<marker at 1190 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccc\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 6 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 4 0 \"ccc\"]
   #<marker at 1195 in alphabet_small.odc.cpio> #<marker at 1275 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"cccc\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 7 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 5 0 \"cccc\"]
   #<marker at 1281 in alphabet_small.odc.cpio> #<marker at 1362 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\" \\.
  [[[[:digit:]]+ 33188 [[:digit:]]+ [[:digit:]]+ 1
	 ([[:digit:]]+ [[:digit:]]+)
	 8 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 6 0 \"ccccc\"]
   #<marker at 1369 in alphabet_small.odc.cpio> #<marker at 1451 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified])
 (\"ccccc\\.d\" \\.
  [[[[:digit:]]+ 16877 [[:digit:]]+ [[:digit:]]+ 2
	 ([[:digit:]]+ [[:digit:]]+)
	 0 [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ [[:digit:]]+ 8 0 \"ccccc\\.d\"]
   #<marker at 1459 in alphabet_small.odc.cpio> #<marker at 1543 in alphabet_small.odc.cpio> cpio-mode-entry-unmodified]))
" cpio-catalog-contents-after)))
    
    (cdmt-odc-test-save)))


;;
;; Run tests
;;

(ert "^cdmt-odc-")

;;; cpio-dired-test.el ends here.

