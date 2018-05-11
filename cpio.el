;; -*- coding: utf-8 -*-
;;; cpio.el --- cpio-mode for emacs
;	$Id: cpio.el,v 1.2.4.9 2018/05/11 13:17:00 doug Exp $	

;; COPYRIGHT 2015, 2017, 2018 Douglas Lewan, d.lewan2000@gmail.com

;; Author: Douglas Lewan (d.lewan2000@gmail.com)
;; Maintainer: -- " --
;; Created: 2015 Jan 03
;; Version: 0.02
;; Keywords: cpio, archive

;;; Commentary:

;;; Documentation:

;; 
;; NAME: cpio-mode
;; 
;; USAGE:
;;     (load-library 'cpio-mode)
;;     (require 'cpio-mode)
;; 
;; DESCRIPTION:
;;     cpio-mode presents a cpio archive as if it were a directory
;;     in a manner like dired-mode.
;;     tar-mode already does such a thing for tar archives,
;;     and some ideas (and likely code) have been adapted from tar-mode.
;; 
;;     To automatically invoke cpio-mode when finding a file
;;     add the following to your find-file-hook.
;; 
;;     You can use toggle-cpio-mode to switch between cpio-mode
;;     and fundamental mode.
;; 
;; KEYMAP:
;;     This should be conceptually as close to dired as I can make it.
;; 
;; OPTIONS:
;; 
;; ENVIRONMENT:
;;     Early development was done under emacs 24.2
;;     on the Fedora 18 distribution of 64 bit GNU/Linux.
;; 
;;     Current development is happening under emacs 24.5
;;     on Linux Mint, Linux kernel 4.4.0.
;; 
;; RETURN CODE:
;; 
;; NOTES:
;;     Binary formats are not yet implemented.
;; 
;; CAVEATS:
;;     Only regular files can be edited.
;;     I'm not sure what to do with symbolic links yet.
;; 
;; SECURITY ISSUES:
;;     There are no ownership/group-ship tests on anything.
;;     You could create an archive with bad behavior
;;     (for example, a set-uid executable)
;;     when unpacked by root.
;; 

;; 
;; cpio.el is the entry point to all of cpio-mode.
;; It defines the archive management variables and functions
;; that define cpio-mode.
;; That said, there are other components.
;; 1. There's some generically useful code
;;    defined in
;;    • cpio-generic.el, truly generic code,
;;    • cpio-modes.el, file-mode related information,
;; 2. Every archive format has its own file: 
;;    cpio-bin for the cpio binary format,
;;    cpio-crc for the cpio CRC format,
;;    etc.
;; 3. cpio.el, this file, defining the cpio logic
;;    reflected in the catalog,
;;    a list of the information of all the headers
;;    in the current archive.
;; 4. The package cpio-dired, defining the user interface.
;; 
;; The following figure shows the relationships
;; among those components.
;; 
;; +----------------------+   +-------------+   +-------------+
;; | Format specific code |   |             |   |             |
;; | +---------------+    |   |             |   |             |
;; | | cpio-bin      |    |   |             |   |             |
;; | | +--------------+   |   |    CPIO     |   | dired-like  |
;; | +-|cpio-crc      |   |<->|    Logic    |<->|     UI      |
;; |   | +-------------+  |   |             |   |             |
;; |   +-| hpbin       |  |   |             |   |             |
;; |     | +------------+ |   |             |   |             |
;; |     +-| ···        | |   |             |   |             |
;; |       +------------+ |   |             |   |             |
;; +----------------------+   +-------------+   +-------------+
;;             Λ                     Λ                 Λ
;;             |                     |                 |
;;             V                     V                 V
;; +----------------------------------------------------------+
;; | generic code                                             |
;; |          +------------+ +--------------+ +-----+         |
;; |          | cpio-modes | | cpio-generic | | ··· |         |
;; |          +------------+ +--------------+ +-----+         |
;; +----------------------------------------------------------+
;; 
;; The basic idea is that the format-spedific code parses and makes headers
;; while all the cpio logic uses those parsed headers to edit
;; and calls format-specific parsing and making functions.
;; 
;; The main data structures are the following.
;; 
;; 0. Parsed headers, an inode-like array structure.
;; 
;; 1. Entries, an array containing a parsed header,
;;    the header start and the contents start.
;; 
;; 2. The catalog, a list of the entries in the cpio archive,
;;    including the trailer.
;; 
;; 3. The buffer holding the archive.
;;    This buffer is put into cpio-mode.
;;    It holds all the "global" data,
;;    like the catalog described above.
;; 
;; 4. The buffer holding the dired-like UI.
;;    cpio-mode creates this buffer and
;;    puts this buffer into cpio-dired-mode.
;; 
;; 5. Buffers visiting entries.
;;    cpio-dired-mode uses the archive buffer
;;    to get entry contents and them in the visiting buffer.
;;    cpio-dired-mode puts that buffer in cpio-entry-contents-mode,
;;    a minor mode that handles editing and saving
;;    an entry's contents.
;; 
;;    CAUTION: This (5) is not working correctly yet.
;;    Apparently, I don't understand
;;    how to make a minor mode be buffer local yet.
;;    It seems to apply buffer local,
;;    but merely loading it seems to turn it on in all buffers.
;; 
;;    The following code can at least clean up its mess.
;;         (mapc (lambda (b)
;;                 (with-current-buffer b
;;                   (cpio-entry-contents-mode 0)))
;;               (buffer-list))

;;; Naming conventions.

;;
;; All files that define cpio-mode begin with "cpio."
;; 
;; Global variables all begin '*cpio-...'.
;; Functions are named 'cpio-...'.
;; 
;; The corresponding archive format specific names for format FMT
;; begin '*cpio-FMT-...' and 'cpio-FMT-...'.
;; The format-specific variables names are calculated
;; in (cpio-set-local-vars).
;; That function drops directly into corresponding format-specific functions
;;     
;; The format-specific function names are calculated
;; in (cpio-set-local-funcs).
;; Here is the process:
;;     cpio-do-good-stuff-func
;;     --> "cpio-do-good-stuff-func"
;;     --> "cpio" "do" "good" "stuff"
;;     --> "cpio-fmt-do-good-stuff"
;;     --> cpio-fmt-do-good-stuff
;; 
;; The index of FIELD within a parsed header is named 'cpio-FIELD-parsed-idx'.
;;
;; Each archive format FMT has a regular expression
;; that identifies that format unambiguously called '*cpio-FMT-header-re*'.
;; 
;; The functions (cpio-get-FIELD) operate directly on the header
;; to extract FIELD.
;; It's not clear that these need to be defined here.
;;
;; The functions (cpio-FIELD) operate on a parsed header
;; to extract FIELD.
;; 
;; Depending on the context the expression "entry attributes",
;; often abbreviated "attrs", and the phrase "parsed header"
;; are used to reference the structure
;; that stores inode-like information (mode, size, user, etc.).
;; Truly, the expressions are semantically equivalent.
;; However, "parsed header" is used where the topic at hand is
;; the archive, and
;; "entry attributes" is used where the topic at hand is
;; the internal processing within cpio-mode.
;;
;; An "entry" is, somewhat ambiguously, either an entry in the archive
;; or a member of the catalog.
;; The context should make it clear which is intended.
;; Yes, in principle they're isomorphic.
;; (And, yes, I hate specifications that depend on context.)
;; 

;;; Code:

;;
;; Dependencies
;; 

;; During development I need access to local files.
(setq load-path (add-to-list 'load-path (substring default-directory -1)))

(require 'dired)

;; (require 'cpio-generic)
(load (concat default-directory "cpio-generic.el"))
(message "Loaded cpio-generic.el.")
;; (require 'cpio-modes)
(load (concat default-directory "cpio-modes.el"))
(message "Loaded cpio-modes.el.")
;; (require 'cpio-affiliated-buffers)
(load (concat default-directory "cpio-affiliated-buffers"))
(message "Loaded cpio-affiliated-buffers.el.")
;; (require 'cpio-hpbin)
(load (concat default-directory "cpio-hpbin"))
(message "Loaded cpio-hpbin.el")
;; (require 'cpio-hpodc)
(load (concat default-directory "cpio-hpodc"))
(message "Loaded cpio-hpodc.el")
;; (require 'cpio-newc)
(load (concat default-directory "cpio-newc"))
(message "Loaded cpio-newc.el.")
;; (require 'cpio-odc)
(load (concat default-directory "cpio-odc"))
(message "Loaded cpio-odc.el")
;; (require 'cpio-tar)
;; (require 'cpio-ustar)
;; (require 'cpio-wanted)
(load (concat default-directory "cpio-wanted"))
(message "Loaded cpio-wanted.el.")
;; (require 'cpio-dired)
(load (concat default-directory "cpio-dired"))
(message "Loaded cpio-dired.el.")
(load (concat default-directory "cpio-entry-contents-mode"))
(message "Loaded cpio-entry-contents-mode.")


;; 
;; Vars
;; 
(defvar *cpio-format* ()
  "The format of the cpio archive in the current-buffer.
Takes the values 'bin, 'newc, 'odc etc.")
(make-variable-buffer-local '*cpio-format*)

(let ((i 0))
  ;; HEREHERE Are these even used?
  ;; It doesn't look like it.
  (defvar *cpio-magic-idx* 0	; (setq i (1+ i))
    "Index of magic in a parsed cpio header.")
  (setq *cpio-magic-idx* i)
  (setq i (1+ i))

  (defvar *cpio-ino-idx* 0	; (setq i (1+ i))
    "Index of ino in a parsed cpio header.")
  (setq *cpio-ino-idx* i)
  (setq i (1+ i))

  (defvar *cpio-mode-idx* 0	; (setq i (1+ i))
    "Index of mode in a parsed cpio header.")
  (setq *cpio-mode-idx* i)
  (setq i (1+ i))

  (defvar *cpio-uid-idx* 0	; (setq i (1+ i))
    "Index of uid in a parsed cpio header.")
  (setq *cpio-uid-idx* i)
  (setq i (1+ i))

  (defvar *cpio-gid-idx* 0	; (setq i (1+ i))
    "Index of gid in a parsed cpio header.")
  (setq *cpio-gid-idx* i)
  (setq i (1+ i))

  (defvar *cpio-nlink-idx* 0	; (setq i (1+ i))
    "Index of nlink in a parsed cpio header.")
  (setq *cpio-nlink-idx* i)
  (setq i (1+ i))

  (defvar *cpio-mtime-idx* 0	; (setq i (1+ i))
    "Index of mtime in a parsed cpio header.")
  (setq *cpio-mtime-idx* i)
  (setq i (1+ i))

  (defvar *cpio-entry-size-idx* 0 ; (setq i (1+ i))
    "Index of filesize in a parsed cpio header.")
  (setq *cpio-entry-size-idx* i)
  (setq i (1+ i))

  (defvar *cpio-dev-maj-idx* 0	; (setq i (1+ i))
    "Index of dev major in a parsed cpio header.")
  (setq *cpio-dev-maj-idx* i)
  (setq i (1+ i))

  (defvar *cpio-dev-min-idx* 0	; (setq i (1+ i))
    "Index of dev minor in a parsed cpio header.")
  (setq *cpio-dev-min-idx* i)
  (setq i (1+ i))

  (defvar *cpio-rdev-maj-idx* 0	; (setq i (1+ i))
    "Index of rdev major in a parsed cpio header.")
  (setq *cpio-rdev-maj-idx* i)
  (setq i (1+ i))

  (defvar *cpio-rdev-min-idx* 0	; (setq i (1+ i))
    "Index of rdev minor in a parsed cpio header.")
  (setq *cpio-rdev-min-idx* i)
  (setq i (1+ i))

  (defvar *cpio-namesize-idx* 0 ; (setq i (1+ i))
    "Index of namesize in a parsed cpio header.")
  (setq *cpio-namesize-idx* i)
  (setq i (1+ i))

  (defvar *cpio-checksum-idx* 0 ; (setq i (1+ i))
    "Index of checksum in a parsed cpio header.")
  (setq *cpio-checksum-idx* i)
  (setq i (1+ i))

  (defvar *cpio-filename-idx 0 ; (setq i (1+ i))
    "Index of filename in a parsed cpio header.")
  (setq *cpio-filename-idx* i)
  (setq i (1+ i))

  (defvar *cpio-checksum-idx* 0 ; (setq i (1+ i))
    "Index of the checksum in a parsed cpio-header.")
  (setq *cpio-filename-idx* i)
  (setq i (1+ i)))

;; N.B. The format REs go here since they are what we use
;; to discern the type of the archive.
(defvar *cpio-bin-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match BIN format cpio archives.")
(setq *cpio-bin-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-odc-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match odc format cpio archives.")
(setq *cpio-odc-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-crc-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match crc format cpio archives.")
(setq *cpio-crc-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-tar-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match tar format cpio archives.")
(setq *cpio-tar-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-ustar-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match ustar format cpio archives.")
(setq *cpio-ustar-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-hpbin-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match hpbin format cpio archives.")
(setq *cpio-hpbin-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-hpodc-header-re* "nOt yEt iMpLeMeNtEd"
  "RE to match hpodc format cpio archives.")
(setq *cpio-hpodc-header-re* "nOt yEt iMpLeMeNtEd")

(defvar *cpio-re-type-alist* (list (cons *cpio-bin-header-re* 'bin)
				   (cons *cpio-newc-header-re* 'newc)
				   (cons *cpio-odc-header-re* 'odc)
				   (cons *cpio-crc-header-re* 'crc)
				   (cons *cpio-tar-header-re* 'tar)
				   (cons *cpio-ustar-header-re* 'ustar)
				   (cons *cpio-hpbin-header-re* 'hpbin)
				   (cons *cpio-hpodc-header-re* 'hpodc))
  "Association list matching REs defining cpio entry header types
with their corresponding archive types.
The archive types are symbols: 'bin, 'newc, 'odc, etc.
See `cpio-discern-archive-type' for the full list.")
(setq *cpio-re-type-alist* (list (cons *cpio-bin-header-re* 'bin)
				 (cons *cpio-newc-header-re* 'newc)
				 (cons *cpio-odc-header-re* 'odc)
				 (cons *cpio-crc-header-re* 'crc)
				 (cons *cpio-tar-header-re* 'tar)
				 (cons *cpio-ustar-header-re* 'ustar)
				 (cons *cpio-hpbin-header-re* 'hpbin)
				 (cons *cpio-hpodc-header-re* 'hpodc)))

(defvar cpio-build-catalog-func ()
  "The function for building the catalog of a specific archive format.")
(setq cpio-build-catalog-func ())


(defvar cpio-parse-header-func ()
  "")
(setq cpio-parse-header-func ())

(defvar cpio-header-at-point-func ()
  "")
(setq cpio-header-at-point-func ())

(defvar cpio-get-magic-func ()
  "")
(setq cpio-get-magic-func ())

(defvar cpio-get-ino-func ()
  "")
(setq cpio-get-ino-func ())

(defvar cpio-get-mode-func ()
  "")
(setq cpio-get-mode-func ())

(defvar cpio-get-uid-func ()
  "")
(setq cpio-get-uid-func ())

(defvar cpio-get-gid-func ()
  "")
(setq cpio-get-gid-func ())

(defvar cpio-get-nlink-func ()
  "")
(setq cpio-get-nlink-func ())

(defvar cpio-get-mtime-min-func ()
  "")
(setq cpio-get-mtime-min-func ())

(defvar cpio-get-filesize-func ()
  "")
(setq cpio-get-filesize-func ())

(defvar cpio-get-dev-maj-func ()
  "")
(setq cpio-get-dev-maj-func ())

(defvar cpio-get-dev-min-func ()
  "")
(setq cpio-get-dev-min-func ())

(defvar cpio-get-rdev-maj-func ()
  "")
(setq cpio-get-rdev-maj-func ())

(defvar cpio-get-rdev-min-func ()
  "")
(setq cpio-get-rdev-min-func ())

(defvar cpio-get-namesize-func ()
  "")
(setq cpio-get-namesize-func ())

(defvar cpio-get-chksum-func ()
  "")
(setq cpio-get-chksum-func ())

(defvar cpio-get-name-func ()
  "")
(setq cpio-get-name-func ())

(defvar cpio-get-contents-func ()
  "")
(setq cpio-get-contents-func ())

(defvar cpio-make-header-string-func ()
  "")
(setq cpio-make-header-string-func ())

(defvar *cpio-local-funcs* (list
			    ;; Catalog management
			    'cpio-build-catalog-func
			    ;; Header parsing functions
			    'cpio-parse-header-func
			    'cpio-header-at-point-func
			    'cpio-get-magic-func
			    'cpio-get-dev-func
			    'cpio-get-ino-func
			    'cpio-get-mode-func
			    'cpio-get-uid-func
			    'cpio-get-gid-func
			    'cpio-get-nlink-func
			    'cpio-get-rdev-func
			    'cpio-get-mtime-func
			    'cpio-get-namesize-func
			    'cpio-get-filesize-func
			    'cpio-get-name-func
			    'cpio-get-contents-func
			    'cpio-end-of-archive
			    'cpio-make-header-string-func
			    ;; Archive manipulation functions.
			    'cpio-adjust-trailer-func
			    'cpio-delete-trailer-func)
  "A list of variables peculiar to the different headers and their fields.
The design here is that package-wide variables have the prefix `cpio-'
and the corresponding functions for a specific format FMT have the form `cpio-FMT-'.
All of this can then be calculated via (symbol-name), etc.")
(setq *cpio-local-funcs* (list
			  ;; Catalog management
			  'cpio-build-catalog-func
			  ;; Header parsing functions
			  'cpio-end-of-archive-func
			  'cpio-start-of-trailer-func
			  ;; Header making functions
			  'cpio-make-header-string-func
			  ;; Archive manipulation functions
			  'cpio-adjust-trailer-func
			  'cpio-insert-trailer-func
			  'cpio-delete-trailer-func))
;; (make-variable-buffer-local '*cpio-local-funcs*)

(defvar *cpio-catalog* ()
  "The variable that holds the catalog of entries in cpio-mode.
Each entry has the following form:
    name
    [parsed-header
     header-start
     content-start].
• name is the name of the entry
• parsed-header has the description below.
• header-start and content-start are markers,
  so they should be automatically updated
  with modifications to the buffer.
The last entry should is always be the TRAILER entry.

A parsed header is a vector of the following form:
    [inode
     mode
     uid
     gid
     nlink
     mtime
     filesize
     dev-maj
     dev-min
     rdev-maj
     rdev-min
     contents-size
     checksum
     name].")
(make-variable-buffer-local '*cpio-catalog*)
(setq *cpio-catalog* ())

(let ((i 0))
  ;; (defvar *cpio-catalog-entry-name-idx* i)
  ;; (setq i (1+ i))
  (defvar *cpio-catalog-entry-attrs-idx* i)
  (setq *cpio-catalog-entry-attrs-idx* i)
  
  (setq i (1+ i))
  (defvar *cpio-catalog-entry-header-start-idx* i)
  (setq *cpio-catalog-entry-header-start-idx* i)
  
  (setq i (1+ i))
  (defvar *cpio-catalog-entry-contents-start-idx* i)
  (setq *cpio-catalog-entry-contents-start-idx* i))

(defvar *cpio-dired-buffer* ()
  "The [subordinate] buffer used to present the curent catalog
à la dired.")
(setq *cpio-dired-buffer* ())
(make-variable-buffer-local '*cpio-dired-buffer*)

(defvar *cpio-archive-name* ()
  "The name of the cpio archive being processed.")
(setq *cpio-archive-name* ())
(make-variable-buffer-local '*cpio-archive-names*)

;; Indexes for the fields in a parsed header.
(let ((i 0))
  (defvar *cpio-ino-parsed-idx* i)
  (setq *cpio-ino-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-mode-parsed-idx* i)
  (setq *cpio-mode-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-uid-parsed-idx* i)
  (setq *cpio-uid-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-gid-parsed-idx* i)
  (setq *cpio-gid-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-nlink-parsed-idx* i)
  (setq *cpio-nlink-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-mtime-parsed-idx* i)
  (setq *cpio-mtime-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-entry-size-parsed-idx* i)
  (setq *cpio-entry-size-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-dev-maj-parsed-idx* i)
  (setq *cpio-dev-maj-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-dev-min-parsed-idx* i)
  (setq *cpio-dev-min-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-rdev-maj-parsed-idx* i)
  (setq *cpio-rdev-maj-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-rdev-min-parsed-idx* i)
  (setq *cpio-rdev-min-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-namesize-parsed-idx* i)
  (setq *cpio-namesize-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-checksum-parsed-idx* i)
  (setq *cpio-checksum-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-name-parsed-idx* i)
  (setq *cpio-name-parsed-idx* i))

(defvar *cpio-padding-modulus* ()
  "The modulus to be used for building padded strings.")
(defvar *cpio-padding-char* ()
  "The character to be used for building padded strings.")
(defvar *cpio-padding-str* ()
  "A single character string of the character to be used for building padded strings.")


;;
;; Customizations
;; 
(defgroup cpio ()
  "Customizations for cpio-mode.")

(defcustom cpio-default-format "newc"
  "The default cpio format to use for a new or empty archive."
  :type 'string
  :group 'cpio)

;; N.B. This is here because this file is where the cpio-dired lines are created.
(defcustom cpio-try-names t
  "Non-nil means that GIDs and UIDs are displayed as integers."
  :group 'cpio
  :type 'boolean)


;; 
;; Library
;; 
(defun cpio-discern-archive-type ()
  "Return a symbol reflecting the type of the cpio archive in the current buffer.
Values are 'bin, 'newc, 'odc, 'crc, 'tar, 'ustar, 'hpbin, 'hpodc
and NIL if the current buffer does not begin with a cpio entry header."
  ;; Using a RE may not be the right way to go.
  ;; Maybe each format needs a function.
  (let ((fname "cpio-discern-archive-type")
	(this-archive-type))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (catch 'found-it
	(mapcar (lambda (archive-spec)
		  (cond ((looking-at-p (car archive-spec))
			 (setq this-archive-type (cdr archive-spec))
			 (throw 'found-it t))
			(t t)))
		*cpio-re-type-alist*)))
    this-archive-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header handling

;; HEREHERE I think all these (funcall)s need to be wrapped with *s.
(defun cpio-parse-header (header-str)
  "Return the internal entry header structure encoded in HEADER-STR."
  (let ((fname "cpio-parse-header"))
    (funcall cpio-parse-header-func header-str)))

(defun cpio-header-at-point (&optional where)
  "Return the header string at or following point WHERE.
If WHERE is not given, then use point.
CAVEAT: This searches for the magic number at the begining of the header;
if WHERE is inside the magic number, then the search will fail."
  (unless where (setq where (point)))
  (let ((fname "cpio-header-at-point"))
    (funcall cpio-header-at-point-func where)))

(defun cpio-ino (parsed-header)
  "Return the inode in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-ino"))
    (aref parsed-header *cpio-ino-parsed-idx*)))

(defun cpio-mode-value (parsed-header)
  "Return the mode (as an integer) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-mode"))
    (aref parsed-header *cpio-mode-parsed-idx*)))

(defun cpio-uid (parsed-header)
  "Return the UID (as an integer) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-uid"))
    (aref parsed-header *cpio-uid-parsed-idx*)))

(defun cpio-gid (parsed-header)
  "Return the GID (as an integer) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-gid"))
    (aref parsed-header *cpio-gid-parsed-idx*)))

(defun cpio-nlink (parsed-header)
  "Return the number of links in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-nlink"))
    (aref parsed-header *cpio-nlink-parsed-idx*)))

(defun cpio-mtime (parsed-header)
  "Return the mod time (emacs time structure) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-mtime"))
    (aref parsed-header *cpio-mtime-parsed-idx*)))

(defun cpio-entry-size (parsed-header)
  "Return the size of the contents in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-entry-size"))
    (aref parsed-header *cpio-entry-size-parsed-idx*)))

(defun cpio-dev-maj (parsed-header)
  "Return the dev maj in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-dev"))
    (aref parsed-header *cpio-dev-maj-parsed-idx*)))

(defun cpio-dev-min (parsed-header)
  "Return the dev in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-dev-min"))
    (aref parsed-header *cpio-dev-min-parsed-idx*)))

(defun cpio-rdev-maj (parsed-header)
  "Return the rdev maj in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-rdev-maj"))
    (aref parsed-header *cpio-rdev-maj-parsed-idx*)))

(defun cpio-rdev-min (parsed-header)
  "Return the rdev in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-rdev-min"))
    (aref parsed-header *cpio-rdev-min-parsed-idx*)))

(defun cpio-namesize (parsed-header)
  "Return the size of the name  in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-namesize"))
    (aref parsed-header *cpio-namesize-parsed-idx*)))

(defun cpio-entry-name (parsed-header)
  "Return the name in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-name"))
    (aref parsed-header *cpio-name-parsed-idx*)))

(defun cpio-chksum (parsed-header)
  "Return the checksum in PARSE-HEADER."
  (let ((fname "cpio-chksum"))
    (aref *cpio-chksum-parsed-idx* parsed-header)))

(defun cpio-contents-start (entry-name)
  "Return the contents start for ENTRY-NAME."
  (let* ((fname "cpio-contents-start")
	 (catalog-entry (cdr (assoc entry-name (cpio-catalog)))))
    (aref catalog-entry *cpio-catalog-entry-contents-start-idx*)))

(defun cpio-entry-attrs (entry-name)
  "Retrieve the entry attributes for ENTRY-NAME."
  (let ((fname "cpio-entry-attrs"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-entry-attrs entry-name))
      (aref (cdr (assoc entry-name (cpio-catalog))) 0))))

(defun cpio-entry-header-start (entry)
  "Return the start of the entry specified in ENTRY."
  (let ((fname "cpio-entry-start"))
    (aref entry *cpio-catalog-entry-header-start-idx*)))

(defun cpio-entry-contents-start (entry)
  "Return the start of the contents of the entry specified in ENTRY."
  (let ((fname "cpio-entry-start"))
    (aref entry *cpio-catalog-entry-contents-start-idx*)))

(defun cpio-set-contents-start (entry where)
  "Set the contents start marker in ENTRY to the location WHERE.
WHERE can be an integer or marker."
  (let ((fname "cpio-set-contents-start")
	(where-marker (cond ((integerp where)
			     (set-marker (make-marker) where))
			    ((markerp where)
			     where)
			    (t
			     (error 'wrong-type-error where)))))
    ;; (error "%s() is not yet implemented" fname)
    (aset entry *cpio-catalog-entry-contents-start-idx* where-marker)))

(defun cpio-contents (entry-name &optional archive-buffer)
  "Return a string that is the contents of the named entry."
  (let ((fname "cpio-contents"))
    (cond (archive-buffer
	   (with-current-buffer archive-buffer
	     (cpio-contents entry-name)))
	  (*cab-parent*
	   (with-current-buffer *cab-parent*
	     (cpio-contents entry-name)))
	  (t
	   (let* ((entry-attrs    (cpio-entry-attrs    entry-name))
		  (contents-start (cpio-contents-start entry-name))
		  (contents-size  (cpio-entry-size entry-attrs))
		  (contents-end (+ contents-start contents-size -1))
		  (result))
	     (if (null entry-attrs)
		 (error "%s(): Could not get entry attributes for [[%s]]." fname))
	     (goto-char contents-start)
	     (forward-char contents-size)
	     (setq result (buffer-substring-no-properties contents-start (point))))))))

(defun cpio-catalog ()
  "Return the catalog relevant to the current buffer."
  (let ((fname "cpio-catalog"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  *cpio-catalog*)
      *cpio-catalog*)))

(defun cpio-make-header-string (parsed-header)
  "Build a padded cpio header string based on the given PARSED-HEADER."
  (let ((fname "cpio-make-header-string"))
    (funcall cpio-make-header-string-func parsed-header)))

(defun cpio-set-entry-size (parsed-header size)
  "Set the entry-size element of PARSED-HEADER to SIZE."
  (let ((fname "cpio-set-entry-size"))
    (aset parsed-header *cpio-entry-size-parsed-idx* size)))

(defun cpio-set-entry-name (parsed-header entry-name)
  "Set the entry-name element of the PARSED-HEADER to ENTRY-NAME.
To be consistent, this also sets the name's size element."
  (let ((fname "cpio-set-entry-name"))
    (aset parsed-header *cpio-name-parsed-idx* entry-name)
    ;; The namesize in the header includes the terminating NULL at the end of the name.
    ;; See, for example, (cpio-newc-header-size).
    (aset parsed-header *cpio-namesize-parsed-idx* (1+ (length entry-name)))))

(defun cpio-set-uid (parsed-header uid)
  "Set the uid field in the PARSED-HEADER to UID.
UID can be either a string (representing a number)
or an integer."
  (let ((fname "cpio-set-uid"))
    ;; (error "%s() is not yet implemented" fname)
    (unless (integerp uid)
      (setq uid (string-to-number uid)))
    (aset parsed-header *cpio-uid-parsed-idx* uid)))

(defun cpio-set-gid (parsed-header gid)
  "Set the gid field in the PARSED-HEADER to GID.
GID can be either a string (representing a number)
or an integer."
  (let ((fname "cpio-set-gid"))
    ;; (error "%s() is not yet implemented" fname)
    (unless (integerp gid)
      (setq uid (string-to-number gid)))
    (aset parsed-header *cpio-gid-parsed-idx* gid)))

(defun cpio-set-mode (parsed-header mode)
  "Set the mode field in the PARSED-HEADER to MODE.
MODE is either an integer or a string representing an integer."
  (let ((fname "cpio-set-mode"))
    ;; (error "%s() is not yet implemented" fname)
    (unless (integerp mode)
      (setq uid (string-to-number mode)))
    (aset parsed-header *cpio-mode-parsed-idx* mode)))

(defun cpio-set-mtime (parsed-header mtime)
  "Set the modification time in the PARSED-HEADER to MTIME.
MTIME is an emacs time."
  (let ((fname "cpio-set-mtime"))
    ;; (error "%s() is not yet implemented" fname)
    (aset parsed-header *cpio-mtime-parsed-idx* mtime)))

(defun cpio-extract-all ()
  "Extract all entries from the cpio archive related to the current buffer."
  (let ((fname "cpio-extract-all"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-extract-all))
      (mapc (lambda (e)
	      (let* ((entry-name (car e)))
		(cpio-extract-entry entry-name)))
	    *cpio-catalog*))))

(defun cpio-ask-user-about-supersession-threat (entry-name)
  "Ask a user who is trying to save ENTRY-NAME what to do
if a file named ENTRY-NAME already exists
or if there is a modified buffer containing that entry's contents."
  (let* ((fname "cpio-ask-user-about-supersession-threat")
	 (intermediate-buffer-name (cpio-contents-buffer-name entry-name))
	 (intermediate-buffer (get-buffer intermediate-buffer-name))
	 (archive-mod-time (cpio-get-archive-mod-time)) ;HEREHERE Do I want this?
	 (entry-mod-time (cpio-mtime (cpio-entry-attrs entry-name))))
    (cond ((and (buffer-live-p intermediate-buffer)
		(buffer-modified-p intermediate-buffer)
		(yes-or-no-p (format "A buffer for entry %s exists and is modified. Save? " entry-name)))
	   t)
	  ((and (file-exists-p entry-name)
		(yes-or-no-p (format "File %s already exists. Overwrite? " entry-name)))
	   t)
	  (t t))))

(defun cpio-get-archive-mod-time ()
  "Return the modification time of the cpio archive affiliated with the current buffer."
  (let ((fname "cpio-get-archive-mod-time")
	(archive-buffer (if *cab-parent*
			    *cab-parent*
			  (current-buffer))))
    (with-current-buffer archive-buffer
      (message "%s(): is not implemented." fname))))

(defun cpio-extract-entry (entry-name &optional from-lisp)
  ;; HEREHERE This will have to deal with creating directories.
  "Extract the archive entry ENTRY-NAME.
If that file already exists (and this is called interactively),
then prompt the user about overwriting it.
If a buffer is already visiting that entry,
then leave that buffer in place;
otherwise kill the intermediate buffer.

The optional argument FROM-LISP indicates
if this was called from a lisp program.
If it is, then the extraction occurs no matter what.

CAVEAT: Extracting the same ENTRY-NAME from different archives
will create a conflict.

CONTRACT: This can only be invoked in a cpio archive under cpio-mode
or a buffer affiliated with such a buffer."
  ;; (interactive "sName: \nP")
  (let* ((fname "cpio-extract-entry")
	 (attrs (cpio-entry-attrs entry-name))
	 (entry-type (cpio-entry-type entry-name)))
    (cond ((string-equal entry-type *cpio-modes-link*)
	   (warn "%s(): Link extraction is not yet implemented." fname))
	  ((string-equal entry-type *cpio-modes-reg*)
	   ;; (message "%s(%s): Extracting a regular file." fname entry-name) (sit-for 0.1)
	   (cpio-extract-regular-file entry-name))
	  ((string-equal entry-type *cpio-modes-dir*)
	    ;; (message "%s(%s): Extracting a directory." fname entry-name) (sit-for 0.1)
	    (cpio-extract-directory entry-name))
	   ((string-equal entry-type *cpio-modes-char*)
	    (warn "%s(): Character special files cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-block*)
	    (warn "%s(): Block special files cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-fifo*)
	    (warn "%s(): FIFOs (pipes) cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-sock*)
	    (warn "%s(): Sockets cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-unkown*)
	    (warn "%s(): Unknown entry type -- not extracting." fname))
	   (t (error "%s(): Impossible condition." fname)))))

(defun cpio-extract-regular-file (entry-name)
  "Extract the regular file entry ENTRY-NAME.
CONTRACT: ENTRY-NAME is in fact an entry of a regular file."
  (let* ((fname "cpio-extract-regular-file")
	 (do-it (if from-lisp
		    t
		  (cpio-ask-user-about-supersession-threat entry-name)))
	 (buffer-name (cpio-contents-buffer-name entry-name))
	 (temp-buffer (get-buffer buffer-name))
	 (restore (buffer-live-p temp-buffer))
	 (contents)
	 (archive-buffer (if *cab-parent*
			     *cab-parent*
			   (current-buffer))))
    (if do-it
	(cond (temp-buffer
	       (setq contents (cpio-contents entry-name archive-buffer))
	       (cab-register temp-buffer archive-buffer)
	       (with-current-buffer temp-buffer
		 (insert contents)
		 (write-file entry-name))
	       (unless restore (kill-buffer temp-buffer)))
	      (t
	       (with-temp-buffer 
		 (insert (cpio-contents entry-name archive-buffer))
		 (write-file entry-name)
		 (unless restore (kill-buffer temp-buffer)))
	       (cpio-set-file-attrs entry-name))))))

(defun cpio-extract-directory (entry-name)
  "Extract the directory entry ENTRY-NAME.
CONTRACT: ENTRY-NAME really is a directory entry."
  (let ((fname "cpio-extract-directory")
	(attrs (cpio-entry-attrs entry-name)))
    (make-directory entry-name t)
    (cpio-set-file-attrs entry-name)))

(defun cpio-entry-type (entry-name)
  "Return the type of the entry with the given ENTRY-NAME.
The type is the single character that would be displayed
in the initial mode slot of 'ls -l'.
That is, 'l' is a link, '-' is a regular file, etc.
See (cpio-int-mode-to-file-type) in cpio-modes.el for more detail.
If ENTRY-NAME is not in the current archive, then return NIL."
  (let* ((fname "cpio-entry-type")
	 (entry-attrs)
	 (entry-mode))
    (cond ((and entry-name
		(setq entry-attrs (cpio-entry-attrs entry-name))
		(setq entry-mode (cpio-mode-value entry-attrs)))
	   (cpio-int-mode-to-file-type entry-mode))
	  (t nil))))

(defun cpio-numeric-entry-type (numeric-mode)
  "Return the numeric entry type of the given NUMERIC MODE."
  (let ((fname "cpio-numeric-entry-type"))
    ;; (error "%s() is not yet implemented" fname)
    (cond ((= #o170000 (logand s-ifmt   numeric-mode))
	   s-ifmt)
	  ((= #o140000 (logand s-ifsock numeric-mode))
	   s-ifsock)
	  ((= #o120000 (logand s-iflnk  numeric-mode))
	   s-iflink)
	  ((/= 0       (logand s-ifreg  numeric-mode))
	   s-ifreg)
	  ((/= 0       (logand s-ifdir  numeric-mode))
	   s-ifdir)
	  ((/= s-ifblk (logand s-ifblk  numeric-mode))
	   s-ifblk)
	  ((/= 0       (logand s-ifchr  numeric-mode))
	   s-ifchr)
	  ((/= 0       (logand s-ififo  numeric-mode))
	   s-ififo)
	  (t
	   s_iunk))))

(defun cpio-set-file-attrs (file-name)
  "Set the attributes on FILE-NAME
based on its attributes in the catalog."
  (let* ((fname "cpio-set-file-attrs")
	 (attrs (cpio-entry-attrs file-name))
	 (mode-value (cpio-mode-value attrs))
	 (mod-time-string (cpio-mtime-to-touch-string (cpio-mtime attrs)))
	 (uid (cpio-uid-to-uid-string (cpio-uid attrs)))
	 (gid (cpio-gid-to-gid-string (cpio-gid attrs))))
    (call-process "chown"
		  nil
		  nil
		  nil
		  uid
		  file-name)
    (call-process "chgrp"
		  nil
		  nil
		  nil
		  gid
		  file-name)
    (chmod file-name mode-value)
    (call-process "touch"
		  nil
		  nil
		  nil
		  "-t"
		  mod-time-string
		  file-name)))

(defun cpio-mtime-to-touch-string (mtime)
  "Convert the given MTIME to a time that touch(1) understands.
MTIME is an emacs time.
Touch understands times of the form YYYYMMDDhhmm.ss."
  (let ((fname "cpio-mtime-to-touch-string"))
    (format-time-string "%Y%m%d%M%H.%S" mtime)))

(defun cpio-adjust-trailer ()
  "Replace the trailer in the current buffer
with one with the correct size fot its contents."
  (let* ((fname "cpio-adjust-trailer"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (funcall cpio-adjust-trailer-func))
      (funcall cpio-adjust-trailer-func))))

(defun cpio-insert-trailer ()
  "Insert a trailer in a cpio archive."
  (let ((fname "cpio-insert-trailer"))
    ;; (error "%s() is not yet implemented" fname)
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (funcall cpio-insert-trailer-func))
      (funcall cpio-insert-trailer-func))))

(defun cpio-delete-trailer ()
  "Delete the trailer in the cpio archive buffer affiliated with the current buffer."
  (let ((fname "cpio-delete-trailer"))
    ;; (error "%s() is not yet implemented" fname)
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (funcall cpio-delete-trailer-func))
      (funcall cpio-delete-trailer-func))))

(defun cpio-delete-archive-entry (entry)
  "Delete the entry in the cpio archive specified by ENTRY.
ENTRY is a catalog entry."
  (let ((fname "cpio-delete-archive-entry"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-delete-archive-entry entry))
      (let* ((attrs (aref entry 0))
	     (size (cpio-entry-size attrs))
	     (entry-start (cpio-entry-header-start entry))
	     (contents-start (cpio-contents-start (cpio-entry-name attrs)))
	     (entry-end (1+ (round-up (+ (1- contents-start)
					 (cpio-entry-size attrs))
				      *cpio-padding-modulus*))))
	(setq buffer-read-only nil)
	(delete-region entry-start entry-end)
	(setq buffer-read-only t)))))

(defun cpio-insert-padded-header (header-string)
  "Insert an appropriately padded version of HEADER-STRING."
  (let ((fname "cpio-insert-padded-header")
	(padding))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-insert-padded-header header-string))
      (insert (cpio-padded header-string *cpio-padding-modulus* *cpio-padding-char*)))))

(defun cpio-insert-padded-contents (contents) ;HEREHERE Generic
  "Insert an appropriately padded version of CONTENTS into the archive buffer."
  (let ((fname "cpio-insert-padded-contents"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-insert-padded-contents contents))
      (insert (cpio-padded contents *cpio-padding-modulus* *cpio-padding-char*)))))

(defun cpio-dired-buffer-name (archive-name)
  "Return the name of the dired-style buffer for ARCHIVE-NAME."
  (let ((fname "cpio-dired-buffer-name"))
    (concat "CPIO archive: " (file-name-nondirectory archive-name))))

(defun cpio-present-ala-dired (archive-buffer)
  "Create a buffer with a ls -l format reflecting the contents of the current cpio archive.
This returns the buffer created."
  (let* ((fname "cpio-present-ala-dired")
	 (archive-name (with-current-buffer archive-buffer
			 (file-name-nondirectory (buffer-file-name))))
	 (buffer-name (cpio-dired-buffer-name archive-name))
	 (buffer (get-buffer-create buffer-name)) ;Is this not archive-buffer?
	 (entry-string)
	 (catalog (cpio-catalog)))
    (with-current-buffer buffer
      (setq *cpio-catalog* catalog)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "CPIO archive: " archive-name ":\n\n")
      (mapc (lambda (e)
	      (let ((line (cpio-dired-format-entry (aref (cdr e) 0))))
		(insert (concat line "\n"))))
	    (cpio-sort-catalog))
      (setq buffer-read-only t)
      (cpio-dired-mode)
      (cpio-dired-move-to-first-entry))
    ;; No, I do not yet understand why this must be done
    ;; every time the presentation is updated.
    (cab-register buffer archive-buffer)
    buffer))

(defun cpio-dired-move-to-first-entry ()
  "Move the point to the first entry in a cpio-dired style buffer."
  (let ((fname "cpio-dired-move-to-first-entry"))
    ;; (error "%s() is not yet implemented" fname)
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (goto-char (point-min))
    (cpio-dired-next-line *cpio-dired-head-offset*)))

(defun cpio-sort-catalog ()
  "Return a copy of the catalog sorted by entry name (car cpio-catalog-entry)."
  (let ((fname "cpio-sort-catalog"))
    (sort *cpio-catalog* 'cpio-entry-less-p)))

(defun cpio-entry-less-p (l r)
  "Return non-nil if [the car of] entry L precedes [the car of] entry L.
CONTRACT: L and R should be entries:
        (entry-name [inode mode uid ...] entry-start entry-end)."
  (let ((fname "cpio-entry-less-p"))
    (string-lessp (car l) (car r))))

(defun cpio-dired-format-entry (attrs &optional mark)
  "Create a dired-style line for ATTRS.
If the optional MARK is given,
then it is a character and used as the mark on the generated line.
The line does not include a trailing <new line>."
  (let* ((fname "cpio-dired-format-entry")
	 (mode-string       (cpio-int-mode-to-mode-string         (cpio-mode-value attrs)))
	 (uid-string        (cpio-uid-to-uid-string               (cpio-uid        attrs)))
	 (gid-string        (cpio-gid-to-gid-string               (cpio-gid        attrs)))
	 (nlink-string      (cpio-nlink-to-nlink-string           (cpio-nlink      attrs)))
	 (mtime-string      (cpio-mtime-to-mtime-string           (cpio-mtime      attrs)))
	 (filesize-string   (cpio-filesize-to-filesize-string     (cpio-entry-size attrs)))
	 (dev-maj-string    (cpio-dev-maj-to-dev-maj-string       (cpio-dev-maj    attrs)))
	 (dev-min-string    (cpio-dev-min-to-dev-min-string       (cpio-dev-min    attrs)))
	 (entry-name-string (cpio-entry-name-to-entry-name-string (cpio-entry-name attrs)))
	 (fmt (if entry-name-string
		  (if cpio-try-names
		      (format "%%c %%s %%3s %%8s %%8s %%8s %%7s %%s")
		    (format   "%%c %%s %%3s %%5s %%5s %%8s %%7s %%s"))
		nil)))
    (unless mark (setq mark ?\s))
    (unless (characterp mark)
      (signal 'wrong-type-error (list 'characterp mark)))
    (if fmt
	(format fmt mark 
		mode-string nlink-string uid-string gid-string 
		filesize-string mtime-string entry-name-string))))

(defun cpio-uid-to-uid-string (uid)
  "Convert the given UID, an integer, to a string."
  (let ((fname "cpio-uid-to-uid-string"))
    (if cpio-try-names
	(or (user-login-name uid)
	    (number-to-string uid))
      (number-to-string uid))))

(defun cpio-gid-to-gid-string (gid)
  "Convert the given GID, an integer, to a string."
  (let ((fname "cpio-gid-to-gid-string"))
    (if cpio-try-names
	(or (user-login-name gid)
	    (number-to-string gid))
      (number-to-string gid))))

(defun cpio-nlink-to-nlink-string (nlink)
  "Convert the given NLINK, an integer, to a string."
  (let ((fname "cpio-nlink-to-nlink-string"))
    (number-to-string nlink)))

(defun cpio-mtime-to-mtime-string (mtime &optional long)
  "Convert the given MTIME, an emacs internal time, to a string.
CAUTION: This depends on your emacs being able to handle
a UNIX/GNU/Linux time as an integer."
  (let ((fname "cpio-mtime-to-mtime-string")
	(six-months (* 6 30 24 60 60))
	(now (time-to-seconds (current-time)))
	(tmp-time (time-to-seconds mtime)))
    (cond (long
	   (format-time-string "%F %T" mtime))
	  ((< (- now tmp-time) six-months)
	   (format-time-string "%b %d %H:%M" mtime))
	  (t
	   (format-time-string "%b %d %Y " mtime)))))

(defun cpio-filesize-to-filesize-string (filesize)
  "Convert the given FILESIZE, an integer, to a string."
  (let ((fname "cpio-filesize-to-filesize-string"))
    (number-to-string filesize)))

(defun cpio-dev-maj-to-dev-maj-string (dev-maj)
  "Do Convert the given DEV-MAJ, an integer, to a string."
  (let ((fname "cpio-dev-maj-to-dev-maj-string"))
    (number-to-string dev-maj)))

(defun cpio-dev-min-to-dev-min-string (dev-min)
  "Do Convert the given DEV-MIN, an integer, to a string."
  (let ((fname "cpio-dev-min-to-dev-min-string"))
    (number-to-string dev-min)))

(defun cpio-entry-name-to-entry-name-string (name)
  "DConvert the given NAME, an integer, to a string."
  (let ((fname "cpio-entry-name-to-entry-name-string"))
    name))

(defun cpio-find-entry (entry-name)
  "Find the given ENTRY-NAME and return the buffer holding its contents."
  (let ((fname "cpio-dired--find-entry")
	(target-buffer))
    ;; (error "%s() is not yet implemented" fname)
    (if (null (setq target-buffer (get-buffer-create (cpio-contents-buffer-name entry-name))))
	(error "%s(): Could not get a buffer for entry [[%s]]." fname))
    (cab-register target-buffer *cab-parent*)
    (with-current-buffer target-buffer
      (cond ((or (/= 0 (1- (point)))
		 (= 0 (length (buffer-string))))
	     (erase-buffer) 		;This should not be necessary.
	     (insert (cpio-contents entry-name))
	     ;; (setq buffer-read-only t)
	     (goto-char (point-min)))
	    (t t))
      (make-variable-buffer-local 'cpio-entry-name)
      (setq cpio-entry-name entry-name)
      (set-buffer-modified-p nil)
      (cpio-entry-contents-mode))
    (pop-to-buffer target-buffer)))

(defun cpio-create-entry-attrs (filename)
  "Create an entry attribute structure based on the given FILENAME."
  (let* ((fname "cpio-create-entry-attrs")
	 (attrs (file-attributes filename))

	 (ino (nth 10 attrs))
	 (mode (cpio-mode-string-to-int-mode (nth 8 attrs)))
	 (uid (nth 2 attrs))
	 (gid (nth 3 attrs))

	 (nlink 1)
	 (mtime (time-to-seconds (nth 5 attrs)))
	 (entry-size (nth 7 attrs))
	 (dev-maj (nth 11 attrs))

	 (dev-min 1)
	 (rdev-maj 0)
	 (rdev-min 0)
	 (namesize (length filename))

	 (checksum 0)

	 (result (make-vector 14 nil)))
    ;; (error "%s() is not yet implemented" fname)
    (aset result *cpio-ino-parsed-idx*        ino)
    (aset result *cpio-mode-parsed-idx*       mode)
    (aset result *cpio-uid-parsed-idx*        uid)
    (aset result *cpio-gid-parsed-idx*        gid)

    (aset result *cpio-nlink-parsed-idx*      nlink)
    (aset result *cpio-mtime-parsed-idx*      (seconds-to-time mtime))
    (aset result *cpio-entry-size-parsed-idx* entry-size)
    (aset result *cpio-dev-maj-parsed-idx*    dev-maj)

    (aset result *cpio-dev-min-parsed-idx*    dev-min)
    (aset result *cpio-rdev-maj-parsed-idx*   rdev-maj)
    (aset result *cpio-rdev-min-parsed-idx*   rdev-min)
    (aset result *cpio-namesize-parsed-idx*   namesize)
    
    (aset result *cpio-checksum-parsed-idx*   checksum)
    (aset result *cpio-name-parsed-idx*       filename)

    result))

(defun cpio-create-faux-directory-attrs (name)
  "Create attributes appropriate for adding a directory entry to a cpio-archive.
CAVEAT: While many attributes are derived from a best guess of reality,
many are simply invented."
  (let* ((fname "cpio-create-faux-directory-attrs")
	 (local-attrs (file-attributes "."))
	 (ino 1)
	 ;; HEREHERE think about basing the mode on umask or local-attrs.
	 (mode (logior s-ifdir
		       s-irwxu
		       s-irusr
		       s-ixusr))
	 (uid (user-uid))
	 (gid (group-gid))
	 
	 (nlink 1)
	 (mtime (current-time))
	 (entry-size 0)
	 (dev-maj 1)
	 (dev-min 1)
	 (rdev-maj 0)
	 (rdev-min 0)
	 (namesize (1+ (length name)))
	 (checksum 0) 			;HEREHERE This will have to change.
	 (result (make-vector 14 nil)))
    ;;(error "%s() is not yet implemented" fname)
    (aset result *cpio-ino-parsed-idx* ino)
    (aset result *cpio-mode-parsed-idx* mode)
    (aset result *cpio-uid-parsed-idx* uid)
    (aset result *cpio-gid-parsed-idx* gid)
    
    (aset result *cpio-nlink-parsed-idx* nlink)
    (aset result *cpio-mtime-parsed-idx* mtime)
    (aset result *cpio-entry-size-parsed-idx* entry-size)
    (aset result *cpio-dev-maj-parsed-idx* dev-maj)
    
    (aset result *cpio-dev-min-parsed-idx* dev-min)
    (aset result *cpio-rdev-maj-parsed-idx* rdev-maj)
    (aset result *cpio-rdev-min-parsed-idx* rdev-min)
    (aset result *cpio-namesize-parsed-idx* namesize)
    
    (aset result *cpio-checksum-parsed-idx* checksum)
    (aset result *cpio-name-parsed-idx* name)
    
    result))

(defun cpio-entry-exists-p (name)
  "Return non-nil if there's already an entry called NAME
in the current archive."
  (let ((fname "cpio-entry-exists-p"))
    ;; (error "%s() is not yet implemented" fname)
    (assoc name (cpio-catalog))))

(defun cpio-move-to-entry (entry-name)
  "Move the point to ENTRY-NAME."
  (let ((fname "cpio-move-to-entry")
	(where nil))
    ;; (error "%s() is not yet implemented" fname)
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (not (looking-at-p (concat entry-name "$")))
	(cpio-dired-next-line 1))
      (if (looking-at-p (concat entry-name "$"))
	  (setq where (point))))
    (if where
	(goto-char where))))


;; 
;; Commands
;; 

(defun cpio-view-dired-style-buffer ()
  "Switch to the dired style buffer corresponding to this archive buffer."
  ;; (cpio-dired-buffer-name) is in cpio.el because
  ;; it is invoked in the archive's directory.
  (interactive)
  (let ((fname "cpio-view-dired-style-buffer")
	(archive-file-name (buffer-file-name)))
    (unless (eq major-mode 'cpio-mode)
      (error "%s(): You're not in a cpio archive buffer under cpio-mode." fname))
    (bury-buffer)
    (switch-to-buffer (cpio-dired-buffer-name archive-file-name))))


;; 
;; Mode definition
;; 

;; HEREHERE I'm hoping dired-mode gives me decent stuff for free.
;; dired-mode -- Nope the hooks for dired-mode want a nicer environment.
;; I'll have to see how tar-mode does it.
(define-derived-mode cpio-mode fundamental-mode "cpio-mode"
  "Treat cpio archives like file systems with a dired UI."
  (if (null (setq *cpio-format* (cpio-discern-archive-type)))
      (error "You're not in a supported CPIO buffer."))
  (message "You're in a cpio buffer of type [[%s]]." (symbol-name *cpio-format*)) ; (sit-for 1.0)
  (let ((archive-buffer (current-buffer))
	(cpio-dired-buffer))
    (setq buffer-read-only t)
    (message "Establishing local variables.")
    (cpio-set-locals *cpio-format*)
    (setq *cpio-archive-name* (buffer-file-name))
    (cpio-build-catalog)
    (with-current-buffer (setq cpio-dired-buffer
			       (cpio-present-ala-dired (current-buffer)))
      (cpio-dired-set-unmodified))
    (cpio-create-keymap)
    (bury-buffer)
    ;; cpio-mode is the top level function here,
    ;; so this should control what we see at this point.
    (switch-to-buffer cpio-dired-buffer)))

(defvar *cpio-dired-modified* nil
  "A flag to record if any archive-modifying events have occured
since either the beginning or the last save.")
(make-variable-buffer-local '*cpio-dired-modified*)

(defun cpio-dired-modified-p ()
  "Return non-NIL if the catalog has been modified
and, thus, the archive can be saved."
  (let ((fname "cpio-dired-modified-p"))
    ;; (error "%s() is not yet implemented" fname)
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): only makes sense in a cpio-dired buffer."))
    *cpio-dired-modified*))

(defun cpio-dired-set-modified ()
  "Flag the catalog as modified."
  (let ((fname "cpio-dired-set-modified"))
    ;; (error "%s() is not yet implemented" fname)
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): only makes sense in a cpio-dired buffer."))
    (setq *cpio-dired-modified* t)))

(defun cpio-dired-set-unmodified ()
  "Flag the catalog as not modified."
  (let ((fname "cpio-dired-set-unmodified"))
    ;; (error "%s() is not yet implemented" fname)
    ;; HEREHERE There's probably more to this than just the following.
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): only makes sense in a cpio-dired buffer."))
    (setq *cpio-dired-modified* t)
    (with-current-buffer *cab-parent*
      (set-buffer-modified-p nil))))

(defvar *cpio-have-made-keymap* nil
  "Flag to indicate that the cpio-mode-map has already been built.")
(defun cpio-create-keymap ()
  (let ((fname "cpio-create-keymap")
	(keymap (make-keymap)))
    (setq cpio-mode-map keymap)
    (unless *cpio-have-made-keymap*
      (define-key cpio-mode-map "\C-c\C-c" 'cpio-view-dired-style-buffer)
      (define-key cpio-mode-map        "q" 'cpio-quit))))

(defun cpio-quit ()
  "Quit cpio mode and kill all the affiliated buffers."
  (interactive)
  (let ((fname "cpio-quit"))
    (if *cab-parent*
	(with-current-buffer *cab-pattern*
	  (cpio-quit)
	  (unless (kill-buffer)
	    (warn "%s(): Could not kill [[%s]]." fname (current-buffer)))))))

(defun cpio-entry (entry-name)
  "Return the entry in the catalog for the entry with ENTRY-NAME."
  (let ((fname "cpio-entry"))
    (if *cab-parent*
	(cdr (assoc entry-name (with-current-buffer *cab-parent*
				 *cpio-catalog*)))
      (cdr (assoc entry-name *cpio-catalog*)))))

(defun cpio-entry-attrs-from-catalog-entry (catalog-entry)
  "Do that."
  (let ((fname "cpio-entry c-attrs-from-catalog"))
    (aref catalog-entry *cpio-catalog-entry-attrs-idx*)))

(defun cpio-build-catalog ()
  "Build the catalog that tracks the entries in this cpio-mode buffer.
cpio-mode maintains the catalog in the *cpio-catalog* variable."
  (let ((fname "cpio-build-catalog"))
    (goto-char (point-min))
    (setq *cpio-catalog* (funcall cpio-build-catalog-func))))

(defun cpio-set-locals (archive-type)
  "Establish certain variables as local to the current buffer and give them good values.
ARCHIVE-TYPE is a symbol."
  (let ((fname "cpio-set-locals"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-set-local-vars))
      (cpio-set-local-vars archive-type)
      (cpio-set-local-funcs archive-type))))

(defun cpio-set-local-funcs (archive-type)
  "Establish the functions for the given archive type.
The functions are assigned to the elements of *cpio-local-funcs*,
a string of symbols.
Thus, to use them as functions
you need to (funcall) them or to (apply) them.
CAVEAT: No checking is done.
This function doesn't care /why/ you are asking for functions
that are appropriate for ARCHIVE-TYPE.
That's the caller's business.

See *cpio-local-funcs* for more information."
  (let ((fname "cpio-set-local-funcs")
	(archive-type-name (symbol-name archive-type)))
    (mapc 'make-local-variable
	  *cpio-local-funcs*)
    ;; Here is the process:
    ;; cpio-do-good-stuff-func
    ;; --> "cpio-do-good-stuff-func"
    ;; --> "cpio" "do" "good" "stuff"
    ;; --> "cpio-fmt-do-good-stuff"
    ;; --> cpio-fmt-do-good-stuff
    ;; (setq cpio-do-good-stuff-func cpio-hdr-do-good-stuff)
    ;; Here's an example of the desired result for newc headers.
    ;; (setq cpio-parse-header-func 'cpio-newc-parse-header)
    (mapc (lambda (local-func-var)
	    (let* ((name-parts (split-string (symbol-name local-func-var) "-"))
		   (target-name (concat (car name-parts) "-" ;HEREHERE Should this be (pop)?
					(symbol-name archive-type))))
	      (setq name-parts (cdr (remove "func" name-parts)))
	      (mapc (lambda (part)
		      (setq target-name (concat target-name "-" part)))
		    name-parts)
	      ;; 1. (set) not (setq) because
	      ;; 2. local-func-var holds a symbol)
	      (set local-func-var (read target-name))
	      target-name))
	  *cpio-local-funcs*)))

(defun cpio-set-local-vars (archive-type)
  "Set all the necessary local variables for the CPIO archive type given."
  (let ((fname "cpio-set-local-vars"))
    ;; Some variables are not format-specific.
    (make-local-variable '*cpio-catalog*)
    (setq *cpio-catalog* ())
    (make-variable-buffer-local '*cpio-archive-name*)
    (setq *cpio-archive-name* (buffer-file-name))
    ;; Now for the format-specific variables.
    (cond ((eq archive-type 'bin)
	   (cpio-set-local-bin-vars))
	  ((eq archive-type'newc)
	   (cpio-set-local-newc-vars))
	  ((eq archive-type'odc)
	   (cpio-set-local-odc-vars))
	  ((eq archive-type'crc)
	   (cpio-set-local-crc-vars))
	  ((eq archive-type'tar)
	   (cpio-set-local-tar-vars))
	  ((eq archive-type'ustar)
	   (cpio-set-local-ustar-vars))
	  ((eq archive-type'hpbin)
	   (cpio-set-local-hpbin-vars))
	  ((eq archive-type'hpodc)
	   (cpio-set-local-hpodc-vars))
	  (t (error "%s(): Unknown archive type [[%s]]" fname archive-type)))))

(defun cpio-set-local-bin-vars ()
  "Set buffer local variables appropriate for a BIN format CPIO archive."
  (let ((fname "cpio-set-local-bin-vars"))))

(defun cpio-set-local-newc-vars ()
  "Set buffer local variables appropriate for a NEWC format CPIO archive."
  (let ((fname "cpio-set-local-newc-vars"))
    (cpio-set-local-newc-offset-vars)
    (make-local-variable '*cpio-padding-modulus*)
    (setq *cpio-padding-modulus* *cpio-newc-padding-modulus*)
    (make-local-variable '*cpio-padding-char*)
    (setq *cpio-padding-char* *cpio-newc-padding-char*)
    (make-local-variable '*cpio-padding-str*)
    (setq *cpio-padding-str* *cpio-newc-padding-str*)))

;; 
;; Assuming this has worked. (It runs without errors.)
;; I think I now need to use (symbol-value) 
;; to get the archive-specific information that I need.
;; 
(defun cpio-set-local-newc-offset-vars ()
  "Set the variables that define the offset to the fields of a NEWC format CPIO archive.
This also establishes those variables as buffer-local."
  ;; There is an implicit contract that you are in the right buffer.
  (let ((fname "cpio-set-local-newc-offset-vars")
	(newc-offset-vars (list '*cpio-newc-magic-field-offset*
				'*cpio-newc-ino-field-offset*
				'*cpio-newc-mode-field-offset*
				'*cpio-newc-uid-field-offset*
				'*cpio-newc-gid-field-offset*
				'*cpio-newc-nlink-field-offset*
				'*cpio-newc-mtime-field-offset*
				'*cpio-newc-filesize-field-offset*
				'*cpio-newc-dev-maj-field-offset*
				'*cpio-newc-dev-min-field-offset*
				'*cpio-newc-rdev-maj-field-offset*
				'*cpio-newc-rdev-min-field-offset*
				'*cpio-newc-namesize-field-offset*
				'*cpio-newc-chksum-field-offset*
				'*cpio-newc-name-field-offset*))
	(format-name "newc"))
    (mapcar 'make-local-variable newc-offset-vars)
    ;; We want a simple and consistent map
    ;; between the global [meta-]variable's name
    ;; and the format-specific variable's name.
    ;; We know the format-specific names,
    ;; so start with them.
    ;; Here's the process:
    ;;     *newc-magic-field-offset*
    ;;     --> *cpio-magic-field-offset*
    (mapcar (lambda (fsv)		;format-specific-variable
	      (let* ((fs-name (symbol-name fsv))
		     (general-name (progn (or (string-match (concat "\\`\\*cpio-\\(" format-name "-\\)") fs-name)
					      (error "%s(): Some FSV isn't playing by the rules! [[%s]]" fname fsv))
					  (replace-match "" t t fs-name 1)))
		     (general-var (make-symbol general-name)))
		(set general-var fsv)))
	    newc-offset-vars)))

(defun cpio-set-local-odc-vars ()
  "Set buffer local variables appropriate for a ODC format CPIO archive."
  (let ((fname "cpio-set-local-odc-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-crc-vars ()
  "Set buffer local variables appropriate for a CRC format CPIO archive."
  (let ((fname "cpio-set-local-crc-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-tar-vars ()
  "Set buffer local variables appropriate for a TAR format CPIO archive."
  (let ((fname "cpio-set-local-tar-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-ustar-vars ()
  "Set buffer local variables appropriate for a USTAR format CPIO archive."
  (let ((fname "cpio-set-local-ustar-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-hpbin-vars ()
  "Set buffer local variables appropriate for a HPBIN format CPIO archive."
  (let ((fname "cpio-set-local-hpbin-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-hpodc-vars ()
  "Set buffer local variables appropriate for a HPODC format CPIO archive."
  (let ((fname "cpio-set-local-hpodc-vars"))
    (error "%s() is not yet implemented" fname)))


(provide 'cpio)
;;; cpio.el ends here
