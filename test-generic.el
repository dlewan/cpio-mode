;;; test-generic.el --- Generic code to support automated tests. -*- coding: utf-8 -*-

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
;; Created: 2018 Nov 19
;; Version: 0.13β
;; Keywords: files

;;; Commentary:

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 

;;
;; Vars
;;

(defvar *cdmt-header-re* ()
  "Variable to hold the format-specific RE to match a header.")
;; These indexes don't seem to be used.
;; (defvar *cpio-magic-re-idx* ())
;; (defvar *cpio-mode-re-idx* ())
;; (defvar *cpio-uid-re-idx* ())
;; (defvar *cpio-gid-re-idx* ())
;; (defvar *cpio-nlink-re-idx* ())
;; (defvar *cpio-filesize-re-idx* ())
;; (defvar *cpio-namesize-re-idx* ())
;; (defvar *cpio-chksum-re-idx* ())
;; (defvar *cpio-filename-re-idx* ())

(defvar *cdmt-archive-format* ())


(defun cdmt-filter-archive-contents (archive-contents)
  "Make the given ARCHIVE-CONTENTS fully printable and readable."
  (let ((fname "cdmt-filter-archive-contents")
	(char-map (list (cons "\0" "\\0"))))
    (setq archive-contents (cdmt-reformat-headers archive-contents))
    (mapc (lambda (cm)
	    (let ((from (car cm))
		  (to (cdr cm)))
	      (setq archive-contents (cdmt-global-sub from to archive-contents))))
	  char-map)
    archive-contents))

(defun cdmt-reset (&optional make large)
  "Reset the current cpio-dired environment.
If MAKE is non-nil, then run 'make FORMAT' as part of the reset."
  (let ((fname "cdmt-reset")
	(archive-name)
	(archive-names (list *cdmt-small-archive*
			     *cdmt-large-archive*)))
    (cd run-dir)
    
    (mapc (lambda (b)
	    (if (and (buffer-live-p b)
		     (with-current-buffer b
		       buffer-file-name)
		     (with-current-buffer b
		       (or (eq major-mode 'cpio-mode)
			   (eq major-mode 'cpio-dired-mode)
			   (member 'cpio-entry-contents-mode (current-minor-modes)))))
		(with-current-buffer b
		  (cpio-not-modified))))
	  (buffer-list))
	  
    (mapc (lambda (b)
	    (if (and (buffer-live-p b)
		     (with-current-buffer b
		       buffer-file-name)
		     (with-current-buffer b
		       (or (eq major-mode 'cpio-mode)
			   (eq major-mode 'cpio-dired-mode)
			   (member 'cpio-entry-contents-mode (current-minor-modes)))))
	      (kill-buffer b)))
	  (buffer-list))


    (cd run-dir)
    
    (if make
	(shell-command (format "cd test_data/alphabet ; make clean %s" *cdmt-archive-format*) nil nil))
    
    (setq archive-name (if large 
			   *cdmt-large-archive*
			 *cdmt-small-archive*))
    
    (delete-other-windows)
    (with-current-buffer (setq cpio-archive-buffer (find-file-noselect archive-name))
      (if (string-match "/test_data/.+/test_data/" (buffer-file-name))
	  (error "Bogus archive!"))
      (cpio-mode))
    (setq cpio-dired-buffer (switch-to-buffer (cpio-dired-buffer-name archive-name)))))

(defun cdmt-reformat-headers (archive-contents)
  "Reformat the cpio entry headers in the given hex ARCHIVE-CONTENTS
so that they are human readable.
The hex archive formats are newc and crc.
CAVEATS: \(1\) If ARCHIVE-CONTENTS contains entries that contain entry headers,
               then those will also be reformatted.
         \(2\) The entry names are taken to be a sequence of printable characters.
               So, if NULLs have been converted to printable characters,
               then the entry names will be incorrect."
  (let ((fname "cdmt-reformat-headers"))
    (while (string-match *cdmt-header-re* archive-contents)
      (setq archive-contents (concat (substring archive-contents 0 (match-beginning 0))
				     (concat (match-string-no-properties *cpio-magic-re-idx*    archive-contents) "\t(( magic    ))\n")
				     (concat "DEADBEEF"                                                           "\t(( ino      ))\n")
				     (concat (match-string-no-properties *cpio-mode-re-idx*     archive-contents) "\t(( mode     ))\n")
				     (concat (match-string-no-properties *cpio-uid-re-idx*      archive-contents) "\t(( uid      ))\n")
				     (concat (match-string-no-properties *cpio-gid-re-idx*      archive-contents) "\t(( gid      ))\n")
				     (concat (match-string-no-properties *cpio-nlink-re-idx*    archive-contents) "\t(( nlink    ))\n")
				     (concat "DEADBEEF"                                                           "\t(( mtime    ))\n")
				     (concat (match-string-no-properties *cpio-filesize-re-idx* archive-contents) "\t(( filesize ))\n")
				     (concat "DEADBEEF"                                                           "\t(( dev maj  ))\n")
				     (concat "DEADBEEF"                                                           "\t(( dev min  ))\n")
				     (concat "DEADBEEF"                                                           "\t(( rdev maj ))\n")
				     (concat "DEADBEEF"                                                           "\t(( rdev min ))\n")
				     (concat (match-string-no-properties *cpio-namesize-re-idx* archive-contents) "\t(( namesize ))\n")
				     (concat (match-string-no-properties *cpio-chksum-re-idx*   archive-contents) "\t(( chksum   ))\n")
				     (concat (match-string-no-properties *cpio-filename-re-idx* archive-contents) "\t(( filename ))\n")
				     (substring archive-contents (match-end 0)))))
    (concat archive-contents "\n")))

;; The following few functions can be refactored with a FORMAT arg.
;; cdmt-unfinished-command 
;; cdmt-finished-command 
;; cdmt-finished-command
;; cdmt-all-finished-commands

;;
;; The contents of these tests were generated
;; from the dired-mode-map definition
;; (where commands are tagged as done).
;; A few keyboard macros modified those results.
;; The following commands were used to create the skeletons below

(defun cdmt-unfinished-command ()	;It doesn't look like FORMAT is needed.
  "Create a stub test for the next unfinished command."
  (interactive)
  (let ((fname "cdmt-unfinished-command")
	(test-declaration-string "cdmt-")
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

(defun cdmt-all-unfinished-commands ()	; FORMAT isn't needed.
  "Write stub tests for all unfinished commands following point."
  (interactive)
  (let ((fname "cdmt-all-unfinished-commands"))
    (while (cdmt-unfinished-command))))

(defun cdmt-finished-command (archive-format)
  "Write a stub test for a finished command for the given FORMAT."
  (interactive)
  (let ((fname "cdmt-finished-command")
	(finished-command-regexp ") ;✓$")
	(command-name)
	(where))
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
	   (insert (format "  \"Test the function M-x cpio-%s-%s.\"\n" format command-name))
	   (insert         "  (shell-command (format \"cd test_data/alphabet ; make %s\" nil nil)\n" archive-format)
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
	  (t nil))))

(defun cdmt-all-finished-commands (archive-format)
  "Build stub tests for all the finished commands."
  (interactive)
  (let ((fname "cdmt-all-finished-commands"))
    (while (cdmt-finished-command archive-format))))


;; The following functions are already generic (Check that!):
;; cdmt-sweep
;; cdmt-do-cpio-id
;; cdmt-sweep-ids
;; cdmt-sweep-times
;; cm-setup
;; cdmt-sweep-catalog -- will require picking the right version.
;; cdmt-tidy-up-catalog -- Also look for (pp ...).
;; cdmt-test-save -- will require picking the right version.

(defun cdmt-tidy-up-catalog ()
  "Remove [ and ] from the pretty printed catalog and replace them with » and « respectively.
Return the new string."
  (let ((fname "cdmt-bin-tidy-up-catalog")
	(catalog-string (pp (cpio-catalog) 'cdmt-noop))
	(substitutions (list (cons "\\[" "»")
			     (cons "\\]" "«")
			     (cons "\\\"" "¨"))))
    (mapc (lambda (si)
	    (let ((match       (car si))
		  (replacement (cdr si)))
	      (while (string-match match catalog-string)
		(setq catalog-string (replace-match replacement nil t catalog-string)))))
	  substitutions)
    catalog-string))

(defun cdmt-test-save (format &optional large)
  "A generic test to run at the end of every test
to check that the saved archive seems sane.
FORMAT is a string denoting the cpio-format of the archive.
The default archive is the small archive.
However, if LARGE is not NIL, then save the large archive."
  (cd run-dir)
  (let* ((fname "cdmt-test-save")
	 (alphabet-dir (concat default-directory "test_data/alphabet"))
	 (test-buffer-dir (concat alphabet-dir "/asdfasdf"))
	 (test-buffer)
	 (directory default-directory)
	 (dired-buffer (current-buffer))
	 (archive (if large (format "../alphabet.%s.cpio" format)
		    (format "../alphabet_small.%s.cpio" format))))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
	 
    (if (file-exists-p test-buffer-dir)
	(call-process "rm" nil nil nil "-rf" test-buffer-dir))
    (if (file-exists-p test-buffer-dir)
	(error "%s(): Removing %s failed." fname test-buffer-dir))
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

(defun cdmt-global-sub (from-str to-str string)
  "Globally substitute TO-STR for FROM-STR in STRING and return the new string.
In principal, FROM-STR can be a regular expression."
  (let ((fname "cdmt-global-sub"))
    (while (string-match from-str string)
      (setq string (replace-match to-str nil t string 0)))
    string))

;; Search for »hex« to carry this function through.
(defun cdmt-sweep-hex ()
  "Convert the hex fields to octal fields."
  (interactive)
  (let ((fname "cdmt-sweep-hex")
	(value)
	(replacement))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:xdigit:]]\\{8\\}" (point-max) t)
	(setq value (string-to-number (match-string 0) 16))
	(setq replacement (format "%06o" value))
	(replace-match replacement)))))

(defun compare-results ()
  "Between the buffers »expected« and »actual« compare the results on the current ilne."
  (interactive)
  (let ((fname "compare-results")
	(expected-buf (get-buffer-create "expected"))
	(expected-value)
	(actual-buf (get-buffer-create "actual"))
	(actual-value)
	(line-no (1+ (count-lines (point-min) (point)))))
    (cond ((eq (current-buffer) expected-buf)
	   (setq expected-value (buffer-substring (line-beginning-position) (line-end-position)))
	   (set-buffer actual-buf)
	   (goto-line line-no)
	   (setq actual-value (buffer-substring (line-beginning-position) (line-end-position))))
	  ((eq (current-buffer) actual-buf)
	   (setq actual-value (buffer-substring (line-beginning-position) (line-end-position)))
	   (with-current-buffer expected-buf
	     (goto-line line-no)
	     (setq expected-value (buffer-substring (line-beginning-position) (line-end-position)))))
	  (t
	   (error "%s(): You're not in either results buffer." fname)))
    (while (string-match "\\\\\\\\" expected-value)
      (setq expected-value (concat (substring expected-value 0 (match-beginning 0))
				   "\\"
				   (substring expected-value (match-end 0)))))
    ;; N.B. You're now in actual-buf.
    (cond ((string-match expected-value actual-value)
	   (message "Life is good.")
	   t)
	  (t
	   ;; Really let the user know about this failure.
	   (error "Mismatch at line %d." line-no)
	  ;; This is a failure. Return it.
	   nil))
    (sit-for 1.0)))


;;
;; Hacks
;; 
(defun create-general-cpio-mode-function ()
  "Create a general cpio-mode function set to the next cpio-newc function.
Well, that's the intent, but, really, it's a hack."
  (interactive)
  (let ((fname "create-general-cpio-mode-function")
	(cpio-function-name)
	(cpio-function-definition)
	(start -1)
	(end -1)
	(defun-end -1))
    (cond ((re-search-forward " \\(cpio-newc\\(-[-[:alnum:]]+\\)\\)" (point-max))
	   (setq cpio-function-name (match-string-no-properties 1))
	   (setq cpio-function-definition 
		 (format "(setq cpio%s-function %s)\n" (match-string-no-properties 2)
			 cpio-function-name))
	   (end-of-defun)
	   (insert cpio-function-definition))
	  (t nil))))

(defun bbb-newc (header-string)
  "Return a crudely parsed newc header from the given HEADER-STRING."
  (let* ((fname "bbb-newc")
	 (lengths (list 6 8 8 8 8  8 8 8 8 8  8 8 8 8 8))
	 (stops (let ((i 0)
		      (j 0)
		      (n 0))
		  (mapcar (lambda (l)
			    (prog1
				n
			      (setq n (+ n (nth i lengths)))
			      (setq i (1+ i))))
			  lengths)))
	 (i 0)
	 (j 1))
    (setq header-string (cg-strip-right "\0" header-string t))
    (mapcar (lambda (s)
	      (prog1 (substring header-string (nth i stops) (nth j stops))
		(setq i j)
		(setq j (1+ j))))
	    stops)))

(defun fix-next-writable ()
  "Prepare the next (with-writable-buffer) form."
  (interactive)
  (let ((fname "aaa")
	(start)
	(end))
    ;; (next-error 2)
    (cond ((search-forward "buffer-read-only nil" (point-max) t)
	   (beginning-of-line)
	   (setq start (point))
	   (setq end (save-excursion
		       (end-of-defun)
		       (point)))
	   
	   (delete-region (line-beginning-position) (line-end-position))
	   (insert "(with-writable-buffer")
	   (indent-for-tab-command)
	   (search-forward "buffer-read-only t" end t)
	   (end-of-line)
	   (skip-chars-backward ")")
	   (delete-region (line-beginning-position) (point))
	   (delete-indentation)
	   (goto-char start)
	   (indent-pp-sexp))
	  (t nil))))

(defun update-checks ()
  "(should (progn (message \"...\"))) --> (should (progn (message \"%s: ...\" test-name)))"
  (interactive)
  (let ((fname "update-checks")
	(fill-pos (make-marker)))
    (while (re-search-forward "^\\s-+(should\\s-+(progn (message \"" (point-max) t)
      (set-marker fill-pos (match-end 0))
      (cond ((search-forward "\")" (line-end-position) t)
	     (goto-char (1- (match-end 0)))
	     (insert " test-name")
	     (goto-char fill-pos)
	     (insert "%s: "))
	    (t
	     (warn "%s(): Line %d ends oddly." fname (count-lines (point-min)
								  (point))))))))
(defun grab-temps ()
  "Grab all the temporary buffer/file uses and
put them in a buffer called temp.el.

CAUTION: This is a HACK. It was modified 
to grab temp- references in sub-directories."
  (interactive)
  (let ((fname "grab-temps")
	(temp-uses-buffer (find-file-noselect "temp.el"))
	(dir)
	(func-text)
	(grep-parts)
	(file)
	(file-buf)
	(line))
    ;; (with-current-buffer temp-uses-buffer (erase-buffer))
    (mapc (lambda (d)
	    (setq dir (concat "~/src/3rdParty/GNU/emacs/emacs-24.5/lisp/" d "/"))
	    
	    (lgrep "with-temp" "*.el" dir)
	    (with-current-buffer "*grep*"
	      (while (re-search-forward "^[^:]+:" (point-max) t)

		;; (next-error)

		(setq grep-parts (split-string (buffer-substring-no-properties
						(line-beginning-position)
						(line-end-position))
					       ":")))
	      (setq file (concat dir
				 (nth 0 grep-parts)))
	      (setq line (string-to-number (nth 1 grep-parts)))
	      (setq file-buf (find-file file))
	      (with-current-buffer file-buf
		(goto-line line)
		(mark-defun)
		(setq func-text (buffer-substring (point) (mark))))
	      (with-current-buffer temp-uses-buffer
		(goto-char (point-max))
		(insert func-text))))
	  (list
	   "calc"
	   "celendar"
	   "cedet"
	   "emacs-lisp"
	   "emacs-parallel"
	   "emulation"
	   "erc"
	   "eshell"
	   "gnus"
	   "international"
	   "language"
	   "leim"
	   "mail"
	   "mh-e"
	   "net"
	   "nxml"
	   ;; "obsolete"
	   "org"
	   "play"
	   "progmodes"
	   "term"
	   "textmodes"
	   "url"
	   "vc"))))

;;
;; Hacks
;;
(defvar cdmt-generic-vars (list
			   "cdmt-small-archive"
			   "cdmt-large-archive"
			   "cdmt-header-re"

			   "cpio-magic-re-idx"
			   "cpio-mode-re-idx"
			   "cpio-uid-re-idx"
			   "cpio-gid-re-idx"
			   "cpio-nlink-re-idx"
			   "cpio-filesize-re-idx"
			   "cpio-namesize-re-idx"
			   "cpio-chksum-re-idx"
			   "cpio-filename-re-idx"
			   "cdmt-archive-format"))

(defun cdmt-convert-vars (format)
  "Convert all the uses of FORMAT specific vars to their generic counterparts.
FORMAT is a string.

NOTE: Only use this after the definitions of the generic variables."

  (interactive "sFormat? ")
  (let* ((fname "cdmt-convert-vars")
	 (formats (list
		   "bin"
		   "crc"
		   "newc"
		   "odc"))
	 (beginning)
	 (end)
	 (format-specific-name))
    (unless (string-match (regexp-opt formats) format)
      (error "%s(): Uknown format [[%s]]." fname format))

    (widen)
    (mapc (lambda (v-name)
	    (string-match "-" v-name)
	    (setq beginning (substring v-name 0 (match-end 0)))
	    (setq end (substring v-name 0 (match-end 0)))
	    (setq format-specific-name (concat beginning format "-" end))

	    ;; Get past the vars under change.
	    (goto-char (point-min))
	    (search-forward "Vars" (point-max) nil) ;Error out if the buffer doesn't have a Vars page.
	    (re-search-forward page-delimiter (point-max) nil) ;Or if it's not actually a page.
	    ;; Now replace every thing else.
	    (while (search-forward format-specific-name (point-max) t)
	      (replace-match v-name nil nil format-specific-name)))
	  cdmt-generic-vars)))

(defvar cdmt-generic-funcs (list
			    "cdmt-filter-archive-contents"
			    "cdmt-reset"
			    "cdmt-reformat-headers"
			    "cdmt-unfinished-command"
			    "cdmt-all-unfinished-commands"
			    "cdmt-finished-command"
			    "cdmt-all-finished-commands"
			    "cdmt-tidy-up-catalog"
			    "cdmt-test-save"
			    "cdmt-global-sub"
			    "cdmt-sweep-hex"))
(setq cdmt-generic-funcs (list
			    "cdmt-filter-archive-contents"
			    "cdmt-reset"
			    "cdmt-reformat-headers"
			    "cdmt-unfinished-command"
			    "cdmt-all-unfinished-commands"
			    "cdmt-finished-command"
			    "cdmt-all-finished-commands"
			    "cdmt-tidy-up-catalog"
			    "cdmt-test-save"
			    "cdmt-global-sub"
			    "cdmt-sweep-hex"
			    "cdmt-filter-archive-contents"))

(defun convert-to-generic-funcs (format)
  ""
  (interactive "sFormat? ")
  (let ((fname "convert-to-generic-funcs")
	 (formats (list
		   "bin"
		   "crc"
		   "newc"
		   "odc"))
	 (beginning)
	 (end)
	 (format-specific-name))
    (unless (string-match (regexp-opt formats) format)
      (error "%s(): Uknown format [[%s]]." fname format))

    (widen)
    (mapc (lambda (f-name)
	    (string-match "-" f-name)
	    (setq beginning (substring f-name 0 (match-end 0)))
	    (setq end (substring f-name (match-end 0)))
	    (setq format-specific-name (concat beginning format "-" end))
	    
	    ;; Get past the vars under change.
	    (goto-char (point-min))
	    (search-forward "Vars" (point-max) nil) ;Error out if the buffer doesn't have a Vars page.
	    (re-search-forward page-delimiter (point-max) nil) ;Or if it's not actually a page.
	    ;; Now replace every thing else.
	    (while (search-forward format-specific-name (point-max) t)
	      (replace-match f-name)))
	  cdmt-generic-funcs)))

(defun cdmt-ediff-results ()
  "Compare the results of a (string-match) on the current line in a buffer of ERT results.
The results are always presented as (string-match EXPECTED-RE ACTUAL) on that line."
  (interactive)
  (let ((fname "cdmt-ediff-results")
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

(defun aaa ()
  "Narrow to the next 'Killing buffer' block,
including any messages after the previous one."
  (interactive)
  (let ((fname "aaa")
	(start -1)
	(end -1))
    (widen)
    (end-of-line)
    (cond ((re-search-forward "^\\(cdmt-\\|killing buffer \\)" (point-max) t)
	   (setq start (match-beginning 0))
	   (if (or (looking-at-p "killing buffer")
		   (re-search-forward "^killing buffer " (point-max) t))
	       (end-of-line))
	   (if (re-search-forward "^[[:graph:]]" (point-max) t)
	       (setq end (match-beginning 0))
	     (setq end (point-max)))
	   (narrow-to-region start end)
	   (goto-char end))
	  (t				;We're done.
	   nil))))

(defun cdmt-message (fmt &rest fills)
  "Write a message to the *cab-info-buffer*."
  (let ((fname "cdmt-message"))
    (if noninteractive
	(with-current-buffer *cab-info-buffer*
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert (apply 'format fmt fills))
	  (unless (bolp) (insert "\n"))))))

;; (defun cdmt-message (fmt &rest fills)
;;   "A NOOP to replace the functional (cdmt-message)."
;;   (let ((fname "cdmt-message"))))

(defun cdmt-ensure-test-names ()
  "Make sure every message after the point includes the test-name."
  (interactive)
  (let ((fname "aaa")
	(message-func-name "cdmt-message")
	(msg-end (make-marker)))
    (while (search-forward (concat "(" message-func-name " ") (point-max) t)
      (set-marker msg-end (line-end-position))
      (skip-chars-forward "\" ")
      (unless (looking-at-p "%s")
	(insert "%s(): ")
	(re-search-forward "\"[ )]" msg-end t)
	(backward-char 1)
	(insert " test-name")
	(end-of-line)))))

(defun mmm (test-name line-no)
  "A little debugger to see if cpio-dired-test.el is still there."
  (let ((fname "mmm"))
    (catch 'avoid
      (throw 'avoid t)
      (cdmt-message "%s(): (%d)" test-name line-no) ;; (sit-for .1)
      (mapc (lambda (b)
	      (if (member b (buffer-list))
		  (cdmt-message "... and %s is still there. (%d)." b line-no)
		(cdmt-message "... BUT %s is GONE. (%d)." b line-no)
		;; (sit-for 10.0)))
		))
	    buffers))))

(defun move-mesages-outside ()
  "Move (progn (message)s) outside of (should) (from this point on)."
  (interactive)
  (let ((fname "move-mesages-outside")
	(start (make-marker))
	(inner-end (make-marker))
	(end (make-marker))
	(limit (make-marker))
	(last-parse-point (set-marker (make-marker) (point-min)))
	(parse-data ())
	(messages ""))
    ;; Make sure the START marker doesn't move forward.
    (set-marker-insertion-type start nil)
    (save-match-data
      ;; Look for the start of a sequence of messages inside a (should).
      (while (re-search-forward "(should \\((progn (message\\)" (point-max) t)
	;; Keep track of parsing.
	(setq parse-data (parse-partial-sexp last-parse-point (point)))
	(setq last-parse-point (match-beginning 0))
	;; Keep track of where things matched.
	(set-marker start (match-beginning 1))
	;; Ignore things if you're inside a comment. (No sexps in comments.)
	(unless (nth 4 parse-data)
	  ;; Keep track of where the sexp for this match ends.
	  (set-marker limit (save-excursion
			      (goto-char (match-beginning 0))
			      (forward-sexp)
			      (point)))
	  ;; Skip past any (message)s before LIMIT.
	  (goto-char start)
	  (while (re-search-forward "(message.+$" limit t)
	    (set-marker end (1+ (match-end 0))))
	  (set-marker end (point))
	  ;; Save and delete those (message)s.
	  (setq messages (buffer-substring start end))
	  (delete-region start end)
	  ;; Put them before the (should).
	  ;; (This is where we care about the insertion type.)
	  (goto-char start)
	  (beginning-of-line)
	  (open-line 1)
	  (insert messages)
	  ;; Indent to make emacs parsing happy.
	  (indent-for-tab-command)
	  (set-marker inner-end (point)) ;Keep track of the end of (message)s.
	  (beginning-of-line)
	  ;; Get rid of the initial '(progn '.
	  (cond ((looking-at "\\s-*(progn ")
		 (delete-region (point) (match-end 0))
		 (indent-for-tab-command))
		(t
		 (error "%s(): Bad (messages) block at %d.\n"
			fname (count-lines (point-min) (point)))))

	  ;; skip over the (should) and...
	  (goto-char inner-end)
	  (forward-sexp)
	  ;; ...delete the now extra closing paren.
	  (if (looking-at ")")
	      (delete-char 1)
	    (error "%s(): No closing paren at %d?"
		   fname (count-lines (point-min) (point))))))
      ;; And do it all again.
      )))

(defun cdmt-noop (c)
  "Accept the character C and do nothing."
  (let ((fname "cdmt-noop"))))


;;
;; Functions for managing standard debuggers
;; 
(defvar *cg-debugger-re* "^\\s-*(message \"%s(): \\([[:digit:]]+\\)\" \\(f\\|test-\\)name)$"
  "RE to match a debugger created by M-x insert-debugger.")
(setq *cg-debugger-re* "^\\s-*(message \"%s(): \\([[:digit:]]+\\)\" \\(f\\|test-\\)name)$")

(defun insert-debugger ()
  "Insert a new debugger statement above the line containing point."
  (interactive)
  (let ((fname "insert-debugger")
	(var-name))

    (if (string-match "-test.el" (buffer-file-name))
	(setq var-name "test-name")
      (setq var-name fname))

    (beginning-of-line)
    (open-line 1)
    (insert (format "(message \"%%s(): %d\" %s)" (count-lines (point-min) (point)) var-name))
    (indent-according-to-mode)))
(local-set-key "\M-\C-i" 'insert-debugger)

(defun update-debuggers ()
  "Update the line numbers in all the debuggers created by M-x insert-debugger."
  (interactive)
  (let ((fname "update-debuggers"))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward *cg-debugger-re* (point-max) t)
	  (replace-match (format "%d" (count-lines (point-min) (point)))
			 nil nil nil 1))))
    (save-buffer)))
(local-set-key "\M-\C-u" 'update-debuggers)

(defun remove-debugger ()
  "Remove the next debugger.
Return T if one was found
and NIL otherwise.
This function respects narrowing."
  (interactive)
  (let ((fname "remove-debugger"))
    (cond ((re-search-forward *cg-debugger-re* (point-max) t)
	   (delete-region (match-beginning 0) (match-end 0))
	   t)
	  (t nil))))

(defun remove-some-debuggers (arg)
  "Remove the next ARG debuggers.
Return non-NIL if any were found and deleted.
Return NIL if none were found.
This function respects narrowing."
  (interactive "p")
  (let ((fname "remove-some-debuggers")
	(ct 0))
    (while (and (< 0 arg) (remove-debugger))
      (setq ct (1+ ct))
      (setq arg (1- arg)))
    ct))

(defun remove-all-debuggers ()
  "Remove all debuggers created by (insert-debuggers).
This function respects narrowing."
  (interactive)
  (let ((fname "remove-all-debuggers"))
    (while (remove-debugger))))

(provide 'test-generic)
;;; test-generic.el ends here


