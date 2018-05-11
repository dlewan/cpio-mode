;; -*- coding: utf-8 -*-
;;; cpio-entry-contents-mode.el --- minor mode for editing a cpio-entry's contents.
;	$Id: cpio-entry-contents-mode.el,v 1.1.4.3 2018/04/26 14:15:31 doug Exp $	
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
;; Created: 2017 Dec 06
;; Version: 0.02
;; Keywords: cpio, contents, edit

;;; Commentary:

;; This file contains code for editing and saving
;; the contents of entris in a cpio-archive.

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
;; Commands
;; 
(defun cpio-entry-contents-save (&optional arg)
  "Save the contents of the current buffer in it's cpio archive."
  (interactive "p")
  (let ((fname "cpio-entry-contents-save")
	(name cpio-entry-name)
	(entry (cpio-entry cpio-entry-name))
	(attrs (cpio-entry-attrs cpio-entry-name))
	(header-string)
	(size (buffer-size))
	(new-contents (buffer-string)))
    (with-current-buffer *cab-parent*
      ;; 0. Make sure the archive is writeable.
      (setq buffer-read-only nil)
      ;; 1. Delete the entry's head and contents (plus padding) in the parent buffer.
      (cpio-delete-archive-entry entry)
      ;; 2. Update the entry size in the entry.
      (cpio-set-entry-size attrs size)
      ;; 3. Build the entry header.
      (setq header-string (cpio-make-header-string attrs))
      ;; 4. Write the header in the archive buffer (plus padding).
      (goto-char (cpio-entry-header-start entry))
      (insert header-string)
      (aset entry *cpio-catalog-entry-contents-start-idx* (point-marker))
      ;; 5. Write the new contents in the archive buffer (plus padding).
      (cpio-insert-padded-contents new-contents)
      ;; 6. Adjust the trailer.
      (cpio-adjust-trailer)
      ;; 7. Save the archive buffer.
      (basic-save-buffer)
      ;; 8. Mark the buffer saved.
      (setq buffer-read-only t))
    ;; 9. Mark the contents buffer as unmodified.
    (set-buffer-modified-p nil)
    ;; 10. Update the dired-like interface.
    (with-current-buffer *cab-parent*
	(cpio-present-ala-dired (current-buffer)))))

(defun cpio-entry-contents-kill (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME.
A name denotes the name of an entry in the cpio archive."
  (interactive "P")
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "cpio-entry-contents-kill")
	(buffer (if (bufferp buffer-or-name)
		    buffer-or-name
		  (get-buffer-create buffer-or-name))))
    (if (and (buffer-modified-p buffer)
	     (yes-or-no-p "Buffer is modified. Really kill? "))
	(kill-buffer buffer))))


;; 
;; Mode definition (IF APPROPRIATE)
;; 
(defvar *cpio-entry-contents-mode-map* (make-sparse-keymap)
  "Keymap for cpio-entry-contents-mode.")

(defun cpio-entry-contents-make-keymap ()
  "Define the keys that cpio-entry-contents-mode must override."
  (let ((fname "cpio-entry-contents-make-keymap"))
    (define-key *cpio-entry-contents-mode-map* "\C-x\C-s" 'cpio-entry-contents-save)
    (define-key *cpio-entry-contents-mode-map* "\C-x\C-k" 'cpio-entry-contents-kill)
    (define-key *cpio-entry-contents-mode-map* "\M-,"     'cpio-tags-loop-continue)))

(define-minor-mode cpio-entry-contents-mode
  "Minor mode for working with an entry's contents from a cpio archive.
This mode is automatically invoked when the contents of a cpio entry are
prepared for editing."
  nil
  " entry contents"
  :keymap *cpio-entry-contents-mode-map* :global nil)

(cpio-entry-contents-make-keymap)

;; (defvar cpio-entry-contents-mode nil
;;   "Variable to turn cpio-entry-contents-mode on or off.")
;; (make-variable-buffer-local 'cpio-entry-contents-mode)
;; 
;; (defun cpio-entry-contents-mode (&optional prefix)
;;   "Set up the current buffer as a buffer subordinate to a buffer holding a cpio archive.
;; When called without an argument turn cpio-entry-contents-mode on."
;;   (interactive (list (or current-prefix-arg 'toggle)))
;;   (let ((fname "cpio-entry-contents-mode")
;; 	(enable (if (eq prefix 'toggle)
;; 		    (not cpio-entry-contents-mode)
;; 		  (> (prefix-numeric-value prefix) 0))))
;;     (cpio-entry-contents-make-keymap)
;;     (if enable
;; 	(setq cpio-entry-contents-mode t)
;;       (setq cpio-entry-contents-mode nil))))


(provide 'cpio-entry-contents-mode)
;;; cpio-entry-contents-mode.el ends here

