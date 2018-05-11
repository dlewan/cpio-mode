;; -*- coding: utf-8 -*-
;;; cpio-affiliated-buffers.el --- Establish and manage buffers affiliated with each other.
;	$Id: cpio-affiliated-buffers.el,v 1.1.2.13 2018/04/26 14:15:30 doug Exp $	

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
;; Created: 2017 Nov 22
;; Version: 
;; Keywords: 

;;; Commentary:

;; To keep track of which buffers are connected to a specific archive,
;; cpio-mode uses the idea of affiliated buffers.
;; 
;; The buffers affiliated with an archive's buffer are the following:
;; 1. The buffer holding the dired-like information.
;; 2. The buffers holding any entry's contents.
;; Killing the dired-like buffer also kills the archive's buffer,
;; and killing the archive's buffer kills
;; all remaining affiliated buffers.
;; 
;; HEREHERE (Well, that's the intent. It doesn't seem to work correctly yet.)

;;; Documentation:

;; The following incantation should run the tests well.
;; emacs -quick --fullheight --debug-init --load cab-test.el --eval '(ert t)'&

;;; Code:

;;
;; Generic functions
;; 




;;
;; Dependencies
;; 


;; 
;; Vars
;; 
(defvar *cab-subordinates* ()
  "A list of subordinate buffers affiliated with the current buffer.")
(make-variable-buffer-local '*cab-subordinates*)
(defvar *cab-parent* nil
  "The parent buffer of an affiliated buffer.")
(make-variable-buffer-local '*cab-parent*)

;; 
;; Library
;; 
(defun cab-register (buffer parent)
  "Register the given BUFFER as an affiliate of the PARENT buffer."
  (let ((fname "cab-register"))
    (if (not (bufferp buffer))
	(error "%s(): proposed buffer [[%s]] is not a buffer." fname buffer))
    (if (not (bufferp parent))
	(error "%s(): proposed parent buffer [[%s]] is not a buffer." fname parent))
    (if (equal buffer parent)
	(error "%s(): You can't affiliate a buffer [[%s]] with itself [[%s]]." fname buffer parent))

    (unless (cab-registered-p buffer parent)
      (with-current-buffer parent
	(push buffer *cab-subordinates*))
      (add-hook 'kill-buffer-hook 'cab-kill-buffer-hook))

    (with-current-buffer buffer
      (cond ((null *cab-parent*)
	     (setq *cab-parent* parent)

	     (make-variable-buffer-local 'kill-buffer-hook)
	     (add-hook 'kill-buffer-hook 'cab-kill-buffer-hook))
	    (t t)))

    (local-set-key "\C-x\C-k" 'cab-deregister)))

(defun cab-registered-p (buffer parent)
  "Return non-NIL if BUFFER is already registered to PARENT.
CONTRACT: BUFFER and PARENT are buffers."
  (let ((fname "cab-registered-p"))
    (with-current-buffer parent
      (not (member buffer *cab-subordinates*)))))

(defun cab-kill-buffer-hook ()
  "Deregister the current buffer when it is killed."
  (let ((fname "cab-kill-buffer-hook"))
    (cond ((buffer-live-p (current-buffer))
	   (message "    Deregistering subordinates: [[%s]]." *cab-subordinates*)
	   (mapc 'cab-deregister *cab-subordinates*))
	  (t nil))))

(defun OBS-cab-deregister (buffer)
  "Deregister the given BUFFER as an affiliate of its parent buffer."
  (let ((fname "cab-deregister")
	(local-subordinates ())
	(parent))
    (if (buffer-live-p buffer)
	(with-current-buffer buffer

	  (setq parent *cab-parent*)
	  (setq *cab-parent* nil)
	  (mapc (lambda (b)
		  (kill-buffer b))
    		*cab-subordinates*)))
    (if (buffer-live-p parent)
	(with-current-buffer parent
	  (mapc (lambda (b)
		  (if (and (equal b buffer)
			   (buffer-live-p b))
		      (remove-hook 'kill-buffer-hook 'cab-deregister 'local)
		    (if (buffer-live-p b)
			(push b local-subordinates))))
		  *cab-subordinates*)
	  (setq *cab-subordinates* local-subordinates)))))

(defun cab-deregister (&optional buffer)
  "Deregister and kill BUFFER and all its subordinate buffers.
Remove its entry in its parent buffer."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (let ((fname "cab-deregister")
	(parent)
	(subordinates))
    (cond ((buffer-live-p buffer)
	   (with-current-buffer buffer 
	     (setq parent *cab-parent*)
	     (setq subordinates *cab-subordinates*))
	   (mapc 'cab-deregister subordinates)
	   (message "    About to use parent: [[%s]]." parent)
	   (if (and parent (bufferp parent) (buffer-live-p parent))
	       (with-current-buffer parent
		 (setq *cab-subordinates* (delete buffer *cab-subordinates*))))
	   (remove-hook 'kill-buffer-hook 'cab-simple-deregister)
	   (kill-buffer buffer))
	  (t t))))

(defun cab-simple-deregister (buffer)
  "Deregister BUFFER and all its subordinates, but don't kill it."
  (let ((fname "cab-simple-deregister")
	(parent)
	(subordinates))
    (with-current-buffer buffer 
      (setq parent *cab-parent*)
      (setq subordinates *cab-subordinates*))
    (mapc 'cab-simple-deregister subordinates)
    (with-current-buffer parent
      (setq *cab-subordinates* (delete buffer *cab-subordinates*)))))

(defun cab-clean ()
  "Clean up affiliated buffers.
CAVEAT: This function should disappear as affiliated buffer code stabilizes."
  (interactive)
  (let ((fname "cab-clean"))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (if (boundp '*cab-subordinates*)
		  (setq *cab-subordinates* (delete-duplicates *cab-subordinates*)))))
	  (buffer-list))))


;; 
;; Commands
;; 


;; 
;; Mode definition (IF APPROPRIATE)
;; 



(provide 'cpio-affiliated-buffers)
;;; cpio-affiliated-buffers.el ends here

