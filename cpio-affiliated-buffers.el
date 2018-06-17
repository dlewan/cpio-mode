;; -*- coding: utf-8 -*-
;;; cpio-affiliated-buffers.el --- Establish and manage buffers affiliated with each other.
;	$Id: cpio-affiliated-buffers.el,v 1.6 2018/06/16 14:50:07 doug Exp $	

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
;; Killing [deregistering] the dired-like buffer also kills the archive's buffer,
;; and killing the archive's buffer kills
;; all remaining affiliated buffers.
;; 
;; HEREHERE (Well, that's the intent. It doesn't seem to work correctly yet.)

;;; Documentation:

;; Two variables hold the relationships among buffers:
;; • *cab-subordinates* -- a list of the buffers immediately subordinate
;;   to the current buffer.
;; • *cab-parent* -- a buffer, the buffer to which the current buffer is affiliated.
;; Both variables are buffer local.
;; 
;; The existence of a subordinate buffer depends
;; on the the existence of its parent.
;; One consequence is that a subordinate buffer can have only one parent.
;; Another is that killing the parent buffer kills all subordinates as well.
;; Should a subordinate buffer have further subordinates,
;; then they must also be killed.

;; API:
;; (cab-register (buffer parent))
;;     Register BUFFER as a subordinate of PARENT.
;; (cab-registered-p (buffer parent)
;;     Return non-NIL if BUFFER is a registered subordinate of PARENT.
;; (cab-kill-buffer-hook)
;;     A hook for subordinate buffers that removes their registry entry 
;;     with PARENT.
;; (cab-deregister (&optional buffer))
;;     Kill BUFFER and its subordinates.
;;     Deregister BUFFER with its parent.
;;     HEREHERE This doesn't seem to do what it's supposed to.
;; (cab-simple-deregister (buffer))
;;     The internal function for (cab-deregister).
;;     Don't use this directly.
;;     HEREHERE Again, this doesn't look like it does the right thing.
;; (cab-clean)
;;     A temporary function for development
;;     that should more forcefully enforce the intent of (cab-deregister).


;; The following incantation should run the tests well.
;; emacs -quick --fullheight --debug-init --load cab-test.el --eval '(ert t)'&

;;; Code:

;;
;; Development
;;
(defun cab-setup-parenthood-check ()
  "Set up a simple situation where the parenthood check should error out."
  (let ((b0 (find-file-noselect "b0"))
	(b1 (find-file-noselect "b1")))
    (cab-register b1 b0)
    (cab-register b0 b1)))
(defun cab-setup-parenthood-check-1 ()
  "Set up a large situation where the parenthood check should error out."
  (let* ((b0 (find-file-noselect "bb0"))
	 (b1 (find-file-noselect "bb1"))
	 (b2 (find-file-noselect "bb2"))
	 (b3 (find-file-noselect "bb3"))
	 (b4 (find-file-noselect "bb4"))
	 (b5 (find-file-noselect "bb5"))
	 (b6 (find-file-noselect "bb6"))
	 (b7 (find-file-noselect "bb7"))
	 (b8 (find-file-noselect "bb8"))
	 (b9 (find-file-noselect "bb9"))
	 (parent b0)
	 )
    (mapc (lambda (b)
	    (cab-register b parent)
	    (setq parent b))
	  (list b1 b2 b3 b4 b5 b6 b7 b8 b9))
    (cab-register b0 b9)))


;;
;; Generic functions
;; 




;;
;; Dependencies
;; 
(require 'cl)


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
  "Register the given BUFFER as an affiliate of the PARENT buffer.
If BUFFER is already an affiliate of PARENT, then succeed quietly.
Return non-NIL on success.
Return NIL if buffer is already affiliated to another parent."
  (let ((fname "cab-register"))
    (if (not (bufferp buffer))
	(error "%s(): proposed buffer [[%s]] is not a buffer." fname buffer))
    (if (not (bufferp parent))
	(error "%s(): proposed parent buffer [[%s]] is not a buffer." fname parent))
    (if (equal buffer parent)
	(error "%s(): You can't affiliate a buffer [[%s]] with itself [[%s]]." fname buffer parent))

    (if (cab-detect-parenthood-cycle buffer parent)
	(error "%s(): Registering [[%s]] as a subordinate of [[%s]] would create a cycle of parents." fname buffer parent))

    (cond ((cab-registered-p buffer parent)
	   t)
	  ((with-current-buffer buffer *cab-parent*)
	   nil)
	  (t
	   (unless (cab-registered-p buffer parent)
	     (with-current-buffer parent
	       (push buffer *cab-subordinates*)
	       (make-local-variable 'kill-buffer-hook)
	       (add-hook 'kill-buffer-hook 'cab-kill-buffer-hook)
	       
	       ;; kill-buffer-hook
	       ;; (with-current-buffer "cpio.el" kill-buffer-hook)
	       
	       (local-set-key "\C-x\C-k" 'cab-deregister))
	     
	     (with-current-buffer buffer
	       (cond ((null *cab-parent*)
		      (setq *cab-parent* parent)
		      (make-local-variable 'kill-buffer-hook)
		      (add-hook 'kill-buffer-hook 'cab-kill-buffer-hook)
		      
		      ;; kill-buffer-hook
		      ;; (with-current-buffer "cpio.el" kill-buffer-hook)
		      
		      (local-set-key "\C-x\C-k" 'cab-deregister))
		     (t t))))))))
  
(defun cab-detect-parenthood-cycle (buffer parent)
  "Return non-NIL if affiliating BUFFER with PARENT would create a parenthood cycle."
  (let ((fname "cab-detect-parenthood-cycle")
	)
    ;; (error "%s() is not yet implemented" fname)
    (with-current-buffer parent
      (catch 'detected
	(while parent
	  (with-current-buffer parent
	    (cond ((eq (current-buffer) buffer)
		   (throw 'detected t))
		  ((null *cab-parent*)
		   (setq parent *cab-parent*))
		  (t
		   (setq parent *cab-parent*)))))))
    ))

(defun cab-registered-p (buffer parent)
  "Return non-NIL if BUFFER is already registered to PARENT.
CONTRACT: BUFFER and PARENT are buffers."
  (let ((fname "cab-registered-p"))
    (cond ((or (null buffer)
	       (not (bufferp buffer))
	       (not (buffer-live-p buffer)))
	   nil)
	  ((or (null parent)
	       (not (bufferp parent))
	       (not (buffer-live-p parent)))
	   nil)
	  ((and (bufferp parent)
		(buffer-live-p parent))
	   (with-current-buffer parent
	     (member buffer *cab-subordinates*))))))

(defun cab-kill-buffer-hook ()
  "Deregister the current buffer when it is killed."
  (let ((fname "cab-kill-buffer-hook"))
    (cond ((buffer-live-p (current-buffer))
	   ;; (message "    Deregistering subordinates: [[%s]]." *cab-subordinates*)
	   (mapc 'cab-deregister *cab-subordinates*))
	  (t nil))))

(defun cab-deregister (&optional buffer)
  "Deregister and kill BUFFER and all its subordinate buffers.
Note that that will include their subordinates too.
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
	   (cond ((and parent
		       (bufferp parent)
		       (buffer-live-p parent)
		       (cab-registered-p buffer parent))
		  (mapc 'cab-deregister subordinates)
		  ;; (message "    About to use parent: [[%s]]." parent)
		  (with-current-buffer parent
		    (setq *cab-subordinates* (delete buffer *cab-subordinates*)))
		  (mapc 'cab-deregister *cab-subordinates*)
		  (cond ((buffer-live-p buffer)
			 (with-current-buffer buffer
			   (remove-hook 'kill-buffer-hook 'cab-simple-deregister))
			 (kill-buffer buffer))
			(t t)))
		 (t nil)))
	  (t nil))))

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


(provide 'cpio-affiliated-buffers)
;;; cpio-affiliated-buffers.el ends here

