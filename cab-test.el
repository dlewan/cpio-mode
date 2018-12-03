;;; cab-test.el --- automated tests for cpio-affiliated-buffers. -*- coding: utf-8 -*-

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

;; Author: Douglas Lewan <(d.lewan2000@gmail.com>
;; Maintainer: -- " --
;; Created: 2017 Nov 22
;; Version: 0.13β
;; Keywords: files

;;; Commentary:

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 
(eval-when-compile
  (require 'ert))
(eval-when-compile
  (if (file-exists-p (concat default-directory "cpio-affiliated-buffers.elc"))
      (load (concat default-directory "cpio-affiliated-buffers.elc"))
    (load (concat default-directory "cpio-affiliated-buffers.el"))))
(if (file-exists-p (concat default-directory "test-generic.elc"))
    (load (concat default-directory "test-generic.elc"))
  (load (concat default-directory "test-generic.el")))

(local-set-key "\M-\C-i" 'insert-debugger)
(local-set-key "\M-\C-u" 'update-debuggers)

;;;;;;;;;;;;;;;;
;; Make the byte compiler happy.
(defvar *cab-parent)
(defvar *cab-subordinates*)
(declare-function ert-set-test "/usr/share/emacs/24.5/lisp/emacs-lisp/ert.el")
(declare-function ert--signal-should-execution "/usr/share/emacs/24.5/lisp/emacs-lisp/ert.el")
(declare-function ert-fail "/usr/share/emacs/24.5/lisp/emacs-lisp/ert.el")
(declare-function ert "/usr/share/emacs/24.5/lisp/emacs-lisp/ert.el")
(declare-function cdmt-message "test-generic.el")
;; EO making the byte compiler happy.
;;;;;;;;;;;;;;;;

;; 
;; Vars
;; 


;; 
;; Library
;; 
(ert-deftest cab-test-registry-1 ()
  "Tests a simple (cab-register child parent)."
  (let ((fname "cab-test-registry-1")
	(parent (get-buffer-create "r-1-p"))
	(child  (get-buffer-create "r-1-c")))
    ;; (if (buffer-live-p parent)
    ;;     (message "    Parent buffer is live.")
    ;;   (message "    Parent buffer is NOT live."))
    ;; (if (buffer-live-p child)
    ;;     (message "    Child buffer is live.")
    ;;   (message "    Child buffer is NOT live."))
    
    (cab-register child parent)
    ;; (message "parent of child is [[%s]]." (with-current-buffer child *cab-parent*))
    ;; (message "subordinates of child are [[%s]]." (with-current-buffer child *cab-subordinates*))
    ;; (message "parent of parent is [[%s]]." (with-current-buffer parent *cab-parent*))
    ;; (message "subordinates of parent are [[%s]]." (with-current-buffer parent *cab-subordinates*))

    (should (equal (with-current-buffer parent *cab-parent*)
		   nil))
    (should (equal (with-current-buffer parent *cab-subordinates*)
		   (list child)))
    (should (equal (with-current-buffer child *cab-parent*)
		   parent))
    (should (equal (with-current-buffer child *cab-subordinates*)
		   ()))))

(ert-deftest cab-test-registry-2 ()
  "Tests registering two buffers under the same parent."
  (let ((fname "cab-test-registry-2")
	(parent   (get-buffer-create "r-2-p2"))
	(buffer-1 (get-buffer-create "r-2-b21"))
	(buffer-2 (get-buffer-create "r-2-b22")))
    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)

    (should (= (with-current-buffer parent (length *cab-subordinates*))
	       2))
    (should (member buffer-1 (with-current-buffer parent *cab-subordinates*)))
    (should (member buffer-2 (with-current-buffer parent *cab-subordinates*)))
    (should (equal (with-current-buffer buffer-1 *cab-parent*)
		   parent))
    (should (equal (with-current-buffer buffer-2 *cab-parent*)
		   parent))
    (should (equal (with-current-buffer buffer-1 *cab-subordinates*)
		   ()))
    (should (equal (with-current-buffer buffer-2 *cab-subordinates*)
		   ()))))

(ert-deftest cab-test-registry-3 ()
  "Tests registering a 3 level hierarchy."
  (let ((fname "cab-test-registery-3")
	(grandparent    (get-buffer-create "r-3-gp3"))
	(parent-1       (get-buffer-create "r-3-p31"))
	(parent-2       (get-buffer-create "r-3-p32"))
	(grandchild-311 (get-buffer-create "r-3-b311"))
	(grandchild-312 (get-buffer-create "r-3-b312"))
	(grandchild-313 (get-buffer-create "r-3-b313"))
	(grandchild-321 (get-buffer-create "r-3-b321"))
	(grandchild-322 (get-buffer-create "r-3-b322")))
    (cab-register parent-1       grandparent)
    (cab-register parent-2       grandparent)
    (cab-register grandchild-311 parent-1)
    (cab-register grandchild-312 parent-1)
    (cab-register grandchild-313 parent-1)
    (cab-register grandchild-321 parent-2)
    (cab-register grandchild-322 parent-2)

    (should (equal (with-current-buffer grandparent (length *cab-subordinates*))
		   2))
    (should (equal (with-current-buffer parent-1 (length *cab-subordinates*))
		   3))
    (should (equal (with-current-buffer parent-2 (length *cab-subordinates*))
		   2))
    (should (member parent-1 (with-current-buffer grandparent *cab-subordinates*)))
    (should (member parent-2 (with-current-buffer grandparent *cab-subordinates*)))
    (should (member grandchild-311 (with-current-buffer parent-1 *cab-subordinates*)))
    (should (member grandchild-312 (with-current-buffer parent-1 *cab-subordinates*)))
    (should (member grandchild-313 (with-current-buffer parent-1 *cab-subordinates*)))
    (should (member grandchild-321 (with-current-buffer parent-2 *cab-subordinates*)))
    (should (member grandchild-322 (with-current-buffer parent-2 *cab-subordinates*)))))

(ert-deftest cab-test-deregister-1 ()
  "Test deregistering the registry established in (cab-test-registry-1)."
  (let ((fname "cab-test-registry-1")
	(parent (get-buffer-create "dr-1-p1"))
	(buffer (get-buffer-create "dr-1-b2")))
    (cab-register buffer parent)
    (cab-deregister buffer)
    
    (with-current-buffer parent 
      (mapc (lambda (sb)
	      (should-not (buffer-live-p sb)))
	    *cab-subordinates*))
    
    ;; Revive the buffer.
    (setq buffer (get-buffer-create "dr-1-b2"))
    (cab-register buffer parent)
    (cab-deregister parent)
    (should-not (buffer-live-p parent))
    (should-not (cab-registered-p buffer parent))))

(ert-deftest cab-test-deregister-2 ()
  "Test deregistering the registry established in (cab-test-registry-2)."
  (let ((fname "cab-test-registry-2")
	(parent   (get-buffer-create "dr-2-p2"))
	(buffer-1 (get-buffer-create "dr-2-b21"))
	(buffer-2 (get-buffer-create "dr-2-b22")))
    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)
    (cab-deregister buffer-1)

    (should (= (with-current-buffer parent (length *cab-subordinates*))
	       1))

    (cab-deregister buffer-2)

    (should (= (with-current-buffer parent (length *cab-subordinates*))
	       0))

    (setq buffer-1 (get-buffer-create "b21"))
    (setq buffer-2 (get-buffer-create "b22"))
    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)

    (cab-deregister parent)

    (should-not (buffer-live-p parent))

    (should-not (buffer-live-p buffer-1))
    (should-not (cab-registered-p buffer-1 parent))

    (should-not (buffer-live-p buffer-2))
    (should-not (cab-registered-p buffer-2 parent))))

(ert-deftest cab-test-deregister-3 ()
  "Test deregistering the registry established in (cab-test-registry-3)."
  (let ((fname "cab-test-deregister-3")
	(grandparent    (get-buffer-create "dr-3-gp3"))
	(parent-1       (get-buffer-create "dr-3-p31"))
	(parent-2       (get-buffer-create "dr-3-p32"))
	(grandchild-311 (get-buffer-create "dr-3-b311"))
	(grandchild-312 (get-buffer-create "dr-3-b312"))
	(grandchild-313 (get-buffer-create "dr-3-b313"))
	(grandchild-321 (get-buffer-create "dr-3-b321"))
	(grandchild-322 (get-buffer-create "dr-3-b322")))
    (cab-register parent-1       grandparent)
    (cab-register parent-2       grandparent)
    (cab-register grandchild-311 parent-1)
    (cab-register grandchild-312 parent-1)
    (cab-register grandchild-313 parent-1)
    (cab-register grandchild-321 parent-2)
    (cab-register grandchild-322 parent-2)

    (cab-deregister grandchild-311)

    (should-not (buffer-live-p grandchild-311))
    (should-not (cab-registered-p grandchild-311 parent-1))

    (cab-deregister parent-1)

    (should-not (buffer-live-p parent-1))
    (should-not (buffer-live-p grandchild-312))
    (should-not (buffer-live-p grandchild-313))
    (should-not (cab-registered-p grandchild-312 parent-1))
    (should-not (cab-registered-p grandchild-313 parent-1))

    (should (equal (with-current-buffer parent-2 *cab-parent*)
		   grandparent))
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  (list grandchild-321
		grandchild-321))
    (should (equal (with-current-buffer parent-2 (length *cab-subordinates*))
		   2))
    (should (member grandchild-322 (with-current-buffer parent-2 *cab-subordinates*)))

    ;; (should (null (cab-deregister grandparent)))
    (cab-deregister grandparent)

    ;; (should-not (buffer-live-p grandparent)
    ;; (should (null (buffer-live-p parent-1)))
    ;; (should (null (buffer-live-p parent-2)))
    (mapc (lambda (b)
	    (should-not (buffer-live-p b)))
	  (list parent-1
		parent-2
		grandchild-311
		grandchild-312
		grandchild-313
		grandchild-321
		grandchild-322))))

(ert-deftest cab-test-kill-1 ()
  "Tests killing in the hierarchy created in (cab-test-registry-1)."

  (let ((fname "cab-test-kill-1")
	(parent (get-buffer-create "k-1-p1"))
	(child  (get-buffer-create "k-1-b2")))
    (should (buffer-live-p parent))
    (should (buffer-live-p child))

    (cab-register child parent)
    (kill-buffer child)

    (should     (buffer-live-p parent))
    (should-not (buffer-live-p child))
    ;; Not true with the new kill-semantics. (should-not (member child (with-current-buffer parent *cab-subordinates*)))

    (setq child (get-buffer-create "b2"))
    (cab-register child parent)

    (should (buffer-live-p parent))
    (should (buffer-live-p child))

    (kill-buffer parent)

    (should-not (buffer-live-p parent))
    (should     (buffer-live-p child))
    (should-not (if (buffer-live-p parent)
		    (with-current-buffer parent
		      (member child *cab-subordinates*))
		  nil))))

(ert-deftest cab-test-kill-2 ()
  "Tests killing in the hierarchy established in (cab-test-registry-2)."
  (let ((fname "cab-test-kill-2")
	(parent  (get-buffer-create "k-2-p2"))
	(child-1 (get-buffer-create "k-2-b21"))
	(child-2 (get-buffer-create "k-2-b22")))
    (cab-register child-1 parent)
    (cab-register child-2 parent)

    (should (= (length (with-current-buffer parent *cab-subordinates*))
	       2))
    (should (equal (with-current-buffer child-1 *cab-parent*)
		   parent))
    (should (equal (with-current-buffer child-2 *cab-parent*)
		   parent))

    (kill-buffer child-2)

    (should-not (buffer-live-p child-2))
    (should     (= (length (with-current-buffer parent *cab-subordinates*))
		   2))
    (should     (member child-2 (with-current-buffer parent *cab-subordinates*)))
    (should     (member child-1 (with-current-buffer parent *cab-subordinates*)))
    (should     (member child-2 (with-current-buffer parent *cab-subordinates*)))
    (should (let* ((subordinates (with-current-buffer parent *cab-subordinates*))
		   (live ())
		   (v))
	      (mapc (lambda (b)
		      (if (buffer-live-p b)
			  (push b live)))
		    subordinates)
	      (setq v (length live))
	      (cdmt-message "    [[%d]]" v)
	      (= v 1)))

    (kill-buffer parent)

    (should-not (buffer-live-p parent))
    (should     (buffer-live-p child-1))))

(ert-deftest cab-test-kill-3 ()
  "Test killing a 3 level hierarchy."
  (let ((fname "cab-test-kill-3")
	(grandparent    (get-buffer-create "k-3-gp3"))
	(parent-1       (get-buffer-create "k-3-p31"))
	(parent-2       (get-buffer-create "k-3-p32"))
	(grandchild-311 (get-buffer-create "k-3-b311"))
	(grandchild-312 (get-buffer-create "k-3-b312"))
	(grandchild-313 (get-buffer-create "k-3-b313"))
	(grandchild-321 (get-buffer-create "k-3-b321"))
	(grandchild-322 (get-buffer-create "k-3-b322")))
    (cab-register parent-1       grandparent)
    (cab-register parent-2       grandparent)
    (cab-register grandchild-311 parent-1)
    (cab-register grandchild-312 parent-1)
    (cab-register grandchild-313 parent-1)
    (cab-register grandchild-321 parent-2)
    (cab-register grandchild-322 parent-2)

    (kill-buffer grandchild-312)
    (should-not (buffer-live-p grandchild-312))
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  (list grandchild-311
		grandchild-313))

    (let ((grandparent-subordinates (with-current-buffer grandparent
				      *cab-subordinates*))
	  (parent-1-subordinates    (with-current-buffer parent-1
				      *cab-subordinates*))
	  (parent-2-subordinates    (with-current-buffer parent-2
				      *cab-subordinates*))
	  (gp-subs)
	  (p1-subs)
	  (p2-subs))

      (setq gp-subs ())
      (mapc (lambda (b)
	      (if (buffer-live-p b)
		  (push b gp-subs)))
	    grandparent-subordinates)
      
      (should (= (length gp-subs) 2))
      (should (member parent-1 grandparent-subordinates))
      (should (member parent-2 grandparent-subordinates))
      (setq p1-subs ())
      (mapc (lambda (b)
	      (if (buffer-live-p b)
		  (push b p1-subs)))
	    parent-1-subordinates)
      (should (= (length p1-subs) 2))
      (should (member grandchild-311 p1-subs))

      (kill-buffer grandchild-322)
	     
      (should-not (buffer-live-p grandchild-322))
	     
      (setq gp-subs ())
      (mapc (lambda (b)
	      (if (buffer-live-p b)
		  (push b gp-subs)))
	    grandparent-subordinates)
      (should (= (length gp-subs) 2))
      (should (member parent-1 grandparent-subordinates))
      (should (member parent-2 grandparent-subordinates))
      (setq p2-subs)
      (mapc (lambda (b)
	      (if (buffer-live-p b)
		  (push b p2-subs)))
	    parent-2-subordinates)
      (should (= (length p2-subs) 1))
      (should (member grandchild-321 p2-subs))

      (kill-buffer parent-2)

      (should-not (buffer-live-p parent-2))
      (should     (buffer-live-p grandchild-321))
      
      (setq grandparent-subordinates (with-current-buffer grandparent
				       *cab-subordinates*))
      (setq gp-subs ())
      (mapc (lambda (b)
	      (if (buffer-live-p b)
		  (push b gp-subs)))
		   grandparent-subordinates)
      
      (should     (= (length gp-subs) 1))
      (should     (member parent-1 gp-subs))
      (should-not (member parent-2 gp-subs))

      (kill-buffer grandparent)
    
      (should     (buffer-live-p parent-1))
      (should-not (buffer-live-p parent-2))
      (should     (buffer-live-p grandchild-311))
      (should-not (buffer-live-p grandchild-312))
      (should     (buffer-live-p grandchild-313))
      (should     (buffer-live-p grandchild-321))
      (should-not (buffer-live-p grandchild-322)))))

(ert-deftest cab-test-register-negatively-0 ()
  "Test of the things that (cab-register) should not do.
Included are results from using (cab-registered-p)."
  (let* ((fname "cab-test-register-negatively-0")
	 (root-buffer (get-buffer-create "rn-0-root"))
	 (rand)
	 (unaffiliated-buffers)
	 (affiliated-buffers)
	 (i))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-0-uab%d" i)) unaffiliated-buffers)
      (setq i (1+ i)))

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-0-ab%d" i)) affiliated-buffers)
      (setq i (1+ i)))
    
    (mapc (lambda (b)
	    (should (cab-register b root-buffer)))
	  affiliated-buffers)
    (mapc (lambda (b)
	    (should (cab-registered-p b root-buffer)))
	  affiliated-buffers)
    (mapc (lambda (b)
	    (should-not (cab-registered-p b root-buffer)))
	  unaffiliated-buffers)

    (cab-deregister root-buffer)

    (mapc (lambda (b)
	    (should (not (buffer-live-p b))))
	  affiliated-buffers)
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  unaffiliated-buffers)))

(ert-deftest cab-test-register-negatively-1 ()
  "Test of the things that (cab-register) should not do with two roots.
Included are results from using (cab-registered-p)."
  (let* ((fname "cab-test-register-negatively-1")
	 (root-buffer-0 (get-buffer-create "rn-1-root0"))
	 (root-buffer-1 (get-buffer-create "rn-1-root1"))
	 (rand)
	 (unaffiliated-buffers)
	 (affiliated-buffers-0)
	 (affiliated-buffers-1)
	 (i))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-1-uab%d" i)) unaffiliated-buffers)
      (setq i (1+ i)))

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-1-ab%d-0" i)) affiliated-buffers-0)
      (setq i (1+ i)))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-1-ab%d-1" i)) affiliated-buffers-1)
      (setq i (1+ i)))
    
    (mapc (lambda (b)
	    (should (cab-register b root-buffer-0)))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (cab-register b root-buffer-1)))
	  affiliated-buffers-1)

    (mapc (lambda (b)
	    (should (cab-registered-p b root-buffer-0)))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (not (cab-registered-p b root-buffer-1))))
	  affiliated-buffers-0)

    (mapc (lambda (b)
	    (should (not (cab-registered-p b root-buffer-1))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (not (cab-registered-p b root-buffer-0))))
	  affiliated-buffers-1)

    (mapc (lambda (rb)
	    (mapc (lambda (b)
		    (should (not (cab-registered-p b rb))))
		  unaffiliated-buffers))
	    (list root-buffer-0
		  root-buffer-1))

    (cab-deregister root-buffer-0)

    (mapc (lambda (b)
	    (should-not (buffer-live-p b)))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  affiliated-buffers-1)
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  unaffiliated-buffers)

    (kill-buffer root-buffer-1)

    (should-not (buffer-live-p root-buffer-1))
    (mapc (lambda (b)
	    (should (buffer-live-p b))
	    (should (with-current-buffer b
		      (boundp '*cab-parent*))))
	  affiliated-buffers-1)
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  unaffiliated-buffers)))

(ert-deftest cab-test-register-negatively-2 ()
  "Test of the things that (cab-register) should not do with two roots.
Included are results from using (cab-registered-p)."
  (let* ((fname "cab-test-register-negatively-2")
	 (root-buffer-0 (get-buffer-create "rn-2-root0"))
	 (root-buffer-1 (get-buffer-create "rn-2-root1"))
	 (rand)
	 (unaffiliated-buffers)
	 (affiliated-buffers-0)
	 (affiliated-buffers-1)
	 (i))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-2-uab%d" i)) unaffiliated-buffers)
      (setq i (1+ i)))

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-2-ab%d-0" i)) affiliated-buffers-0)
      (setq i (1+ i)))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (get-buffer-create (format "rn-2-ab%d-1" i)) affiliated-buffers-1)
      (setq i (1+ i)))
    
    (mapc (lambda (b)
	    (should (cab-register b root-buffer-0)))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (cab-register b root-buffer-1)))
	  affiliated-buffers-1)

    (kill-buffer root-buffer-0)

    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (buffer-live-p b))
	    (should (cab-registered-p b root-buffer-1)))
	  affiliated-buffers-1)
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  unaffiliated-buffers)

    (kill-buffer root-buffer-1)

    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  (append affiliated-buffers-0 affiliated-buffers-1))
    (mapc (lambda (b)
	    (should (buffer-live-p b)))
	  unaffiliated-buffers)))


;; Seed the random number generator.
(random t)

(unless noninteractive
  (ert "\\`cab-"))

(cab-clean-ruthlessly)


;;; cab-test.el ends here

