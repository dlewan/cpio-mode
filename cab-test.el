;; -*- coding: utf-8 -*-
;;; cab-text.el --- brief description
;	$Id: cab-test.el,v 1.5 2018/06/16 18:01:35 doug Exp $	

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

;;; Documentation:

;;; Code:

;;
;; Dependencies
;; 
(require 'ert)
(load (concat default-directory "cpio-affiliated-buffers.el"))

(local-set-key "\M-\C-i" 'insert-debugger)
(local-set-key "\M-\C-u" 'update-debuggers)

;; 
;; Vars
;; 


;; 
;; Library
;; 
(ert-deftest cab-test-registry-1 ()
  "Tests a simple (cab-register child parent)."
  (let ((fname "cab-test-registry-1")
	(parent (find-file-noselect "p"))
	(child (find-file-noselect "c")))
    (message "%s(): Entering." fname)
    (message "%s(): 58" fname)
    (if (buffer-live-p parent)
	(message "    Parent buffer is live.")
      (message "    Parent buffer is NOT live."))
    (message "%s(): 62" fname)
    (if (buffer-live-p child)
	(progn (message "%s(): 64" fname)
	       (message "    Child buffer is live."))
      (message "%s(): 66" fname)
      (message "    Child buffer is NOT live."))
    (message "%s(): 68" fname)
    
    (cab-register child parent)
    (message "parent of child is [[%s]]." (with-current-buffer child *cab-parent*))
    (message "subordinates of child are [[%s]]." (with-current-buffer child *cab-subordinates*))
    (message "parent of parent is [[%s]]." (with-current-buffer parent *cab-parent*))
    (message "subordinates of parent are [[%s]]." (with-current-buffer parent *cab-subordinates*))

    (should (progn (message "%s(): 76" fname)
		   (equal (with-current-buffer parent
			    *cab-parent*)
			  nil)))
    (should (progn (message "%s(): 80" fname)
		   (equal (with-current-buffer parent
			    *cab-subordinates*)
			  (list child))))
    (should (progn (message "%s(): 84" fname)
		   (equal (with-current-buffer child
			    *cab-parent*)
			  parent)))
    (should (progn (message "%s(): 88" fname)
		   (equal (with-current-buffer child
			    *cab-subordinates*)
			  ())))
    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-registry-2 ()
  "Tests registering two buffers under the same parent."
  (let ((fname "cab-test-registry-2")
	(parent (find-file-noselect "p2"))
	(buffer-1 (find-file-noselect "b21"))
	(buffer-2 (find-file-noselect "b22")))
    (message "%s(): Entering." fname)

    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)

    (should (progn (message "%s(): 109" fname)
		   (= (with-current-buffer parent
			(length *cab-subordinates*))
		      2)))
    (should (progn (message "%s(): 113" fname)
		   (member buffer-1 (with-current-buffer parent
				      *cab-subordinates*))))
    (should (progn (message "%s(): 116" fname)
		   (member buffer-2 (with-current-buffer parent
				      *cab-subordinates*))))
    (should (progn (message "%s(): 119" fname)
		   (equal (with-current-buffer buffer-1
			    *cab-parent*)
			  parent)))
    (should (progn (message "%s(): 123" fname)
		   (equal (with-current-buffer buffer-2
			    *cab-parent*)
			  parent)))
    (should (progn (message "%s(): 127" fname)
		   (equal (with-current-buffer buffer-1
			    *cab-subordinates*)
			  ())))
    (should (progn (message "%s(): 131" fname)
		   (equal (with-current-buffer buffer-2
			    *cab-subordinates*)
			  ())))
    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-registry-3 ()
  "Tests registering a 3 level hierarchy."
  (let ((fname "cab-test-registery-3")
	(grandparent (find-file-noselect "gp3"))
	(parent-1       (find-file-noselect "p31"))
	(parent-2       (find-file-noselect "p32"))
	(grandchild-311 (find-file-noselect "b311"))
	(grandchild-312 (find-file-noselect "b312"))
	(grandchild-313 (find-file-noselect "b313"))
	(grandchild-321 (find-file-noselect "b321"))
	(grandchild-322 (find-file-noselect "b322")))
    (message "%s(): Entering." fname)
    
    (cab-register parent-1       grandparent)
    (cab-register parent-2       grandparent)
    (cab-register grandchild-311 parent-1)
    (cab-register grandchild-312 parent-1)
    (cab-register grandchild-313 parent-1)
    (cab-register grandchild-321 parent-2)
    (cab-register grandchild-322 parent-2)

    (should (progn (message "%s(): 158" fname)
		   (equal (with-current-buffer grandparent
			    (length *cab-subordinates*))
			  2)))
    (should (progn (message "%s(): 162" fname)
		   (equal (with-current-buffer parent-1
			    (length *cab-subordinates*))
			  3)))
    (should (progn (message "%s(): 166" fname)
		   (equal (with-current-buffer parent-2
			    (length *cab-subordinates*))
			  2)))
    (should (progn (message "%s(): 170" fname)
		   (member parent-1 (with-current-buffer grandparent
				      *cab-subordinates*))))
    (should (progn (message "%s(): 173" fname)
		   (member parent-2 (with-current-buffer grandparent
				      *cab-subordinates*))))
    (should (progn (message "%s(): 176" fname)
		   (member grandchild-311 (with-current-buffer parent-1
					    *cab-subordinates*))))
    (should (progn (message "%s(): 179" fname)
		   (member grandchild-312 (with-current-buffer parent-1
					    *cab-subordinates*))))
    (should (progn (message "%s(): 182" fname)
		   (member grandchild-313 (with-current-buffer parent-1
					    *cab-subordinates*))))
    (should (progn (message "%s(): 185" fname)
		   (member grandchild-321 (with-current-buffer parent-2
					    *cab-subordinates*))))
    (should (progn (message "%s(): 188" fname)
		   (member grandchild-322 (with-current-buffer parent-2
					    *cab-subordinates*))))
    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-deregister-1 ()
  "Test deregistering the registry established in (cab-test-registry-1)."
  (let ((fname "cab-test-registry-1")
	(parent (find-file-noselect "p1"))
	(buffer (find-file-noselect "b2")))
    (message "%s(): Entering." fname)

    (cab-register buffer parent)
    (cab-deregister buffer)

    (message "%s(): 203" fname)
    (with-current-buffer parent 
      (mapc (lambda (sb)
	      (should (null (buffer-live-p sb))))
	    *cab-subordinates*))

    (setq buffer (find-file-noselect "b2"))
    (cab-register buffer parent)
    (cab-deregister parent)
    (should (progn (message "%s(): 211" fname)
		   (buffer-live-p parent)))
    (should (progn (message "%s(): 213" fname)
		   (cab-registered-p buffer parent)))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-deregister-2 ()
  "Test deregistering the registry established in (cab-test-registry-2)."
  (let ((fname "cab-test-registry-2")
	(parent (find-file-noselect "p2"))
	(buffer-1 (find-file-noselect "b21"))
	(buffer-2 (find-file-noselect "b22")))
    (message "%s(): Entering." fname)

    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)
    (cab-deregister buffer-1)


    (should (progn (message "%s(): 230" fname)
		   (= (with-current-buffer parent
			(length *cab-subordinates*))
		      1)))

    (cab-deregister buffer-2)

    (should (progn (message "%s(): 237" fname)
		   (= (with-current-buffer parent
			(length *cab-subordinates*))
		      0)))

    (setq buffer-1 (find-file-noselect "b21"))
    (setq buffer-2 (find-file-noselect "b22"))
    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)

    (should (progn (message "%s(): You can't deregister the root." fname)
		   (null (cab-deregister parent))))

    (should (progn (message "%s(): 248" fname)
		   (buffer-live-p parent)))

    (should (cab-registered-p buffer-1 parent))
    (should (cab-registered-p buffer-2 parent))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-deregister-3 ()
  "Test deregistering the registry established in (cab-test-registry-3)."
  (let ((fname "cab-test-deregister-3")
	(grandparent    (find-file-noselect "gp3"))
	(parent-1       (find-file-noselect "p31"))
	(parent-2       (find-file-noselect "p32"))
	(grandchild-311 (find-file-noselect "b311"))
	(grandchild-312 (find-file-noselect "b312"))
	(grandchild-313 (find-file-noselect "b313"))
	(grandchild-321 (find-file-noselect "b321"))
	(grandchild-322 (find-file-noselect "b322")))
    (message "%s(): Entering." fname)

    (cab-register parent-1       grandparent)
    (cab-register parent-2       grandparent)
    (cab-register grandchild-311 parent-1)
    (cab-register grandchild-312 parent-1)
    (cab-register grandchild-313 parent-1)
    (cab-register grandchild-321 parent-2)
    (cab-register grandchild-322 parent-2)
    (cab-deregister grandchild-311)

    (should (progn (message "%s(): 275" fname)
		   (not (buffer-live-p grandchild-311))))

    (cab-deregister parent-1)

    (should (progn (message "%s(): 280" fname)
		   (not (buffer-live-p grandchild-312))))
    (should (progn (message "%s(): 282" fname)
		   (not (buffer-live-p grandchild-313))))
    (should (progn (message "%s(): 284" fname)
		   (equal (with-current-buffer parent-2
			    *cab-parent*)
			  grandparent)))
    (mapc (lambda (bi)
	    (let ((vname (cdr bi))
		  (b (car bi)))
	      (should (progn (message "%s(): Checking that [[%s]] is still live. (294)" fname vname)
			     (buffer-live-p b)))))
	  (list (cons grandchild-321 "grandchild-321")
		(cons grandchild-321 "grandchild-321")
		))
    (should (progn (message "%s(): 288" fname)
		   (equal (with-current-buffer parent-2
			    (length *cab-subordinates*))
			  2)))
    (should (progn (message "%s(): 292" fname)
		   (member grandchild-322 (with-current-buffer parent-2
					    *cab-subordinates*))))

    ;; (should (null (cab-deregister grandparent)))
    (kill-buffer grandparent)

    (should (progn (message "%s(): 300" fname)
		   (null (buffer-live-p parent-1))))
    (should (progn (message "%s(): 302" fname)
		   (null (buffer-live-p parent-2))))
    (mapc (lambda (bi)
	    (let ((vname (cdr bi))
		  (b (car bi)))
	      (should (progn (message "%s(): Checking if [[%s]] is no longer live. (321)" fname vname)
			     (not (buffer-live-p b))))))
	  (list (cons parent-1 "parent-1")
		(cons parent-2 "parent-2")
		(cons grandchild-311 "grandchild-311")
		(cons grandchild-312 "grandchild-312")
		(cons grandchild-313 "grandchild-313")
		(cons grandchild-321 "grandchild-321")
		(cons grandchild-322 "grandchild-322")))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-kill-1 ()
  "Tests killing in the hierarchy created in (cab-test-registry-1)."

  (let ((fname "cab-test-kill-1")
	(parent (find-file-noselect "p1"))
	(child (find-file-noselect "b2")))
    (message "%s(): Entering." fname)

    (should (cond ((buffer-live-p parent)
		   (message "%s(): 317" fname)
		   (message "    Parent buffer is live.")
		   t)
		  (t
		   (message "%s(): 321" fname)
		   (message "    Parent buffer is not live!")
		   nil)))
    (should (cond ((buffer-live-p child)
		   (message "%s(): 325" fname)
		   (message "    Child buffer is live.")
		   t)
		  (t
		   (message "%s(): 329" fname)
		   (message "    Child buffer is not live!")
		   nil)))

    (cab-register child parent)
    (kill-buffer child)

    (should (progn (message "%s(): 336" fname)
		   (buffer-live-p parent)))
    (should (progn
	      (message "%s(): 339" fname)
	      (message "    Testing if [[%s]] is not live." child)
	      (not (buffer-live-p child))))
    (should (progn (message "%s(): 342" fname)
		   (message "    Testing if killed child [[%s]] is still a member of parent [[%s]]." child parent)
		   (member child (with-current-buffer parent *cab-subordinates*))))

    (setq child (find-file-noselect "b2"))
    (cab-register child parent)

    (should (progn (message "%s(): 349" fname)
		   (buffer-live-p parent)))

    (kill-buffer parent)

    (should (progn (message "%s(): 354" fname)
		   (message "    Testing if child buffer [[%s]] is not live after killing the parent." child)
		   (not (buffer-live-p child))))
    (should (progn (message "%s(): 357" fname)
		   (message "    Testing if parent buffer [[%s]] is not live." parent)
		   (not (buffer-live-p parent))))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-kill-2 ()
  "Tests killing in the hierarchy established in (cab-test-registry-2)."
  (let ((fname "cab-test-kill-2")
	(parent (find-file-noselect "p2"))
	(child-1 (find-file-noselect "b21"))
	(child-2 (find-file-noselect "b22")))
    (message "%s(): Entering." fname)

    (cab-register child-1 parent)
    (cab-register child-2 parent)

    (should (progn (message "%s(): 374" fname)
		   (= (length (with-current-buffer parent
				*cab-subordinates*))
		      2)))
    (should (progn (message "%s(): 378" fname)
		   (equal (with-current-buffer child-1
			    *cab-parent*)
			  parent)))
    (should (progn (message "%s(): 382" fname)
		   (equal (with-current-buffer child-2
			    *cab-parent*)
			  parent)))

    (kill-buffer child-2)

    (should (progn (message "%s(): 389" fname)
		   (not (buffer-live-p child-2))))
    (should (progn (message "%s(): 391" fname)
		   (= (length (with-current-buffer parent
				*cab-subordinates*))
		      2)))
    (should (progn (message "%s(): 395" fname)
		   (member child-2 (with-current-buffer parent
				     *cab-subordinates*))))
    (should (progn (message "%s(): 398" fname)
		   (member child-1 (with-current-buffer parent
				     *cab-subordinates*))))
    (should (progn (message "%s(): 401" fname)
		   (member child-2 (with-current-buffer parent
				     *cab-subordinates*))))
    (should (progn (message "%s(): 404" fname)
		   (let* ((subordinates (with-current-buffer parent
					  *cab-subordinates*))
			  (live ())
			  (v))
		     (message "    Counting the subordinates of the parent [[%s]]." subordinates)
		     (mapc (lambda (b)
			     (if (buffer-live-p b)
				 (push b live)))
			   subordinates)
		     (setq v (length live))
		     (message "    [[%d]]" v)
		     (= v 1))))

    (kill-buffer parent)

    (should (progn (message "%s(): 421" fname)
		   (not (buffer-live-p parent))))
    (should (progn (message "%s(): 423" fname)
		   (not (buffer-live-p child-1))))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-kill-3 ()
  "Test killing a 3 level hierarchy."
  (let ((fname "cab-test-kill-3")
	(grandparent    (find-file-noselect "gp3"))
	(parent-1       (find-file-noselect "p31"))
	(parent-2       (find-file-noselect "p32"))
	(grandchild-311 (find-file-noselect "b311"))
	(grandchild-312 (find-file-noselect "b312"))
	(grandchild-313 (find-file-noselect "b313"))
	(grandchild-321 (find-file-noselect "b321"))
	(grandchild-322 (find-file-noselect "b322")))
    (message "%s(): Entering." fname)

    (cab-register parent-1       grandparent)
    (cab-register parent-2       grandparent)
    (cab-register grandchild-311 parent-1)
    (cab-register grandchild-312 parent-1)
    (cab-register grandchild-313 parent-1)
    (cab-register grandchild-321 parent-2)
    (cab-register grandchild-322 parent-2)

    (kill-buffer grandchild-312)
    (should (progn (message "%s(): 450" fname)
		   (not (buffer-live-p grandchild-312))))

    (let ((grandparent-subordinates (with-current-buffer grandparent
					      *cab-subordinates*))
		  (parent-1-subordinates (with-current-buffer parent-1
					   *cab-subordinates*))
		  (parent-2-subordinates (with-current-buffer parent-2
					   *cab-subordinates*))
		  (gp-subs)
		  (p1-subs)
		  (p2-subs))
    
      (progn (message "%s(): Testing for grandchild-312 as killed." fname)
	     
	     (setq gp-subs ())
	     (mapc (lambda (b)
		     (if (buffer-live-p b)
			 (push b gp-subs)))
		   grandparent-subordinates)
	     
	     (should (progn (message "%s(): 472" fname)
			    (= (length gp-subs) 2)))
	     (should (progn (message "%s(): 474" fname)
			    (member parent-1 grandparent-subordinates)))
	     (should (progn (message "%s(): 476" fname)
			    (member parent-2 grandparent-subordinates)))
	     (setq p1-subs ())
	     (mapc (lambda (b)
		     (if (buffer-live-p b)
			 (push b p1-subs)))
		   parent-1-subordinates)
	     (should (progn (message "%s(): 483" fname)
			    (= (length p1-subs) 2)))
	     (should (progn (message "%s(): 485" fname)
			    (member grandchild-311 p1-subs))))

      (kill-buffer grandchild-322)
	     
      (progn (message "%s(): Testing for granchild-322 as killed." fname)
	     (should (progn (message "%s(): 492" fname)
			    (not (buffer-live-p grandchild-322))))
	     
	     (setq gp-subs ())
	     (mapc (lambda (b)
		     (if (buffer-live-p b)
			 (push b gp-subs)))
		   grandparent-subordinates)
	     (should (progn (message "%s(): 500" fname)
			    (= (length gp-subs) 2)))
	     (should (progn (message "%s(): 502" fname)
			    (member parent-1 grandparent-subordinates)))
	     (should (progn (message "%s(): 504" fname)
			    (member parent-2 grandparent-subordinates)))
	     (setq p2-subs)
	     (mapc (lambda (b)
		     (if (buffer-live-p b)
			 (push b p2-subs)))
		   parent-2-subordinates)
	     (should (progn (message "%s(): 511" fname)
			    (= (length p2-subs) 1)))
	     (should (progn (message "%s(): 513" fname)
			    (member grandchild-321 p2-subs))))

      (kill-buffer parent-2)

      (progn (message "%s(): Testing for parent-2 as killed." fname)

	     (should (progn (message "%s(): 521" fname)
			    (not (buffer-live-p parent-2))))
	     (should (progn (message "%s(): 523" fname)
			    (not (buffer-live-p grandchild-321))))

	     (setq grandparent-subordinates (with-current-buffer grandparent
					      *cab-subordinates*))
	     (setq gp-subs ())
	     (mapc (lambda (b)
		     (if (buffer-live-p b)
			 (push b gp-subs)))
		   grandparent-subordinates)

	     (should (progn (message "%s(): 534" fname)
			    (= (length gp-subs) 1)))
	     (should (progn (message "%s(): 536" fname)
			    (member parent-1 gp-subs)))
	     (should (progn (message "%s(): 538" fname)
			    (not (member parent-2 gp-subs)))))

      (kill-buffer grandparent)
    
      (progn (message "Testing for the grandparent as killed." fname)

	     (should (progn (message "%s(): 547" fname)
			    (not (buffer-live-p parent-1))))
    	     (should (progn (message "%s(): 549" fname)
			    (not (buffer-live-p parent-2))))
	     (should (progn (message "%s(): 551" fname)
			    (not (buffer-live-p grandchild-311))))
	     (should (progn (message "%s(): 553" fname)
			    (not (buffer-live-p grandchild-312))))
	     (should (progn (message "%s(): 555" fname)
			    (not (buffer-live-p grandchild-313))))
	     (should (progn (message "%s(): 557" fname)
			    (not (buffer-live-p grandchild-321))))
	     (should (progn (message "%s(): 559" fname)
			    (not (buffer-live-p grandchild-322)))))
      ;; This bothers the byte compiler for some reason.
      (message "%s(): Leaving." fname))))

(ert-deftest cab-test-register-negatively-0 ()
  "Test of the things that (cab-register) should not do.
Included are results from using (cab-registered-p)."
  (let* ((fname "cab-test-register-negatively-0")
	 (root-buffer (find-file-noselect "root"))
	 (rand)
	 (unaffiliated-buffers)
	 (affiliated-buffers)
	 (i)
	)

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "uab%d" i)) unaffiliated-buffers)
      (setq i (1+ i)))

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "ab%d" i)) affiliated-buffers)
      (setq i (1+ i)))
    
    (mapc (lambda (b)
	    (should (progn (message "%s(): registering [[%s]] with [[%s]]." fname b root-buffer)
			   (cab-register b root-buffer))))
	  affiliated-buffers)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be registered to [[%s]]." fname b root-buffer)
			   (cab-registered-p b root-buffer))))
	  affiliated-buffers)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should NOT be registered to [[%s]]." fname b root-buffer)
			   (not (cab-registered-p b root-buffer)))))
	  unaffiliated-buffers)

    (kill-buffer root-buffer)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be killed [[%s]]." fname b root-buffer)
			   (not (buffer-live-p b)))))
	  affiliated-buffers)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should still be live." fname b)
			   (buffer-live-p b))))
	  unaffiliated-buffers)
    ))

(ert-deftest cab-test-register-negatively-1 ()
  "Test of the things that (cab-register) should not do with two roots.
Included are results from using (cab-registered-p)."
  (let* ((fname "cab-test-register-negatively-1")
	 (root-buffer-0 (find-file-noselect "root0"))
	 (root-buffer-1 (find-file-noselect "root1"))
	 (rand)
	 (unaffiliated-buffers)
	 (affiliated-buffers-0)
	 (affiliated-buffers-1)
	 (i)
	)

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "uab%d" i)) unaffiliated-buffers)
      (setq i (1+ i)))

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "ab%d-0" i)) affiliated-buffers-0)
      (setq i (1+ i)))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "ab%d-1" i)) affiliated-buffers-1)
      (setq i (1+ i)))
    
    (mapc (lambda (b)
	    (should (progn (message "%s(): registering [[%s]] with [[%s]]. (657)" fname b root-buffer-0)
			   (cab-register b root-buffer-0))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (progn (message "%s(): registering [[%s]] with [[%s]]. (661)" fname b root-buffer-1)
			   (cab-register b root-buffer-1))))
	  affiliated-buffers-1)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be registered to [[%s]]. (666)" fname b root-buffer-0)
			   (cab-registered-p b root-buffer-0))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should NOT be registered to [[%s]]. (670)" fname b root-buffer-1)
			   (not (cab-registered-p b root-buffer-1)))))
	  affiliated-buffers-0)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should NOT be registered to [[%s]]. (675)" fname b root-buffer-1)
			   (not (cab-registered-p b root-buffer-1)))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should NOT be registered to [[%s]]. (679)" fname b root-buffer-0)
			   (not (cab-registered-p b root-buffer-0)))))
	  affiliated-buffers-1)

    (mapc (lambda (rb)
	    (mapc (lambda (b)
		    (should (progn (message "%s(): [[%s]] should NOT be registered to [[%s]]. (685)" fname b rb)
				   (not (cab-registered-p b rb)))))
		  unaffiliated-buffers))
	    (list root-buffer-0
		  root-buffer-1))

    (kill-buffer root-buffer-0)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be killed [[%s]]. (694)" fname b root-buffer-0)
			   (not (buffer-live-p b)))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should still be live [[%s]]. (698)" fname b root-buffer-1)
			   (buffer-live-p b))))
	  affiliated-buffers-1)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should still be live. (702)" fname b)
			   (buffer-live-p b))))
	  unaffiliated-buffers)

    (kill-buffer root-buffer-1)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be killed [[%s]]. (709)" fname b root-buffer-1)
			   (not (buffer-live-p b)))))
	  affiliated-buffers-1)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should still be live. (713)" fname b)
			   (buffer-live-p b))))
	  unaffiliated-buffers)
    

    ))

(ert-deftest cab-test-register-negatively-2 ()
  "Test of the things that (cab-register) should not do with two roots.
Included are results from using (cab-registered-p)."
  (let* ((fname "cab-test-register-negatively-2")
	 (root-buffer-0 (find-file-noselect "root0"))
	 (root-buffer-1 (find-file-noselect "root1"))
	 (rand)
	 (unaffiliated-buffers)
	 (affiliated-buffers-0)
	 (affiliated-buffers-1)
	 (i)
	)

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "uab%d" i)) unaffiliated-buffers)
      (setq i (1+ i)))

    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "ab%d-0" i)) affiliated-buffers-0)
      (setq i (1+ i)))
    (setq i 0)
    (setq rand (+ 5 (mod (random) 20)))
    (while (< i rand)
      (push (find-file-noselect (format "ab%d-1" i)) affiliated-buffers-1)
      (setq i (1+ i)))
    
    (mapc (lambda (b)
	    (should (progn (message "%s(): registering [[%s]] with [[%s]]. (657)" fname b root-buffer-0)
			   (cab-register b root-buffer-0))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (progn (message "%s(): registering [[%s]] with [[%s]]. (661)" fname b root-buffer-1)
			   (cab-register b root-buffer-1))))
	  affiliated-buffers-1)

    (kill-buffer root-buffer-0)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be killed." fname b)
			   (null (buffer-live-p b)))))
	  affiliated-buffers-0)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] is still alive." fname b)
			   (buffer-live-p b)))
	    (should (progn (message "%s(): [[%s]] is still registered with [[%s]]." fname b root-buffer-1)
			   (cab-registered-p b root-buffer-1))))
	  affiliated-buffers-1)
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should still be alive." fname b)
			   (buffer-live-p b))))
	  unaffiliated-buffers)

    (kill-buffer root-buffer-1)

    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should be killed." fname b)
			   (null (buffer-live-p b)))))
	  (append affiliated-buffers-0 affiliated-buffers-1))
    (mapc (lambda (b)
	    (should (progn (message "%s(): [[%s]] should still be alive." fname b)
			   (buffer-live-p b))))
	  unaffiliated-buffers)
    ))


;; Seed the random number generator.
(random t)

(ert "cab-")


(provide 'cab-test)
;;; cab-test.el ends here

