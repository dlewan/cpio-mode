;; -*- coding: utf-8 -*-
;;; cab-text.el --- brief description
;	$Id: cab-test.el,v 1.1.2.15 2018/05/11 20:13:12 doug Exp $	

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
	(parent (get-buffer-create "p"))
	(child (get-buffer-create "c")))
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
    (should (progn (message "%s(): 92" fname)
		   (equal (with-current-buffer child
			    *cab-parent*)
			  parent)))
    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-registry-2 ()
  "Tests registering two buffers under the same parent."
  (let ((fname "cab-test-registry-2")
	(parent (get-buffer-create "p2"))
	(buffer-1 (get-buffer-create "b21"))
	(buffer-2 (get-buffer-create "b22")))
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
	(grandparent (get-buffer-create "gp3"))
	(parent-1       (get-buffer-create "p31"))
	(parent-2       (get-buffer-create "p32"))
	(grandchild-311 (get-buffer-create "b311"))
	(grandchild-312 (get-buffer-create "b312"))
	(grandchild-313 (get-buffer-create "b313"))
	(grandchild-321 (get-buffer-create "b321"))
	(grandchild-322 (get-buffer-create "b322")))
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
	(parent (get-buffer-create "p1"))
	(buffer (get-buffer-create "b2")))
    (message "%s(): Entering." fname)

    (cab-register buffer parent)
    (cab-deregister buffer)

    (should (progn (message "%s(): 203" fname)
		   (equal (with-current-buffer parent
			    *cab-subordinates*)
			  ())))

    (setq buffer (get-buffer-create "b2"))
    (cab-register buffer parent)
    (cab-deregister parent)
    (should (progn (message "%s(): 211" fname)
		   (not (buffer-live-p parent))))
    (should (progn (message "%s(): 213" fname)
		   (not (buffer-live-p buffer))))
    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-deregister-2 ()
  "Test deregistering the registry established in (cab-test-registry-2)."
  (let ((fname "cab-test-registry-2")
	(parent (get-buffer-create "p2"))
	(buffer-1 (get-buffer-create "b21"))
	(buffer-2 (get-buffer-create "b22")))
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

    (setq buffer-1 (get-buffer-create "b21"))
    (setq buffer-2 (get-buffer-create "b22"))
    (cab-register buffer-1 parent)
    (cab-register buffer-2 parent)
    (cab-deregister parent)

    (should (progn (message "%s(): 248" fname)
		   (not (buffer-live-p parent))))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-deregister-3 ()
  "Test deregistering the registry established in (cab-test-registry-3)."
  (let ((fname "cab-test-deregister-3")
	(grandparent    (get-buffer-create "gp3"))
	(parent-1       (get-buffer-create "p31"))
	(parent-2       (get-buffer-create "p32"))
	(grandchild-311 (get-buffer-create "b311"))
	(grandchild-312 (get-buffer-create "b312"))
	(grandchild-313 (get-buffer-create "b313"))
	(grandchild-321 (get-buffer-create "b321"))
	(grandchild-322 (get-buffer-create "b322")))
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
    (should (progn (message "%s(): 288" fname)
		   (equal (with-current-buffer parent-2
			    (length *cab-subordinates*))
			  2)))
    (should (progn (message "%s(): 292" fname)
		   (member grandchild-322 (with-current-buffer parent-2
					    *cab-subordinates*))))

    (cab-deregister grandparent)

    (should (progn (message "%s(): 298" fname)
		   (not (buffer-live-p grandparent))))
    (should (progn (message "%s(): 300" fname)
		   (not (buffer-live-p parent-1))))
    (should (progn (message "%s(): 302" fname)
		   (not (buffer-live-p parent-2))))

    (message "%s(): Leaving." fname)))

(ert-deftest cab-test-kill-1 ()
  "Tests killing in the hierarchy created in (cab-test-registry-1)."

  (let ((fname "cab-test-kill-1")
	(parent (get-buffer-create "p1"))
	(child (get-buffer-create "b2")))
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

    (setq child (get-buffer-create "b2"))
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
	(parent (get-buffer-create "p2"))
	(child-1 (get-buffer-create "b21"))
	(child-2 (get-buffer-create "b22")))
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
	(grandparent    (get-buffer-create "gp3"))
	(parent-1       (get-buffer-create "p31"))
	(parent-2       (get-buffer-create "p32"))
	(grandchild-311 (get-buffer-create "b311"))
	(grandchild-312 (get-buffer-create "b312"))
	(grandchild-313 (get-buffer-create "b313"))
	(grandchild-321 (get-buffer-create "b321"))
	(grandchild-322 (get-buffer-create "b322")))
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
			    (not (buffer-live-p grandchild-322))))))
    (message "%s(): Leaving." fname)))


(provide 'cab-test)
;;; cab-test.el ends here

