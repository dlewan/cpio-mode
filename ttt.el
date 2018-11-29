;	$Id: ttt.el,v 1.2 2018/11/29 01:57:16 doug Exp $	

;;
;; Eval this buffer to run lots of tests against cpio-mode.
;;

;; From cards/deck.el
(defun randomize-list (ltr)
  "Randomize a list using Knuth's algorithm."
  (mapcar (lambda (le)
	    (cdr le))
	  (sort (mapcar (lambda (le)
			  (cons (random) le))
			ltr)
		'(lambda (l r)
		   (cond ((< (car l) (car r))
			  t)
			 (t
			  nil))))))

(setq buffers (list "cab-test.el" "cpio-dired-bin-test.el" "cpio-dired-crc-test.el" "cpio-dired-odc-test.el" "cpio-dired-test.el"))
(mapc 'find-file-noselect buffers)
(setq buffers (randomize-list (append buffers buffers buffers buffers buffers buffers buffers buffers)))
(setq buffers (randomize-list buffers))
;; (mapc 'eval-buffer b)
(let ((ct 0))
  (mapc (lambda (b)
	  (eval-buffer b)
	  (with-current-buffer "*ert*"
	    (rename-buffer (format "*ert*<%d>" ct)))
	  (setq ct (1+ ct))
	  (sit-for 2.0))
	buffers))

