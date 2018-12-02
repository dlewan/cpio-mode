;	$Id: ttt.el,v 1.4 2018/12/02 00:13:07 doug Exp $	

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
		(lambda (l r)
		  (cond ((< (car l) (car r))
			 t)
			(t
			 nil))))))

(defvar ttt-buffers)
(setq ttt-buffers (list "cab-test.el" "cpio-dired-bin-test.el" "cpio-dired-crc-test.el" "cpio-dired-odc-test.el" "cpio-dired-test.el"))
(mapc 'find-file-noselect ttt-buffers)
(setq ttt-buffers (randomize-list (append ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers ttt-buffers)))
(setq ttt-buffers (randomize-list ttt-buffers))

(shell-command "make clean" "*clean*")
(shell-command "make elc" "*elc*")

;; (mapc 'eval-buffer b)
(let ((ct 0))
  (mapc (lambda (b)
	  (eval-buffer b)
	  (with-current-buffer "*ert*"
	    (rename-buffer (format "*ert*<%d>" ct)))
	  (setq ct (1+ ct))
	  (sit-for 2.0))
	ttt-buffers))

