(defvar *newline* ?\n)

(defmacro output-port* (var+path+erase &rest body)
  (destructuring-bind (var path erase? . min?) var+path+erase
    `(save-excursion
       (let ((,var (find-file ,path)))
	 ,(and erase? '(erase-buffer))
	 ,(if (car* min? nil) '(goto-char (point-min)) '(goto-char (point-max)))
	 (prog1
	     (progn
	       ,@body)
	   (save-buffer)
	   (kill-buffer ,var))))))

(defun string->file (str file)
  (output-port* (out file t) (princ str out)))

(defun port->string (port)
  (with-current-buffer port
    (buffer-substring-no-properties (point-min) (point-max))))

(defun file->string (path)
  (letl it (find-file-noselect path)
    (prog1 (port->string it) (kill-buffer it))))


(defun emacs-common-lisp-string-search (str char start)
  (while (and start (not (= (aref str start) char)) )
    (if (= start 0)
	(setq start nil)
	(setq start (1- start))))
  start)
;; 
(defun emacs-common-lisp:convert-last (prev x)
  (cond ((eq prev :u) `(prin1-to-string (list 'write  ,x)))
	((eq prev :s) (format "(prin1 %s)" x))
	(t (prin1-to-string `(write ,x)))))

(defun emacs-common-lisp:convert (prev x)
  (cond ((eq prev :i)
	 (list 'concat "(defvar " (symbol-name x) " (quote " `(prin1-to-string ,x)"))"))
	((eq prev :u)
	 `(prin1-to-string ,x))
	((eq prev :s) x)
	(t (prin1-to-string x))))

(defmacro emacs-common-lisp-command (&rest args)
  (let ((pre1
	  (do ((args args (cdr args))
	       (prev nil x)
	       (x (car args) (car args))
	       (ret '() ))
	      ((null args)
	       (nreverse (list* (emacs-common-lisp:convert-last prev x) " " ret)))
	    (setq ret (unless (keywordp x)
			  (list* (emacs-common-lisp:convert prev x) " " ret))))))
    `(let ((fname (make-temp-file "emacs-common-lisp" nil ".tmp")))
       (string->file (concat ,@pre1) fname)
       (let* ((it (with-output-to-string
		      (call-process "sbcl" fname standard-output nil "--noinform" "--no-sysinit" "--batch" "--no-print")))
	      (last (- (length it) 4))
	      (str (substring it (emacs-common-lisp-string-search it *newline* last) (1+ last))))
	 (if (string-match "debugger" it)
	     (progn (insert "emacs-common-lisp: error\n")
		    (insert "\n------input------\n")
		    (insert (file->string fname))
		    (delete-file fname)
		    (insert "\n------ CL.output------------\n")
		    (insert it)
		    (insert "\n------emacs-common-lisp end------------\n"))
	     (delete-file fname)
	     (car (read-from-string str)))))))

(defmacro emacs-common-lisp-slime (&rest args)
  (let* ((export '())
	 (ret '())
	 (pre1
	  (do* ((args args (cdr args))
		(prev nil x)
		(x (car args) (car args)))
	       ((null args)  (reverse ret))
	    (cond ((keywordp x)
		   ret)
		  ((eq prev :i)
		   (push `(list (quote ,x) (list 'quote ,x)) export)
		   ret)
		  (t (setq ret (cons x ret)))))))
    `(let ((it (slime-eval (list 'swank:eval-and-grab-output (prin1-to-string (list 'let (list ,@export) (quote ,@pre1)))))))
       (car (read-from-string (cadr it))))))

(defmacro emacs-common-lisp (&rest args)
  (if (slime-connected-p)
      `(emacs-common-lisp-slime ,@args)
      `(emacs-common-lisp-command ,@args)))

(provide 'emacs-common-lisp)
