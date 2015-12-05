;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using condition-case to catch errors ...

(condition-case var
    (condition-case var
	(signal 'error '(stringp 99))
      (error
       (signal (car var) (cdr var))))
  (error
   (message "%s" var)))


(symbol-plist 'wrong-type-argument)
=> (error-conditions (wrong-type-argument error) error-message "Wrong type argument")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More complex examples ...

(condition-case nil
    (let ((foo))
      (put 'foo 'error-conditions '(error goo))
      (string= "foo" 99)
      (signal 'foo 99))
  (error (message "yeehah")))

(eval `(condition-case nil
	   (string= "foo" 99)	;; Raises wrong-type-argument
	 (,(if (string= "yes" "no") 'error 'foobar)
	  (message "An error occurred.")
	  (sleep-for 2)
	  (message "Yes it did."))))
