(in-package #:cl-user)

(defmacro with-fdefinition ((name function) &body body)
  "Binds the FDEFINITION of NAME to FUNCTION during the execution of the
BODY.  If it wasn't bound before it is FMAKUNBOUND afterwards."
  (let ((bound (gensym))
        (old (gensym)))
    `(symbol-macrolet ((fvalue (fdefinition ',name)))
       (let* ((,bound (fboundp ',name))
              (,old (and ,bound fvalue)))
         (unwind-protect
              (progn
                (setf fvalue ,function)
                ,@body)
           (when ,bound
             (setf fvalue ,old)))))))

(defmacro with-fdefinitions (bindings &body body)
  "Shortcut for multiple sequential WITH-FDEFINITION calls."
  (if bindings
      `(with-fdefinition ,(car bindings)
         (with-fdefinitions ,(cdr bindings)
           ,@body))
      `(progn ,@body)))
