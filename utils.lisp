(in-package #:%rtg-math)

(defmacro case= (form &body cases)
  (let ((g (gensym "val")))
    (labels ((wrap-case (c) `((= ,g ,(first c)) ,@(rest c))))
      (let* ((cases-but1 (mapcar #'wrap-case (butlast cases)))
             (last-case (car (last cases)))
             (last-case (if (eq (car last-case) 'otherwise)
                            `(t ,@(rest last-case))
                            (wrap-case last-case)))
             (cases (append cases-but1 (list last-case))))
        `(let ((,g ,form))
           (cond ,@cases))))))

(defmacro ecase= (form &body cases)
  (let ((gform (gensym "form")))
    `(let ((,gform ,form))
       (case= ,gform
         ,@cases
         (otherwise (error "~a fell through ecase=. Expected one of:~%~a"
                           ',form ',(mapcar #'first cases)))))))


(defun parse-defn-args (typed-args result-types)
  (let ((seen-&key nil)
        (seen-&rest nil)
        (seen-&optional nil)
        (f-args nil)
        (f-sigs nil)
        (f-decls nil))
    (labels ((kwd (x) (intern (symbol-name x) :keyword)))
      (loop :for x :in typed-args :do
         (destructuring-bind (name &optional (type t) opt-val) (ensure-list x)
           (cond
             ((string= name "&KEY")
              (when seen-&optional
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&key t)
              (push name f-args)
              (push name f-sigs))
             ((string= name "&REST")
              (when seen-&optional
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&rest t)
              (push name f-args)
              (push name f-sigs))
             ((string= name "&OPTIONAL")
              (when seen-&rest
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&optional t)
              (push name f-args)
              (push name f-sigs))
             ((char= #\& (char (symbol-name name) 0))
              (error "~a not valid in defn forms" name))
             (t
              (let ((decl-type (cond
                                 (seen-&key `(or ,type null))
                                 (seen-&optional `(or ,type null))
                                 (t type)))
                    (f-sig (cond
                             (seen-&key `(,(kwd name) ,type))
                             (seen-&optional `(or ,type null))
                             (t type)))
                    (f-arg (cond
                             (seen-&key `(,name ,opt-val))
                             (seen-&optional `(,name ,opt-val))
                             (t name))))
                (unless seen-&rest
                  (push `(type ,decl-type ,name) f-decls))
                (push f-sig f-sigs)
                (push f-arg f-args)))))))
    ;;
    (values (reverse f-args)
            `(function ,(reverse f-sigs) ,result-types)
            (reverse f-decls))))

(defun %defn (name typed-args result-types inlinable-p inline-p body)
  (multiple-value-bind (args ftype type-decls)
      (parse-defn-args typed-args result-types)
    (multiple-value-bind (body decls doc)
        (parse-body body :documentation t)
      (let* ((decls (reduce #'append (mapcar #'rest decls)))
             (decls (if inline-p
                        (cons `(inline ,name) decls)
                        decls)))
        `(progn
           (declaim
            ,@(when inline-p `((inline ,name)))
            (ftype ,ftype ,name))
           (defun ,name ,args
             ,@(when doc (list doc))
             (declare ,@type-decls)
             (declare ,@decls)
             ,@body)
           ,@(when (and inlinable-p (not inline-p))
               `((declaim (notinline ,name))))
           ',name)))))

(defmacro defn (name typed-args result-types &body body)
  "Define a typed function"
  (%defn name typed-args result-types nil nil body))

(defmacro defn-inline (name typed-args result-types &body body)
  "Define a typed function and request that it be inlined"
  (%defn name typed-args result-types t t body))

(defmacro defn-inlinable (name typed-args result-types &body body)
  "Define a typed function that can be inlined but by default shouldn't be"
  (%defn name typed-args result-types t nil body))
