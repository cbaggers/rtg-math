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


(defun parse-defn-args (typed-args result-types)
  (let ((seen-&rest nil))
    (labels ((to-pair (x)
               (destructuring-bind (name &optional (type t)) (ensure-list x)
                 (cond
                   ((or (string= name "&OPTIONAL")
                        (string= name "&AUX"))
                    (error "~a not valid in defn forms" name))
                   ((string= name "&REST")
                    (setf seen-&rest t)
                    nil)
                   ((and (not (char= #\& (char (symbol-name name) 0)))
                         (not seen-&rest))
                    (list type name)))))
             ;;
             (mayb-first (x)
               (if (listp x)
                   (first x)
                   x))
             ;;
             (mayb-second (x)
               (if (listp x)
                   (second x)
                   x)))
      ;;
      (values (mapcar #'mayb-first typed-args)
              `(function ,(mapcar #'mayb-second typed-args)
                         ,result-types)
              (remove nil (mapcar #'to-pair typed-args))))))

(defmacro defn (name typed-args result-types &body body)
  (multiple-value-bind (args ftype type-decls)
      (parse-defn-args typed-args result-types)
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      `(progn
         (declaim (ftype ,ftype ,name))
         (defun ,name ,args
           ,@(when doc (list doc))
           (declare ,@type-decls)
           ,@decls
           ,@body)))))

(defmacro defn-inline (name typed-args result-types &body body)
  (multiple-value-bind (args ftype type-decls)
      (parse-defn-args typed-args result-types)
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      `(progn
         (declaim (inline ,name) (ftype ,ftype ,name))
         (defun ,name ,args
           ,@(when doc (list doc))
           (declare ,@type-decls)
           ,@decls
           ,@body)))))
