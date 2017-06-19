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
                    (list 'type type name)))))
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

(defvar *standard-declarations*
  '(dynamic-extent  ignore     optimize
    ftype           inline     special
    ignorable       notinline  type))

(defgeneric handle-defn-declaration (name args)
  (:method (name args)
    nil))

(defmacro define-defn-declaration (name args &body body)
  (assert (not (member name *standard-declarations*)) (name))
  (with-gensyms (gname d-args)
    `(defmethod handle-defn-declaration ((,gname (eql ',name)) ,d-args)
       (declare (ignore ,gname))
       (destructuring-bind ,args ,d-args
         ,@body))))

(defun process-defn-declares (decls)
  (let ((data
         (loop :for decl :in decls :collect
            (let ((name (first decl)))
              (if (or (listp name) (member name *standard-declarations*))
                  (list decl nil nil)
                  (cons nil (multiple-value-list
                             (funcall #'handle-defn-declaration
                                      name (rest decl)))))))))
    (list (remove nil (mapcar #'first data))    ;; cl-decl
          (remove nil (mapcar #'second data))   ;; heads
          (remove nil (mapcar #'third data))))) ;; tails

(defun %defn (name typed-args result-types inline-p body)
  (multiple-value-bind (args ftype type-decls)
      (parse-defn-args typed-args result-types)
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      (destructuring-bind (decls heads tails)
          (process-defn-declares (reduce #'append (mapcar #'rest decls)))
        (let* ((decls (if inline-p
                          (cons `(inline ,name) decls)
                          decls))
               (body (if heads
                         (append heads body)
                         body))
               (body (if tails
                         `((prog1 (progn ,@body) ,@tails))
                         body)))
          `(progn
             (declaim
              ,@(when inline-p `((inline ,name)))
              (ftype ,ftype ,name))
             (defun ,name ,args
               ,@(when doc (list doc))
               (declare ,@type-decls)
               (declare ,@decls)
               ,@body)))))))

(defmacro defn (name typed-args result-types &body body)
  (%defn name typed-args result-types nil body))

(defmacro defn-inline (name typed-args result-types &body body)
  (%defn name typed-args result-types t body))

;;
;; Example usage
;;
;; (define-defn-declaration tester (&key foo bar)
;;   (values `(print ',foo)
;;           `(print ',bar)))
;;
;; (defn-inline foo ((a float)) float
;;   (declare (tester :foo 1 :bar 2))
;;   (* a a))
