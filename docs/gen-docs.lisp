;;
;; To generate docs:
;;
;; - (ql:quickload :staple)
;; - compile the gen-docs function below
;; - (asdf:load-system :rtg-math :force t :verbose t)
;; - (gen-docs)
;;
;; Not how staple was intended to be used but works so ¯\_(ツ)_/¯

(defun gen-docs ()
  (let* ((things '(staple:symb-function
                   staple:symb-accessor))
         (current (loop :for n :in things :collect (staple:converter n)))
         (ignores (lambda (x y) (declare (ignore x y)))))
    (loop
       :for n :in things
       :for c :in current
       :do (when c (setf (staple:converter n) ignores)))
    (unwind-protect
         (staple:generate :rtg-math)
      (loop
         :for n :in things
         :for c :in current
         :do (when c (setf (staple:converter n) c))))))
