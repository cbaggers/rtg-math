(in-package :rtg-math.regions.axis-aligned-box)

(defstruct axis-aligned-box
  (minima (v! -1 -1 -1) :type vec3)
  (maxima (v! 1 1 1) :type vec3))

(declaim (inline maxima))
(defun maxima (aab)
  (axis-aligned-box-maxima aab))

(declaim (inline minima))
(defun minima (aab)
  (axis-aligned-box-minima aab))

;;------------------------------------------------------------
;; This is just local to this file to keep code clean

(defmacro with-aab ((mina maxa) aab &body body)
  (assert (and mina maxa))
  (let ((box (gensym "aab")))
    `(let* ((,box ,aab)
            (,mina (axis-aligned-box-minima ,box))
            (,maxa (axis-aligned-box-maxima ,box)))
       (declare (axis-aligned-box ,box) (vec3 ,mina ,maxa))
       ,@body)))

;;------------------------------------------------------------

(declaim (inline make-aab))
(defun make (&key (minima (v! -1 -1 -1)) (maxima (v! 1 1 1)))
  (make-axis-aligned-box
   :minima minima
   :maxima maxima))

;; {TODO} - change this to work on sequences
;;        - dont use loop so that we can declare types
(defun from-points (list-of-vec3)
  (declare (optimize speed))
  (assert list-of-vec3)
  (let ((first (first list-of-vec3)))
    (declare (vec3 first))
    (let ((max-x (x first))
          (min-x (x first))
          (max-y (y first))
          (min-y (y first))
          (max-z (z first))
          (min-z (z first)))
      (declare (single-float max-x min-x max-y min-y max-z min-z))
      (loop :for vec :in (rest list-of-vec3) :do
         (let ((vec vec))
           (declare (vec3 vec))
           (setf max-x (max max-x (x vec)))
           (setf min-x (min min-x (x vec)))
           (setf max-y (max max-y (y vec)))
           (setf min-y (min min-y (y vec)))
           (setf max-z (max max-z (z vec)))
           (setf min-z (min min-z (z vec)))))
      (make :minima (v! min-x min-y min-z)
            :maxima (v! max-x max-y max-z)))))

(defun from-axis-aligned-boxes (aab-0 aab-1)
  (from-points (list (maxima aab-0)
                     (maxima aab-1)
                     (minima aab-0)
                     (minima aab-1))))

(defun from-aabs (aab-0 aab-1)
  (from-axis-aligned-boxes aab-0 aab-1))

;;------------------------------------------------------------

(defun add-point (aab point-v3)
  (declare (optimize speed) (inline x y z)
           (vec3 point-v3) (axis-aligned-box aab))
  (with-aab (mina maxa) aab
    (make :minima (v3:make (min (x point-v3) (x mina))
                           (min (y point-v3) (y mina))
                           (min (z point-v3) (z mina)))
          :maxima (v! (max (x point-v3) (x maxa))
                      (max (y point-v3) (y maxa))
                      (max (z point-v3) (z maxa))))))



(defun merge-point (aab point-v3)
  (declare (optimize speed) (inline x y z (setf x) (setf y) (setf z))
           (vec3 point-v3) (axis-aligned-box aab))
  (with-aab (mina maxa) aab
    (if (< (x point-v3) (x mina))
        (setf (x mina) (x point-v3))
        (when (> (x point-v3) (x maxa))
          (setf (x maxa) (x point-v3))))
    (if (< (y point-v3) (y mina))
        (setf (y mina) (y point-v3))
        (when (> (y point-v3) (y maxa))
          (setf (y maxa) (y point-v3))))
    (if (< (z point-v3) (z mina))
        (setf (z mina) (z point-v3))
        (when (> (z point-v3) (z maxa))
          (setf (z maxa) (z point-v3))))))

;;------------------------------------------------------------

(defun = (aab-0 aab-1)
  (with-aab (mina-0 maxa-0) aab-0
    (with-aab (mina-1 maxa-1) aab-1
      (and (v3:= mina-0 mina-1)
           (v3:= maxa-0 maxa-1)))))

(defun /= (aab-0 aab-1)
  (not (= aab-0 aab-1)))

;;------------------------------------------------------------

(defun intersects-p (aab-0 aab-1)
  (declare (optimize speed))
  (with-aab (mina-0 maxa-0) aab-0
    (with-aab (mina-1 maxa-1) aab-1
      ;; not seperate on any axis
      (not (or (> (x mina-0) (x maxa-1)) (> (x mina-1) (x maxa-0))
               (> (y mina-0) (y maxa-1)) (> (y mina-1) (y maxa-0))
               (> (z mina-0) (z maxa-1)) (> (z mina-1) (z maxa-0)))))))

;;------------------------------------------------------------

(defun intersects-with-line3-p (aab line3)
  (declare (optimize speed) (axis-aligned-box aab)
           (line3 line3))
  (with-aab (mina maxa) aab
    (let ((dir (line3:direction line3))
          (max-q most-negative-single-float)
          (min-r most-positive-single-float))
      ;; do tests against three sets of planes
      (loop :for i :below 3 :do
         ;; line is parallel to plane
         (if (sfzero-p (aref dir i))
             ;; line passes by box
             (when (or (< (aref (line3:origin line3) i) (aref mina i))
                       (> (aref (line3:origin line3) i) (aref maxa i)))
               (return-from intersects-with-line3-p nil))
             ;; compute intersection parameters and sort (swap if neccesary)
             (let* ((orig (line3:origin line3))
                    (q (/ (- (aref mina i) (aref orig i)) (aref dir i)))
                    (r (/ (- (aref maxa i) (aref orig i)) (aref dir i))))
               (when (> q r)
                 (let ((tmp q))
                   (setf q r
                         r tmp)))
               (when (> q max-q)
                 (setf max-q q))
               (when (< r min-r)
                 (setf min-r r))
               ;; check for intersection failure
               (when (> max-q min-r)
                 (return-from intersects-with-line3-p nil))))
         :finally (return t)))))

;;------------------------------------------------------------

(defun intersects-with-ray3-p (aab ray3)
  (declare (optimize speed) (axis-aligned-box aab)
           (ray3 ray3))
  (with-aab (mina maxa) aab
    (let ((dir (ray3:direction ray3))
          (max-q most-negative-single-float)
          (min-r most-positive-single-float))
      ;; do tests against three sets of planes
      (loop :for i :below 3 :do
         ;; line is parallel to plane
         (if (sfzero-p (aref dir i))
             ;; line passes by box
             (when (or (< (aref (ray3:origin ray3) i) (aref mina i))
                       (> (aref (ray3:origin ray3) i) (aref maxa i)))
               (return-from intersects-with-ray3-p nil))
             ;; compute intersection parameters and sort (swap if neccesary)
             (let* ((orig (ray3:origin ray3))
                    (q (/ (- (aref mina i) (aref orig i)) (aref dir i)))
                    (r (/ (- (aref maxa i) (aref orig i)) (aref dir i))))
               (when (> q r)
                 (let ((tmp q))
                   (setf q r
                         r tmp)))
               (when (> q max-q)
                 (setf max-q q))
               (when (< r min-r)
                 (setf min-r r))
               ;; check for intersection failure
               (when (or (< min-r 0s0) (> max-q min-r))
                 (return-from intersects-with-ray3-p nil))))
         :finally (return t)))))

;;------------------------------------------------------------

;; {TODO}

;; (defun intersects-with-line-segment-p (aab line-seg3)
;;   nil)

;;------------------------------------------------------------

;; {TODO}

;; (defun get-signed-distance-to-plane (aab plane)
;;   )

;;------------------------------------------------------------
