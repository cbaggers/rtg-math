(in-package :rtg-math.region.axis-aligned-box)

(defn-inline maxima ((aabb axis-aligned-box)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (axis-aligned-box-maxima aabb))

(defn-inline minima ((aabb axis-aligned-box)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (axis-aligned-box-minima aabb))

;;------------------------------------------------------------
;; This is just local to this file to keep code clean

(defmacro with-aabb ((mina maxa) aabb &body body)
  (assert (and mina maxa))
  (let ((box (gensym "aabb")))
    `(let* ((,box ,aabb)
            (,mina (axis-aligned-box-minima ,box))
            (,maxa (axis-aligned-box-maxima ,box)))
       (declare (axis-aligned-box ,box) (vec3 ,mina ,maxa))
       ,@body)))

;;------------------------------------------------------------

(defn-inline make ((minima vec3) (maxima vec3)) axis-aligned-box
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-axis-aligned-box :minima minima :maxima maxima))

;;------------------------------------------------------------

;; {TODO} - change this to work on sequences
;;        - dont use loop so that we can declare types
(defn from-points ((list-of-vec3 list)) axis-aligned-box
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
      (make-axis-aligned-box
       :minima (v! min-x min-y min-z)
       :maxima (v! max-x max-y max-z)))))

(defn from-aabbs (&rest (aabbs axis-aligned-box)) axis-aligned-box
  (from-points (loop :for box :in aabbs
                  :collect (maxima box)
                  :collect (minima box))))

;;------------------------------------------------------------

(defn add-point ((aabb axis-aligned-box) (point-v3 vec3)) axis-aligned-box
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina maxa) aabb
    (make-axis-aligned-box
     :minima (v3:make (min (x point-v3) (x mina))
                      (min (y point-v3) (y mina))
                      (min (z point-v3) (z mina)))
     :maxima (v! (max (x point-v3) (x maxa))
                 (max (y point-v3) (y maxa))
                 (max (z point-v3) (z maxa))))))

;; make a non-consing axis-aligned-box package containing merge-point
(defn merge-point ((aabb axis-aligned-box) (point-v3 vec3)) axis-aligned-box
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina maxa) aabb
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
          (setf (z maxa) (z point-v3))))
    aabb))

;;------------------------------------------------------------

(defn = ((aabb-0 axis-aligned-box) (aabb-1 axis-aligned-box)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina-0 maxa-0) aabb-0
    (with-aabb (mina-1 maxa-1) aabb-1
      (and (v3:= mina-0 mina-1)
           (v3:= maxa-0 maxa-1)))))

(defn-inline /= ((aabb-0 axis-aligned-box) (aabb-1 axis-aligned-box)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (not (= aabb-0 aabb-1)))

;;------------------------------------------------------------

(defn intersects-p ((aabb-0 axis-aligned-box) (aabb-1 axis-aligned-box)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina-0 maxa-0) aabb-0
    (with-aabb (mina-1 maxa-1) aabb-1
      ;; not seperate on any axis
      (not (or (> (x mina-0) (x maxa-1)) (> (x mina-1) (x maxa-0))
               (> (y mina-0) (y maxa-1)) (> (y mina-1) (y maxa-0))
               (> (z mina-0) (z maxa-1)) (> (z mina-1) (z maxa-0)))))))

;;------------------------------------------------------------

(defn intersects-with-line3-p ((aabb axis-aligned-box) (line3 line3)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina maxa) aabb
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

(defn intersects-with-ray3-p ((aabb axis-aligned-box) (ray3 ray3)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina maxa) aabb
    (let ((dir (ray3:direction ray3))
          (max-q 0f0)
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
               (when (> max-q min-r)
                 (return-from intersects-with-ray3-p nil))))
         :finally (return t)))))

;;------------------------------------------------------------

(defn intersects-with-line-segment-p ((aabb axis-aligned-box)
                                      (line-seg3 line-segment3))
    boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-aabb (mina maxa) aabb
    (let ((dir (line-seg3:direction line-seg3))
          (max-q 0f0)
          (min-r most-positive-single-float))
      ;; do tests against three sets of planes
      (loop :for i :below 3 :do
         ;; line is parallel to plane
         (if (sfzero-p (aref dir i))
             ;; line passes by box
             (when (or (< (aref (line-seg3:end-point0 line-seg3) i)
                          (aref mina i))
                       (> (aref (line-seg3:end-point0 line-seg3) i)
                          (aref maxa i)))
               (return-from intersects-with-line-segment-p nil))
             ;; compute intersection parameters and sort (swap if neccesary)
             (let* ((orig (line-seg3:end-point0 line-seg3))
                    (q (/ (- (aref mina i) (aref orig i))
                          (aref dir i)))
                    (r (/ (- (aref maxa i) (aref orig i))
                          (aref dir i))))
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
                 (return-from intersects-with-line-segment-p nil))))
         :finally (return t)))))

;;------------------------------------------------------------

;; (defn get-signed-distance-to-plane ((aabb axis-aligned-box)
;;                                     (plane plane)) single-float
;;   )

;;------------------------------------------------------------
