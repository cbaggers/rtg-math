(in-package :rtg-math.region.line3)

;; A line of infinite length in ℝ3

(defn-inline origin ((line3 line3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line3-origin line3))

(defn direction ((line3 line3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line3-direction line3))

(defn-inline make ((origin-v3 vec3) (direction-v3 vec3)) line3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-line3 :origin origin-v3
              :direction (v3:normalize direction-v3)))

;;----------------------------------------------------------------

(defn transform-m3 ((matrix3 mat3) (line3 line3)) line3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (m3:*v matrix3 (origin line3))
        (m3:*v matrix3 (direction line3))))

(defn transform-q ((quat quaternion) (line3 line3)) line3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (q:rotate (origin line3) quat)
        (q:rotate (direction line3) quat)))

;;----------------------------------------------------------------

(defn-inline = ((line3-a line3) (line3-b line3)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (v3:= (origin line3-a) (origin line3-b))
       (v3:= (direction line3-a) (direction line3-b))))

(defn /= ((line3-a line3) (line3-b line3)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline =))
  (not (= line3-a line3-b)))

;;----------------------------------------------------------------

(defn closest-point ((line3 line3) (point-v3 vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir (line3-direction line3))
         (w (v3:- point-v3 (line3-origin line3)))
         (vsq (v3:dot dir dir))
         (proj (v3:dot w dir)))
    (v3:+ (line3-origin line3)
          (v3:*s dir (/ proj vsq)))))

(defn closest-line-points ((line3-a line3) (line3-b line3))
    (values vec3 vec3)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((w0 (v3:- (line3-origin line3-a)
                   (line3-origin line3-b)))
         (a (v3:dot (line3-direction line3-a)
                    (line3-direction line3-a)))
         (b (v3:dot (line3-direction line3-a)
                    (line3-direction line3-b)))
         (c (v3:dot (line3-direction line3-b)
                    (line3-direction line3-b)))
         (d (v3:dot (line3-direction line3-a)
                    w0))
         (e (v3:dot (line3-direction line3-b)
                    w0))
         (denom (- (* a c) (* b b))))
    (if (sfzero-p denom)
        (let ((p0 (line3-origin line3-a))
              (p1 (v3:+ (line3-origin line3-b)
                        (v3:*s (line3-direction line3-b) (/ e c)))))
          (values p0 p1))
        (let ((p0 (v3:+ (line3-origin line3-a)
                        (v3:*s (line3-direction line3-a)
                               (/ (- (* b e) (* c d))
                                     denom))))
              (p1 (v3:+ (line3-origin line3-b)
                        (v3:*s (line3-direction line3-b)
                               (/ (- (* a e) (* b d))
                                     denom)))))
          (values p0 p1)))))

;;----------------------------------------------------------------

(defn distance-squared-to-point ((line3 line3) (point-v3 vec3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir (line3-direction line3))
         (w (v3:- point-v3 (line3-origin line3)))
         (vsq (v3:dot dir dir))
         (proj (v3:dot w dir)))
    (let ((t-c (/ proj vsq)))
      (values (- (v3:dot w w) (* t-c proj))
              t-c))))

(defn distance-to-point ((line3 line3) (point-v3 vec3))
    (values single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c) (distance-squared-to-point line3 point-v3)
    (values (sqrt val) t-c)))

;;----------------------------------------------------------------

(defn distance-squared-to-line3 ((line3-a line3) (line3-b line3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir-a (line3-direction line3-a))
         (dir-b (line3-direction line3-b))
         (origin-a (line3-origin line3-a))
         (origin-b (line3-origin line3-b))
         (w0 (v3:- origin-a origin-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b))))
    (if (sfzero-p denom)
        (let* ((s-c 0f0)
               (t-c (/ e c))
               (wc (v3:- w0 (v3:*s dir-b t-c))))
          (values (v3:dot wc wc) t-c s-c))
        (let* ((s-c (/ (- (* b e) (* c d)) denom))
               (t-c (/ (- (* a e) (* b d)) denom))
               (wc (v3:+ w0 (v3:- (v3:*s dir-a s-c) (v3:*s dir-b t-c)))))
          (values (v3:dot wc wc) t-c s-c)))))

(defn distance-to-line3 ((line3-a line3) (line3-b line3))
    (values single-float single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c s-c)
      (distance-squared-to-line3 line3-a line3-b)
    (values (sqrt val) t-c s-c)))

;;----------------------------------------------------------------
