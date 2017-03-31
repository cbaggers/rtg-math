(in-package :rtg-math.region.ray3)

;; A ray of infinite length out from a point in ℝ3

(defn-inline origin ((ray3 ray3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (ray3-origin ray3))

(defn direction ((ray3 ray3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (ray3-direction ray3))

(defn-inline make ((origin-v3 vec3) (direction-v3 vec3)) ray3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-ray3 :origin origin-v3
             :direction (v3:normalize direction-v3)))

;;------------------------------------------------------------

(defn transform-m3 ((matrix3 mat3) (ray3 ray3)) ray3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (m3:*v matrix3 (origin ray3))
        (m3:*v matrix3 (direction ray3))))

(defn transform-q ((quat quaternion) (ray3 ray3)) ray3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (q:rotate (origin ray3) quat)
        (q:rotate (direction ray3) quat)))

;;------------------------------------------------------------

(defn-inline = ((ray3-a ray3) (ray3-b ray3)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (v3:= (origin ray3-a) (origin ray3-b))
       (v3:= (direction ray3-a) (direction ray3-b))))

(defn /= ((ray3-a ray3) (ray3-b ray3)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline =))
  (not (= ray3-a ray3-b)))

;;------------------------------------------------------------

(defn distance-squared-to-ray3 ((ray3-a ray3) (ray3-b ray3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((orig-a (ray3-origin ray3-a))
         (orig-b (ray3-origin ray3-b))
         (dir-a (ray3-direction ray3-a))
         (dir-b (ray3-direction ray3-b))
         ;; compute intermediate parameters
         (w0 (v3:- orig-a orig-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b)))
         ;; parameters to compute s_c, t_c
         (sn 0f0) (sd 0f0) (tn 0f0) (td 0f0))
    ;; if denom is zero, try finding closest point on ray1 to origin0
    (if (sfzero-p denom)
        ;; clamp s_c to 0
        (setf td c
              sd c
              sn 0f0
              tn e)
        (progn
          ;; clamp s_c within [0,+inf]
          (setf td denom
                sd denom
                sn (- (* b e) (* c d))
                tn (- (* a e) (* b d)))
          (when (< sn 0f0)
            ;; clamp s_c to 0
            (setf sn 0f0
                  tn e
                  td c))))
    ;;
    (let* ((tnl0 (< tn 0f0))
           (tc (if tnl0 0f0 (/ tn td)))
           (sc (if tnl0
                   (if (< d 0f0)
                       0f0
                       (/ (- d) a))
                   (/ sn sd)))
           (wc (v3:+ w0 (v3:- (v3:*s dir-a sc) (v3:*s dir-b tc)))))
      (values (v3:dot wc wc)
              tc
              sc))))

(defn distance-to-ray3 ((ray3-a ray3) (ray3-b ray3))
    (values single-float single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c s-c) (distance-squared-to-ray3 ray3-a ray3-b)
    (values (sqrt val) t-c s-c)))

;;------------------------------------------------------------

(defn distance-squared-to-line3 ((ray3 ray3) (line3 line3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((ray-orig (ray3-origin ray3))
         (line-orig (line3:origin line3))
         (ray-dir (ray3-direction ray3))
         (line-dir (line3:direction line3))
         ;; compute intermediate parameters
         (w0 (v3:- ray-orig line-orig))
         (a (v3:dot ray-dir ray-dir))
         (b (v3:dot ray-dir line-dir))
         (c (v3:dot line-dir line-dir))
         (d (v3:dot ray-dir w0))
         (e (v3:dot line-dir w0))
         (denom (- (* a c) (* b b))))
    ;; if denom is zero, try finding closest point on ray1 to origin0
    (if (sfzero-p denom)
        (let* ((sc 0f0)
               (tc (/ e c))
               (wc (v3:- w0 (v3:*s line-dir tc))))
          (values (v3:dot wc wc) tc sc))
        (let ((tc 0f0)
              (sc 0f0)
              ;; clamp s_c within [0,1]
              (sn (- (* b e) (* c d))))
          (cond ((< sn 0f0)
                 ;; clamp s_c to 0
                 (setf sc 0f0
                       tc (/ e c)))
                ((> sn denom)
                 ;; clamp s_c to 1
                 (setf sc 1f0
                       tc (/ (+ e b) c)))
                (t (setf sc (/ sn denom)
                         tc (/ (- (* a e) (* b d))
                               denom))))
          (let ((wc (v3:+ w0 (v3:*s ray-dir sc) (v3:*s line-dir tc))))
            (values (v3:dot wc wc) tc sc))))))

(defn distance-to-line3 ((ray3 ray3) (line3 line3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c s-c)
      (distance-squared-to-line3 ray3 line3)
    (declare ((single-float 0f0 #.most-positive-single-float) val))
    (values (sqrt val) t-c s-c)))

;;------------------------------------------------------------

(defn distance-squared-to-point ((ray3 ray3) (point-v3 vec3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir (ray3-direction ray3))
         (w (v3:- point-v3 (ray3-origin ray3)))
         (proj (the single-float (v3:dot w dir))))
    (if (<= proj 0f0)
        (values (v3:dot w w) 0f0)
        (let* ((vsq (v3:dot dir dir))
               (t-c (/ proj vsq)))
          (values (- (v3:dot w w) (* t-c proj))
                  t-c)))))

(defn distance-to-point ((ray3 ray3) (point-v3 vec3))
    (values single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c)
      (distance-squared-to-point ray3 point-v3)
    (declare ((single-float 0f0 #.most-positive-single-float) val))
    (values (sqrt val) t-c)))

;;------------------------------------------------------------

(defn closest-ray-points ((ray3-a ray3) (ray3-b ray3)) (values vec3 vec3)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((orig-a (ray3-origin ray3-a))
         (orig-b (ray3-origin ray3-b))
         (dir-a (ray3-direction ray3-a))
         (dir-b (ray3-direction ray3-b))
         ;; compute intermediate parameters
         (w0 (v3:- orig-a orig-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b))))
    (let ((sn 0f0) (sd 0f0) (tn 0f0) (td 0f0))
      (if (sfzero-p denom)
          (setf td c
                sd c
                sn 0f0
                tn e)
          (progn
            (setf td denom
                  sd denom
                  sn (- (* b e) (* c d))
                  tn (- (* a e) (* b d)))
            (when (< sn 0f0)
              (setf sn 0f0
                    tn e
                    td c))))
      (let* ((tnl0 (< tn 0f0))
             (tc (if tnl0 0f0 (/ tn td)))
             (sc (if tnl0
                     (if (< d 0f0)
                         0f0
                         (/ (- d) a))
                     (/ sn sd))))
        (values (v3:+ orig-a (v3:*s dir-a sc))
                (v3:+ orig-b (v3:*s dir-b tc)))))))

;;------------------------------------------------------------

(defn closest-point ((ray3 ray3) (point-v3 vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir (ray3-direction ray3))
         (orig (ray3-origin ray3))
         (w (v3:- point-v3 orig))
         (proj (the single-float (v3:dot w dir))))
    (if (<= proj 0f0)
        orig
        (let* ((vsq (v3:dot dir dir)))
          (v3:+ orig (v3:*s dir (/ proj vsq)))))))

;;------------------------------------------------------------
