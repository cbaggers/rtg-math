(in-package :rtg-math.region.line-segment3)

;; A line segment in ℝ3

(defn line-segment3-end-point1 ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3:+ (line-segment3-end-point0 line-seg3)
        (line-segment3-offset line-seg3)))

(defn-inline end-point0 ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line-segment3-end-point0 line-seg3))

(defn-inline end-point1 ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line-segment3-end-point1 line-seg3))

(defn direction ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line-segment3-offset line-seg3))

(defn make ((end-point0 vec3) (end-point1 vec3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-line-segment3 :end-point0 end-point0
                      :offset (v3:- end-point1 end-point0)))

(defn make-from-point-offset ((point-v3 vec3) (offset-v3 vec3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-line-segment3 :end-point0 point-v3
                      :offset offset-v3))

;;----------------------------------------------------------------

(defn-inline = ((line-seg3-a line-segment3) (line-seg3-b line-segment3))
    boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (v3:= (end-point0 line-seg3-a) (end-point0 line-seg3-b))
       (v3:= (direction line-seg3-a) (direction line-seg3-b))))

(defn /= ((line-seg3-a line-segment3) (line-seg3-b line-segment3))
    boolean
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline =))
  (not (= line-seg3-a line-seg3-b)))

;;------------------------------------------------------------

(defn transform-m3 ((matrix3 mat3) (line-seg3 line-segment3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (m3:*v matrix3 (end-point0 line-seg3))
        (m3:*v matrix3 (direction line-seg3))))

(defn transform-q ((quat quaternion) (line-seg3 line-segment3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (q:rotate (end-point0 line-seg3) quat)
        (q:rotate (direction line-seg3) quat)))

;;----------------------------------------------------------------

(defn length-squared ((line-seg3 line-segment3))
    (single-float 0f0 #.most-positive-single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3:length-squared (line-segment3-offset line-seg3)))

(defn length ((line-seg3 line-segment3))
    (single-float 0f0 #.most-positive-single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3:length (line-segment3-offset line-seg3)))

;;----------------------------------------------------------------

(defn %seg-to-seg-internals ((line-seg-a line-segment3)
                             (line-seg-b line-segment3))
    (values vec3
            vec3
            vec3
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  "Returns: dir-a dir-b w0 tc sc"
  ;;
  (let* ((orig-a (end-point0 line-seg-a))
         (orig-b (end-point0 line-seg-b))
         (dir-a (direction line-seg-a))
         (dir-b (direction line-seg-b))
         ;; compute intermediate parameters
         (w0 (v3:- orig-a orig-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b)))
         ;; parameters to compute s_c, t_c
         (sn 0f0) (sd 0f0) (tn 0f0) (td 0f0)
         ;; return vals
         (tc 0f0) (sc 0f0))
    ;; if denom is zero, try finding closest point on segment1 to origin0
    (if (sfzero-p denom)
        ;;clamp s_c to 0
        (setf td c
              sd c
              sn 0f0
              tn e)
        ;; clamp s_c within [0,1]
        (progn
          (setf td denom
                sd denom
                sn (- (* b e) (* c d))
                tn (- (* a e) (* b d)))
          (if (< sn 0f0)
              ;;clamp s_c to 0
              (setf sn 0f0
                    tn e
                    td c)
              (when (> sn sd)
                ;;clamp s_c to 1
                (setf sn sd
                      tn (+ e b)
                      td c)))))
    ;;
    (cond
      ((< tn 0f0)
       ;; clamp s_c within [0,1]
       ;; clamp t_c to 0
       (setf tc 0f0)
       (cond
         ((< (- d) 0f0)
          ;; clamp s_c to 0
          (setf sc 0f0))
         ((> (- d) a)
          ;; clamp s_c to 1
          (setf sc 1f0))
         (t
          (setf sc (/ (- d) a)))))
      ((> tn td)
       ;;clamp t_c to 1
       (setf tc 1f0)
       (cond
         ((< (+ (- d) b) 0f0)
          ;; clamp s_c to 0
          (setf sc 0f0))
         ((> (+ (- d) b) a)
          ;; clamp s_c to 1
          (setf sc 1f0))
         (t
          (setf sc (/ (+ (- d) b) a)))))
      (t
       (setf tc (/ tn td)
             sc (/ sn sd))))
    ;; return common componants
    (values dir-a dir-b w0 tc sc)))

(defn distance-squared-to-line-seg3 ((line-seg-a line-segment3)
                                     (line-seg-b line-segment3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (dir-a dir-b w0 tc sc)
      (%seg-to-seg-internals line-seg-a line-seg-b)
    ;; compute difference vector and distance squared
    (let ((wc (v3:+ w0 (v3:*s dir-a sc) (v3:*s dir-b tc))))
      (values (v3:dot wc wc)
              tc
              sc))))

(defn distance-to-line-seg3 ((line-seg-a line-segment3)
                             (line-seg-b line-segment3))
    (values single-float single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c s-c)
      (distance-squared-to-line-seg3 line-seg-a line-seg-b)
    (values (sqrt val) t-c s-c)))

;;----------------------------------------------------------------

(defn %seg-to-ray-internals ((line-seg3 line-segment3)
                                (ray3 ray3))
    (values vec3
            vec3
            vec3
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  "Returns: dir-a dir-b w0 tc sc"
  (let* ((orig-a (end-point0 line-seg3))
         (orig-b (ray3:origin ray3))
         (dir-a (direction line-seg3))
         (dir-b (ray3:direction ray3))
         ;; compute intermediate parameters
         (w0 (v3:- orig-a orig-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b)))
         ;; parameters to compute s_c, t_c
         (sn 0f0) (sd 0f0) (tn 0f0) (td 0f0)
         ;; return vals
         (tc 0f0) (sc 0f0))
    ;; if denom is zero, try finding closest point on segment1 to origin0
    (if (sfzero-p denom)
        ;;clamp s_c to 0
        (setf td c
              sd c
              sn 0f0
              tn e)
        ;; clamp s_c within [0,1]
        (progn
          (setf td denom
                sd denom
                sn (- (* b e) (* c d))
                tn (- (* a e) (* b d)))
          (if (< sn 0f0)
              ;;clamp s_c to 0
              (setf sn 0f0
                    tn e
                    td c)
              (when (> sn sd)
                ;;clamp s_c to 1
                (setf sn sd
                      tn (+ e b)
                      td c)))))
    ;;
    (if (< tn 0f0)
        (progn
          ;; clamp s_c within [0,1]
          ;; clamp t_c to 0
          (setf tc 0f0)
          (cond
            ((< (- d) 0f0)
             ;; clamp s_c to 0
             (setf sc 0f0))
            ((> (- d) a)
             ;; clamp s_c to 1
             (setf sc 1f0))
            (t
             (setf sc (/ (- d) a)))))
        (setf tc (/ tn td)
              sc (/ sn sd)))
    ;; compute difference vector and distance squared
    (values dir-a dir-b w0 tc sc)))

(defn distance-squared-to-ray3 ((line-seg3 line-segment3)
                                (ray3 ray3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;;
  (multiple-value-bind (dir-a dir-b w0 tc sc)
      (%seg-to-ray-internals line-seg3 ray3)
    ;; compute difference vector and distance squared
    (let ((wc (v3:+ w0 (v3:*s dir-a sc) (v3:*s dir-b tc))))
      (values (v3:dot wc wc)
              tc
              sc))))

(defn distance-to-ray3 ((line-seg3 line-segment3) (ray3 ray3))
    (values single-float single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c s-c) (distance-squared-to-ray3 line-seg3 ray3)
    (values (sqrt val) t-c s-c)))

;;----------------------------------------------------------------

(defn distance-squared-to-line3 ((line-seg3 line-segment3)
                                 (line3 line3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;;
  (let* ((orig-a (end-point0 line-seg3))
         (orig-b (line3:origin line3))
         (dir-a (direction line-seg3))
         (dir-b (line3:direction line3))
         ;; compute intermediate parameters
         (w0 (v3:- orig-a orig-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b))))
    ;; if denom is zero, try finding closest point on segment1 to origin0
    (if (sfzero-p denom)
        (let* ((sc 0f0)
               (tc (/ e c))
               (wc (v3:- w0 (v3:*s dir-b tc))))
          (values (v3:dot wc wc)
                  tc
                  sc))
        ;; clamp s_c within [0,1]
        (let ((tc 0f0)
              (sc 0f0)
              (sn (- (* b e) (* c d))))
          (cond
            ((< sn 0f0)
             ;; clamp s_c to 0
             (setf sc 0f0
                   tc (/ e c)))
            ((> sn denom)
             (setf sc 1f0
                   tc (/ (+ e b) c)))
            (t
             (setf sc (/ sn denom)
                   tc (/ (- (* a e) (* b d))
                         denom))))
          ;; compute difference vector and distance squared
          (let ((wc (v3:+ w0 (v3:*s dir-a sc) (v3:*s dir-b tc))))
            (values (v3:dot wc wc)
                    tc
                    sc))))))

(defn distance-to-line3 ((line-seg3 line-segment3) (line3 line3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c s-c)
      (distance-squared-to-line3 line-seg3 line3)
    (declare ((single-float 0f0 #.most-positive-single-float) val))
    (values (sqrt val) t-c s-c)))

;;----------------------------------------------------------------

(defn distance-squared-to-point ((line-seg3 line-segment3) (point-v3 vec3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((w (v3:- point-v3 (end-point0 line-seg3)))
         (dir (direction line-seg3))
         (proj (v3:dot w dir)))
    ;;
    (if (<= proj 0f0)
        ;; endpoint 0 is closest point
        (values (v3:dot w w) 0f0)
        ;;
        (let ((vsq (v3:dot dir dir)))
          (if (>= proj vsq)
              ;; endpoint 1 is closest point
              (values (+ (- (v3:dot w w) (* 2f0 proj))
                         vsq)
                      1f0)
              ;; otherwise somewhere else in segment
              (let ((tc (/ proj vsq)))
                (values (- (v3:dot w w) (* tc proj))
                        tc)))))))

(defn distance-to-point ((line-seg3 line-segment3) (point-v3 vec3))
    (values single-float single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (val t-c)
      (distance-squared-to-point line-seg3 point-v3)
    (declare ((single-float 0f0 #.most-positive-single-float) val))
    (values (sqrt val) t-c)))

;;------------------------------------------------------------

(defn closest-line-segment-points ((line-seg-a line-segment3)
                                   (line-seg-b line-segment3))
    (values vec3 vec3)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (dir-a dir-b w0 tc sc)
      (%seg-to-seg-internals line-seg-a line-seg-b)
    (declare (ignore w0))
    (values (v3:+ (end-point0 line-seg-a) (v3:*s dir-a sc))
            (v3:+ (end-point0 line-seg-b) (v3:*s dir-b tc)))))

;;------------------------------------------------------------

(defn closest-ray-points ((line-seg-a line-segment3)
                          (ray3 ray3))
    (values vec3 vec3)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (multiple-value-bind (dir-a dir-b w0 tc sc)
      (%seg-to-ray-internals line-seg-a ray3)
    (declare (ignore w0))
    (values (v3:+ (end-point0 line-seg-a) (v3:*s dir-a sc))
            (v3:+ (ray3:origin ray3) (v3:*s dir-b tc)))))

;;------------------------------------------------------------

(defn closest-line-points ((line-seg3 line-segment3)
                           (line3 line3))
    (values vec3 vec3)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;;
  (let* ((orig-a (end-point0 line-seg3))
         (orig-b (line3:origin line3))
         (dir-a (direction line-seg3))
         (dir-b (line3:direction line3))
         ;; compute intermediate parameters
         (w0 (v3:- orig-a orig-b))
         (a (v3:dot dir-a dir-a))
         (b (v3:dot dir-a dir-b))
         (c (v3:dot dir-b dir-b))
         (d (v3:dot dir-a w0))
         (e (v3:dot dir-b w0))
         (denom (- (* a c) (* b b))))
    ;; if denom is zero, try finding closest point on segment1 to origin0
    (if (sfzero-p denom)
        (values orig-a
                (v3:+ orig-b (v3:*s dir-b (/ e c))))
        ;; clamp s_c within [0,1]
        (let ((tc 0f0)
              (sc 0f0)
              (sn (- (* b e) (* c d))))
          (cond
            ((< sn 0f0)
             ;; clamp s_c to 0
             (setf sc 0f0
                   tc (/ e c)))
            ((> sn denom)
             (setf sc 1f0
                   tc (/ (+ e b) c)))
            (t
             (setf sc (/ sn denom)
                   tc (/ (- (* a e) (* b d))
                         denom))))
          ;; compute difference vector and distance squared
          (values (v3:+ orig-a (v3:*s dir-a sc))
                  (v3:+ orig-b (v3:*s dir-b tc)))))))

;;------------------------------------------------------------

(defn closest-point ((line-seg3 line-segment3) (point-v3 vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir (direction line-seg3))
         (orig (end-point0 line-seg3))
         (w (v3:- point-v3 orig))
         (proj (v3:dot w dir)))
    (if (<= proj 0f0)
        orig
        (let* ((vsq (v3:dot dir dir)))
          (if (>= proj vsq)
              ;; endpoint 1 is closest point
              (v3:+ orig dir)
              ;; else somewhere else in segment
              (v3:+ orig (v3:*s dir (/ proj vsq))))))))

;;------------------------------------------------------------
