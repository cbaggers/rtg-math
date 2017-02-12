(in-package :rtg-math.regions.line-segment3)

;; A line segment in ℝ3

(defstruct line-segment3
  (end-point0 (v! 0 0 0) :type vec3)
  (offset (v! 0 1 0) :type vec3))

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

(defn distance-squared-to-line-seg3 ((line-seg-a line-segment3)
                                     (line-seg-b line-segment3))
    (values (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float)
            (single-float 0f0 #.most-positive-single-float))
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
    ;; compute difference vector and distance squared
    (let ((wc (v3:+ w0
                    (v3:*s (direction line-seg-a) sc)
                    (v3:*s (direction line-seg-b) tc))))
      (values (v3:dot wc wc)
              tc
              sc))))

;;----------------------------------------------------------------
