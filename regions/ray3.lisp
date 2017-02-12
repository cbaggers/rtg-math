(in-package :rtg-math.regions.ray3)

(defstruct ray3
  (origin (v! 0 0 0) :type vec3)
  (direction (v! 1 1 1) :type vec3))

(declaim (ftype (function (ray3) vec3) origin)
         (inline origin))
(defun origin (ray3)
  (declare (optimize speed))
  (ray3-origin ray3))

(declaim (ftype (function (ray3) vec3) direction)
         (inline direction))
(defun direction (ray3)
  (declare (optimize speed))
  (ray3-origin ray3))

(defun make (origin-v3 direction-v3)
  (make-ray3 :origin origin-v3
             :direction (v3:normalize direction-v3)))

;; {TODO} add - transform taking m4
;;            - transform taking quat, translate, scale

(defun transform-m3 (matrix3 ray3)
  (make-ray3 :origin (m3:*v matrix3 (ray3-origin ray3))
             :direction (m3:*v matrix3 (ray3-direction ray3))))

(defun transform-q (quaternion ray3)
  (transform-m3 (q:to-mat3 quaternion)
                ray3))

(declaim (inline =))
(defun = (ray3-a ray3-b)
  (and (v3:= (origin ray3-a) (origin ray3-b))
       (v3:= (direction ray3-a) (direction ray3-b))))

(defun /= (ray3-a ray3-b)
  (declare (inline =))
  (not (= ray3-a ray3-b)))

;;------------------------------------------------------------

(declaim (ftype (function (ray3 ray3)
                          (values single-float single-float single-float))
                distance-squared-to-ray3))
(defun distance-squared-to-ray3 (ray3-a ray3-b)
  (declare (ray3 ray3-a ray3-b) (optimize speed))
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
    ;; ugh, really imperative
    (let ((sn 0s0) (sd 0s0) (tn 0s0) (td 0s0))
      (if (sfzero-p denom)
          (setf td c
                sd c
                sn 0s0
                tn e)
          (progn
            (setf td denom
                  sd denom
                  sn (- (* b e) (* c d))
                  tn (- (* a e) (* b d)))
            (when (< sn 0s0)
              (setf sn 0s0
                    tn e
                    td c))))
      (let* ((tnl0 (< tn 0s0))
             (tc (if tnl0 0s0 (/ tn td)))
             (sc (if tnl0
                     (if (< d 0s0)
                         0s0
                         (/ (- d) a))
                     (/ sn sd)))
             (wc (v3:+ w0 (v3:- (v3:*s dir-a sc) (v3:*s dir-b tc)))))
        (values (v3:dot wc wc)
                tc
                sc)))))


(declaim (ftype (function (ray3 ray3)
                          (values single-float single-float single-float))
                distance-to-ray3))
(defun distance-to-ray3 (ray3-a ray3-b)
  (declare (ray3 ray3-a ray3-b) (optimize speed))
  (multiple-value-bind (val t-c s-c)
      (distance-squared-to-ray3 ray3-a ray3-b)
    (declare ((single-float 0s0 #.most-positive-single-float) val))
    (values (sqrt val) t-c s-c)))

;;------------------------------------------------------------

(declaim (ftype (function (ray3 line3:line3)
                          (Values single-float single-float single-float))
                distance-squared-to-line3))
(defun distance-squared-to-line3 (ray3 line3)
  (declare (ray3 ray3) (line3:line3 line3) (optimize speed))
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
    ;; ugh, really imperative
    (if (sfzero-p denom)
        (let* ((sc 0s0)
               (tc (/ e c))
               (wc (v3:- w0 (v3:*s line-dir tc))))
          (values (v3:dot wc wc) tc sc))
        (let ((tc 0s0)
              (sc 0s0)
              (sn (- (* b e) (* c d))))
          (cond ((< sn 0s0)
                 (setf sc 0s0
                       tc (/ e c)))
                ((> sn denom)
                 (setf sc 1s0
                       tc (/ (+ e b) c)))
                (t (setf sc (/ sn denom)
                         tc (/ (- (* a e) (* b d))
                               denom))))
          (let ((wc (v3:+ w0 (v3:*s ray-dir sc) (v3:*s line-dir tc))))
            (values (v3:dot wc wc) tc sc))))))

(declaim (ftype (function (ray3 line3:line3)
                          (values single-float single-float single-float))
                distance-to-line3))
(defun distance-to-line3 (ray3 line3)
  (declare (ray3 ray3) (line3:line3 line3) (optimize speed))
  (multiple-value-bind (val t-c s-c)
      (distance-squared-to-line3 ray3 line3)
    (declare ((single-float 0s0 #.most-positive-single-float) val))
    (values (sqrt val) t-c s-c)))

;;------------------------------------------------------------

(declaim (ftype (function (ray3 vec3) (values single-float single-float))
                distance-squared-to-point))
(defun distance-squared-to-point (ray3 point-v3)
  (declare (ray3 ray3) (vec3 point-v3) (optimize speed))
  (let* ((dir (ray3-direction ray3))
         (w (v3:- point-v3 (ray3-origin ray3)))
         (proj (the single-float (v3:dot w dir))))
    (if (<= proj 0s0)
        (values (v3:dot w w) 0s0)
        (let* ((vsq (v3:dot dir dir))
               (t-c (/ proj vsq)))
          (values (- (v3:dot w w) (* t-c proj))
                  t-c)))))


(declaim (ftype (function (ray3 vec3) (values single-float single-float))
                distance-to-point))
(defun distance-to-point (ray3 point-v3)
  (declare (ray3 ray3) (vec3 point-v3) (optimize speed))
  (multiple-value-bind (val t-c)
      (distance-squared-to-point ray3 point-v3)
    (declare ((single-float 0s0 #.most-positive-single-float) val))
    (values (sqrt val) t-c)))

;;------------------------------------------------------------

(declaim (ftype (function (ray3 ray3) (values vec3 vec3))
                closest-ray-points))
(defun closest-ray-points (ray3-a ray3-b)
  (declare (ray3 ray3-a ray3-b) (optimize speed))
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
    ;; ugh, really imperative
    (let ((sn 0s0) (sd 0s0) (tn 0s0) (td 0s0))
      (if (sfzero-p denom)
          (setf td c
                sd c
                sn 0s0
                tn e)
          (progn
            (setf td denom
                  sd denom
                  sn (- (* b e) (* c d))
                  tn (- (* a e) (* b d)))
            (when (< sn 0s0)
              (setf sn 0s0
                    tn e
                    td c))))
      (let* ((tnl0 (< tn 0s0))
             (tc (if tnl0 0s0 (/ tn td)))
             (sc (if tnl0
                     (if (< d 0s0)
                         0s0
                         (/ (- d) a))
                     (/ sn sd))))
        (values (v3:+ orig-a (v3:*s dir-a sc))
                (v3:+ orig-b (v3:*s dir-b tc)))))))

;;------------------------------------------------------------

(declaim (ftype (function (ray3 vec3) vec3) closest-point))
(defun closest-point (ray3 point-v3)
  (declare (ray3 ray3) (vec3 point-v3) (optimize speed))
  (let* ((dir (ray3-direction ray3))
         (orig (ray3-origin ray3))
         (w (v3:- point-v3 orig))
         (proj (the single-float (v3:dot w dir))))
    (if (<= proj 0s0)
        orig
        (let* ((vsq (v3:dot dir dir)))
          (v3:+ orig (v3:*s dir (/ proj vsq)))))))

;;------------------------------------------------------------