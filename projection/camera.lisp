(in-package :rtg-math.projection)

;;------------------------------------------------------------

(defn frustum ((left single-float)
               (right single-float)
               (bottom single-float)
               (top single-float)
               (near single-float)
               (far single-float))
    mat4
  (let ((r-l (- right left))
        (f-n (- far near))
        (t-b (- top bottom)))
    (m4:make (/ (* 2f0 near) r-l)  0f0                   (/ (+ right left) r-l)    0f0
             0f0                   (/ (* 2f0 near) t-b)  (/ (+ top bottom) t-b)    0f0
             0f0                   0f0                   (- (/ (+ far near) f-n))  (- (/ (* 2f0 far near) f-n))
             0f0                   0f0                   -1f0                      0f0)))

;; https://unspecified.wordpress.com/2012/06/21/calculating-the-gluperspective-matrix-and-other-opengl-matrix-maths/
;; https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml
(defn perspective-radian-fov2 ((width single-float) (height single-float)
                              (near single-float) (far single-float)
                              (fov single-float))
    mat4
  ;; note: is right handed
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((aspect (/ width height))
         (fh (* near (tan fov)))
         (fw (* fh aspect)))
    (frustum (- fw) fw (- fh) fh near far)))

(defn perspective-radian-fov ((width single-float) (height single-float)
                              (near single-float) (far single-float)
                              (fov single-float))
    mat4
  ;; note: is right handed
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((d (/ 1f0 (tan (* fov 0.5f0))))
         (recip (/ 1f0 (- near far)))
         (ar (/ width height)))
    (m4:make (/ d ar)  0f0  0f0                     0f0
             0f0       d    0f0                     0f0
             0f0       0f0  (* (+ near far) recip)  (* 2 near far recip)
             0f0       0f0  -1f0                    0f0)))

;; Another reference implementation below is based on GLM
;; (defn perspective-radian-fov ((width single-float) (height single-float)
;;                                (near single-float) (far single-float)
;;                                (fov single-float))
;;     mat4
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((tan-half-fov (tan (* fov 0.5f0)))
;;          (ar (/ width height))
;;          (result (m4:0!)))
;;     (setf (m4:melm result 0 0) (/ 1f0 (* ar tan-half-fov))
;;           (m4:melm result 1 1) (/ 1f0 tan-half-fov)
;;           (m4:melm result 2 3) -1f0
;;           (m4:melm result 2 2) (- (/ (+ far near) (- far near)))
;;           (m4:melm result 3 2) (- (/ (* 2 far near) (- far near))))
;;     result))

(defn perspective ((width single-float) (height single-float)
                   (near single-float) (far single-float)
                   (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov width height near far (radians fov-degrees)))

(defn perspective2 ((width single-float) (height single-float)
                   (near single-float) (far single-float)
                   (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov2 width height near far (radians fov-degrees)))

(defn perspective-v2 ((frame-size-v2 vec2)
                      (near single-float) (far single-float)
                      (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          (radians fov-degrees)))

(defn perspective-v2-radian-fov ((frame-size-v2 vec2)
                                 (near single-float) (far single-float)
                                 (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          fov-degrees))

;;------------------------------------------------------------

(defn orthographic ((frame-width single-float) (frame-height single-float)
                    (near single-float) (far single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((left (- (/ frame-width 2.0)))
        (right (/ frame-width 2.0))
        (top (/ frame-height 2.0))
        (bottom (- (/ frame-height 2.0))))
    (m4:make
     (/ 2f0 (- right left)) 0f0                    0f0                   (- (/ (+ right left) (- right left)))
     0f0                    (/ 2f0 (- top bottom)) 0f0                   (- (/ (+ top bottom) (- top bottom)))
     0f0                    0f0                    (/ -2f0 (- far near)) (- (/ (+ far near) (- far near)))
     0f0                    0f0                    0f0                   1f0)))

(defn orthographic-v2 ((frame-size-v2 vec2)
                       (near single-float) (far single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (orthographic (x frame-size-v2) (y frame-size-v2) near far))

;;------------------------------------------------------------
