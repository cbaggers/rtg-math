(in-package :rtg-math.region.axis-aligned-box.non-consing)

;; make a non-consing axis-aligned-box package containing merge-point
(defn merge-point ((aab axis-aligned-box) (point-v3 vec3)) axis-aligned-box
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
          (setf (z maxa) (z point-v3))))
    aab))
