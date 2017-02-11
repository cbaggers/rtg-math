(in-package #:rtg-math.quaternions.non-consing)

;;----------------------------------------------------------------

(defn-inline w ((quat quaternion)) single-float
  "Returns the w component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (aref quat 0))

(defn-inline x ((quat quaternion)) single-float
  "Returns the x component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (aref quat 1))

(defn-inline y ((quat quaternion)) single-float
  "Returns the y component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (aref quat 2))

(defn-inline z ((quat quaternion)) single-float
  "Returns the z component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (aref quat 3))

(defn-inline (setf w) ((value single-float)
                       (quat quaternion)) single-float
  "Returns the w component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (aref quat 0) value))

(defn-inline (setf x) ((value single-float)
                       (quat quaternion)) single-float
  "Returns the x component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (aref quat 1) value))

(defn-inline (setf y) ((value single-float)
                       (quat quaternion)) single-float
  "Returns the y component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (aref quat 2) value))

(defn-inline (setf z) ((value single-float)
                       (quat quaternion)) single-float
  "Returns the z component of the quaternion"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (aref quat 3) value))

;;----------------------------------------------------------------

(defn dot ((quat-a quaternion) (quat-b quaternion)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  "return the dot product of the quat-a and quat-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:* (w quat-a) (w quat-b))
        (cl:* (x quat-a) (x quat-b))
        (cl:* (y quat-a) (y quat-b))
        (cl:* (z quat-a) (z quat-b))))

;;----------------------------------------------------------------
