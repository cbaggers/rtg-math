(in-package :rtg-math.types)

(deftype vec2 ()
  '(simple-array single-float (2)))
(deftype vec3 ()
  '(simple-array single-float (3)))
(deftype vec4 ()
  '(simple-array single-float (4)))

(deftype ivec2 ()
  '(simple-array (signed-byte 32) (2)))
(deftype ivec3 ()
  '(simple-array (signed-byte 32) (3)))
(deftype ivec4 ()
  '(simple-array (signed-byte 32) (4)))

(deftype uvec2 ()
  '(simple-array (unsigned-byte 32) (2)))
(deftype uvec3 ()
  '(simple-array (unsigned-byte 32) (3)))
(deftype uvec4 ()
  '(simple-array (unsigned-byte 32) (4)))

(deftype int8-vec2 ()
  '(simple-array (signed-byte 8) (2)))
(deftype int8-vec3 ()
  '(simple-array (signed-byte 8) (3)))
(deftype int8-vec4 ()
  '(simple-array (signed-byte 8) (4)))

(deftype uint8-vec2 ()
  '(simple-array (unsigned-byte 8) (2)))
(deftype uint8-vec3 ()
  '(simple-array (unsigned-byte 8) (3)))
(deftype uint8-vec4 ()
  '(simple-array (unsigned-byte 8) (4)))

(deftype mat3 ()
  '(simple-array single-float (9)))
(deftype mat4 ()
  '(simple-array single-float (16)))

(deftype quaternion ()
  '(simple-array single-float (4)))

(defstruct line3
  (origin (make-array 3 :element-type 'single-float :initial-element 0f0)
          :type vec3)
  (direction (make-array 3 :element-type 'single-float :initial-element 1f0)
             :type vec3))

(defstruct ray3
  (origin (make-array 3 :element-type 'single-float :initial-element 0f0)
          :type vec3)
  (direction (make-array 3 :element-type 'single-float :initial-element 1f0)
             :type vec3))

(defstruct line-segment3
  (end-point0 (make-array 3 :element-type 'single-float :initial-element 0f0)
              :type vec3)
  (offset (make-array 3 :element-type 'single-float
                      :initial-contents '(0f0 1f0 0f0))
          :type vec3))

(defstruct axis-aligned-box
  (minima (make-array 3 :element-type 'single-float :initial-element -1f0)
          :type vec3)
  (maxima (make-array 3 :element-type 'single-float :initial-element 1f0)
          :type vec3))
