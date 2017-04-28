# rtg-math

This system provides a selection of the math routines most commonly needed for making realtime graphics in lisp.

The library provides:

- 2, 3 & 4 component vectors (vec2, vec3, vec4), including support for:
 - unit, zero & equality predicates
 - addition, subtraction, multiplication & divising of vectors against each other
 - addition, subtraction, multiplication & divising of scalars against vectors
 - length, length-squared, distance, distance-squared
 - dot, absolute-dot & cross products
 - normalize
 - lerp, bezier & spline
 - Consing and non-consing apis
 - Optional generic API

- 3x3 & 4x4 matrices (`mat3`, `mat4`), including support for:
 - identity, zero & equality predicates
 - component-wise addition & subtraction of matrices
 - multiplication of matrices with matrices and matrices with vectors
 - transpose, adjoint, determinate, trace
 - construction from angle (seperately or as a vec3), axis-angle, scale, or induvidual components
 - inverse & affine-inverse
 - extraction of rows or columns as vectors
 - Consing and non-consing apis
 - Optional generic API

- Quaternions
 - addition, subtraction, multiplication & divising of quaternions against each other
 - conversion from & to 3x3 matrices, axis-angle pairs, look-at position & more
 - magnitude, norm, normalize, conjugate, inverse
 - lerp & slerp

- Basic support for spherical and polar coordinates

And more for all categories.

## CHANGELOG

### 2017-04-28
- all lerp functions are stable, stable-lerp is deprecated & will be removed in a future release

### 2017-04-11

- Fixes to the projection matrices. Are also now typed

### 2017-02-13

- Add non-consing functions for matrix3 & matrix4

### 2017-02-12

- Started Changelog. Sorry we havent had this before.
- Most of vector, matrix, quaternion API is now typed
- Where possible the optimization for the public is set at (speed 3) (safety 1) (debug 1)
- I have started adding a non-consing version of the apis. The nicknames for these packages is the regular nickname with `-n` on the end. Therefore:
 - rtg-math.vector2.non-consing has the nickname v2-n
 - rtg-math.vector3.non-consing has the nickname v3-n
 - rtg-math.vector4.non-consing has the nickname v4-n
 - rtg-math.matrix2.non-consing has the nickname m2-n
 - rtg-math.matrix3.non-consing has the nickname m3-n
 - rtg-math.quaternions.non-consing has the nickname q-n
- q:qconjugate is now deprecated in favor q:conjugate.
- directory structure normalized a bit. Now vectors/matrices/quaternions
- fixed spelling mistake in m3: determinate -> determinant
- added the regions api support lines, rays, line-segments & axis-aligned boxes in ℝ3
