(in-package :rtg-math.vectors)

;;----------------------------------------------------------------

(docs:define-docs
  (defun swizzle
      "
`Swizzle` allows you to compose a new vector out of an existing one
by picking it's components.

Given a `vec3` called `a` = `(v! 1 2 3)`

- `(swizzle a :xy)` → (v! 1 2)
- `(swizzle a :xx)` → (v! 1 1)
- `(swizzle a :yx)` → (v! 2 1)
- `(swizzle a :yyyy)` → (v! 2 2 2 2)
- `(swizzle a :zzyy)` → (v! 3 3 2 2)
")
  (defun s~
      "
Shorthand for `swizzle`

`s~` allows you to compose a new vector out of an existing one
by picking it's components.

Given a `vec3` called `a` = `(v! 1 2 3)`

- `(s~ a :xy)` → (v! 1 2)
- `(s~ a :xx)` → (v! 1 1)
- `(s~ a :yx)` → (v! 2 1)
- `(s~ a :yyyy)` → (v! 2 2 2 2)
- `(s~ a :zzyy)` → (v! 3 3 2 2)
")
  (defun +
      "
Adds 'n' `vec2`, `vec3` or `vec4` together, returning a new vector of the
same kind.
")
  (defun +s
      "
Componentwise addition of the given scalar to the components of the  `vec2`,
`vec3` or `vec4` provided. Returns a new vector of the same kind.
")
  (defun -
      "
Subtracts 'n' `vec2`, `vec3` or `vec4` from each other, returning a new vector
of the same kind.
")
  (defun -s
      "
Componentwise subtractiong of the given scalar from the components of the
`vec2`, `vec3` or `vec4` provided. Returns a new vector of the same kind.
")
  (defun /
      "
Divides the given `vec2`, `vec3` or `vec4` by the scalar or vector provided.

If a vector is provided it must be of the same kind as `vec-a` and the division
is performed component-wise.
")
  (defun bezier
      "
{TODO}
")
  (defun cross
      "
Calculates the cross-product of 2 vectors, i.e. the vector
that lies perpendicular to them both. The resulting vector
will not be normalized.
")
  (defun distance
  "
Return the distance between 2 points defined by vectors
vector-a & vector-b. If comparing distances, use
c-distance-squared as it desnt require a sqrt and thus is
faster.")
  (defun distance-squared
  "
Returns the squared distance between 2 points defined by vectors
vector-a & vector-b")
  (defun dot
      "
Returns the dot product of the 2 given vectors.
")
  (defun face-foreward
      "
Returns `vector-a` if `(> (v3:dot vector-a vector-b) 0)` else returns
`(v3:negate vector-a)`
")
  (defun length
      "
Returns the length of a vector
If you only need to compare relative lengths then definately
stick to length-squared as the sqrt is a slow operation.
")
  (defun length-squared
      "
Finds the squared distance between 2 points defined by vectors
vector-a & vector-b
")
  (defun lerp
      "
Linearly interpolates between the two vectors by the given amount.
Returns a new vector of the same kind.
")
  (defun mix
      "
Linearly interpolates between the two vectors by the given amount.
Returns a new vector of the same kind.
")
  (defun negate
      "
Negates the given vector. Returns a new vector of the same kind.
")
  (defun normalize
      "
Normalizes the vector. Returns a new vector of the same kind.
")
  (defun perp-dot
      "
{TODO}
")
  (defun unitp
      "
Returns T is the given vector has a length eql to 1
")
  (defmacro decf
      "
Decrements the vector in 'place' by another vector of the same kind
")
  (defmacro incf
      "
Increments the vector in 'place' by another vector of the same kind
"))
