(in-package :rtg-math.base-vectors)

(docs:define-docs
  (defun v!
      "
Create a vec2, vec3 or vec4. The kind of vector returned by this function
depends on what is passed as the arguments.

It can take any number of the following..

- floats
- integers
- vec2
- vec3
- vec4

so long as the total number of components is 2, 3 or 4. So any of the following
examples are valid:

- `(v! 1 2 3 4)`
- `(v! 1.0 2 3.0 4)`
- `(v! 1 (v! 2 3) 4)`
- `(v! (v! 1 2) (v! 3 4))`
- `(v! (v! 1 2 3) 4.0)`
")
  (defun v!bool
      "
Create a vector of 2, 3 or 4 booleans out of booleans or other boolean vectors
")
  (defun v!byte
      "
 a vector of 2, 3 or 4 bytes out of bytes or other byte vectors
")
  (defun v!double
      "
Create a vector of 2, 3 or 4 doubles out of doubles or other double vectors
")
  (defun v!int
      "
Create a vector of 2, 3 or 4 ints out of ints or other int vectors
")
  (defun v!int8
      "
Create a vector of 2, 3 or 4 int8s out of int8s or other int8 vectors
")
  (defun v!ubyte
      "
Create a vector of 2, 3 or 4 ubytes out of ubytes or other ubyte vectors
")
  (defun v!uint
      "
Create a vector of 2, 3 or 4 uints out of uints or other uint vectors
")
  (defun v!uint8
      "
Create a vector of 2, 3 or 4 uint8s out of uint8s or other uint8 vectors
")
  (defun v2!
      "
Create vec2 from 1 or 2 floats or ints
")
  (defun v2!byte
      "
Create vector of two bytes from 1 or 2 bytes
")
  (defun v2!double
      "
Create vector of two doubles from 1 or 2 doubles
")
  (defun v2!int
      "
Create vector of two ints from 1 or 2 ints
")
  (defun v2!int8
      "
Create vector of two int8s from 1 or 2 int8s
")
  (defun v2!short
      "
Create vector of two shorts from 1 or 2 shorts
")
  (defun v2!ubyte
      "
Create vector of two ubytes from 1 or 2 ubytes
")
  (defun v2!uint
      "
Create vector of two uints from 1 or 2 uints
")
  (defun v2!uint8
      "
Create vector of two uint8s from 1 or 2 uint8s
")
  (defun v2!ushort
      "
Create vector of two ushorts from 1 or 2 ushorts
")
  (defun v3!
      "
Create a vec3 from 1, 2 or 3 floats or ints.
")
  (defun v3!byte
      "
Creates a vector of 3 bytes from 1, 2 or 3 bytes
")
  (defun v3!double
      "
Creates a vector of 3 doubles from 1, 2 or 3 doubles
")
  (defun v3!int
      "
Creates a vector of 3 ints from 1, 2 or 3 ints
")
  (defun v3!int8
      "
Creates a vector of 3 int8s from 1, 2 or 3 int8s
")
  (defun v3!short
      "
Creates a vector of 3 shorts from 1, 2 or 3 shorts
")
  (defun v3!ubyte
      "
Creates a vector of 3 ubytes from 1, 2 or 3 ubytes
")
  (defun v3!uint
      "
Creates a vector of 3 uints from 1, 2 or 3 uints
")
  (defun v3!uint8
      "
Creates a vector of 3 uint8s from 1, 2 or 3 uint8s
")
  (defun v3!ushort
      "
Creates a vector of 3 ushorts from 1, 2 or 3 ushorts
")
  (defun v4!
      "
Create a vec4 from 1, 2, 3 or 4 floats or ints
")
  (defun v4!byte
      "
Creates a vector of 4 bytes from 1, 2, 3 or 4 bytes
")
  (defun v4!double
      "
Creates a vector of 4 doubles from 1, 2, 3 or 4 doubles
")
  (defun v4!int
      "
Creates a vector of 4 ints from 1, 2, 3 or 4 ints
")
  (defun v4!int8
      "
Creates a vector of 4 int8s from 1, 2, 3 or 4 int8s
")
  (defun v4!short
      "
Creates a vector of 4 shorts from 1, 2, 3 or 4 shorts
")
  (defun v4!ubyte
      "
Creates a vector of 4 ubytes from 1, 2, 3 or 4 ubytes
")
  (defun v4!uint
      "
Creates a vector of 4 uints from 1, 2, 3 or 4 uints
")
  (defun v4!uint8
      "
Creates a vector of 4 uint8s from 1, 2, 3 or 4 uint8s
")
  (defun v4!ushort
      "
Creates a vector of 4 ushorts from 1, 2, 3 or 4 ushorts
"))
