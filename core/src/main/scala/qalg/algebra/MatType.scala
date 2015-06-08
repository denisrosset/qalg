package com.faacets.qalg
package algebra

import scala.{specialized => sp}

/** Trait where the type parameters A, V, M are reversed, so that the implicit
  * search can retrieve the type of the matrices from the type of vectors.
  */
trait MatType[@sp(Double, Long) A, V, M] extends Any
