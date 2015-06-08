package com.faacets.qalg
package algebra

import scala.{specialized => sp}

/** Trait where the type parameters V, M are reversed, so that the implicit
  * search can retrieve the type of the matrices from the type of vectors.
  */
trait MatType[V, M, @sp(Double, Long) A] extends Any
