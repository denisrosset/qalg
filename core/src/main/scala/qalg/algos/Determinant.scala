package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Determinant[M, @sp(Double, Long) A] extends Any {
  /** Computes the determinant of the given matrix. */
  def determinant(m: M): A
}
