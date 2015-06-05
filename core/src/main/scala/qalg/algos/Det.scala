package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Det[M, @sp(Double, Long) A] extends Any {
  /** Computes the determinant of the given matrix. */
  def det(m: M): A
}
