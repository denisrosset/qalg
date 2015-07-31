package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PInv[M, @sp(Double, Long) A] extends Any {
  /** Computes the Moore-Penrose pseudo-inverse of the matrix. */
  def pinv(m: M): M
}
