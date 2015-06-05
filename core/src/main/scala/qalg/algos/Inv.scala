package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Inv[M, @sp(Double, Long) A] extends Any {
  /** Computes the inverse of the given matrix. */
  def inv(m: M): M
}
