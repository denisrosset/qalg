package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Rank[M, @sp(Double, Long) A] extends Any {
  /** Computes the rank of the given matrix. */
  def rank(m: M): A
}
