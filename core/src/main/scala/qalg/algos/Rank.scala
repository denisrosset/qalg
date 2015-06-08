package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Rank[M] extends Any {
  /** Computes the rank of the given matrix. */
  def rank(m: M): Int
}
