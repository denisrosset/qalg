package com.faacets.qalg
package algos

import scala.{specialized => sp}

trait Rank[M] extends Any {
  /** Computes the rank of the given matrix. */
  def rank(m: M): Int
}

object Rank {
  implicit def fromAlg[M](implicit ev: AlgMVF[M, _, _]): Rank[M] = ev.MRank
}
