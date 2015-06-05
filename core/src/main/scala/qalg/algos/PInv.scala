package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PInv[M, @sp(Double, Long) A] extends Any {
  /** Computes the Moore-Penrose pseudo-inverse of the matrix. */
  def pinv(m: M): M
}

trait PInvImpl[M, @sp(Double, Long) A] extends Any {
  implicit def M: MatInRing[M, A]
  implicit def I: Inv[M, A]
  def pinv(m: M): M = {
    val mt = M.t(m)
    if (M.nRows(m) >= M.nCols(m))
      M.times(I.inv(M.times(mt, m)), mt)
    else
      M.times(mt, I.inv(M.times(m, mt)))
  }
}
