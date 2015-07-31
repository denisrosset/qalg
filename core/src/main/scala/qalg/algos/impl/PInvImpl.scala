package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import algebra._

trait PInvImplFromInv[M, @sp(Double, Long) A] extends Any {
  implicit def M: MatRing[M, A]
  implicit def I: Inv[M, A]
  def pinv(m: M): M = {
    val mt = M.t(m)
    if (M.nRows(m) >= M.nCols(m))
      M.times(I.inv(M.times(mt, m)), mt)
    else
      M.times(mt, I.inv(M.times(m, mt)))
  }
}
