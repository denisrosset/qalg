package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra.Ring
import spire.syntax.cfor._
import spire.syntax.ring._

import algebra._

final class DenseTraceImpl[M, @sp(Double, Long) A](implicit M: MatRing[M, A]) extends Trace[M, A] {
  implicit def A: Ring[A] = M.scalar
  def trace(m: M): A = {
    val n = spire.math.min(M.nRows(m), M.nCols(m))
    var sumDiag = A.zero
    cforRange(0 until n) { i =>
      sumDiag = sumDiag + M.apply(m, i, i)
    }
    sumDiag
  }
}
