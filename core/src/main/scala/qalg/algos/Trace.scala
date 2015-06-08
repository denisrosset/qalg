package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra.Ring
import spire.syntax.cfor._
import spire.syntax.ring._

import algebra._

trait Trace[M, @sp(Double, Long) A] extends Any {
  /** Computes the trace of the given matrix. */
  def trace(m: M): A
}

object Trace {
  implicit def fromAlg[M, @sp(Double, Long) A](implicit ev: AlgMVR[M, _, A]): Trace[M, A] = ev.MTrace
}

final class TraceImpl[M, @sp(Double, Long) A](implicit M: MatInRing[M, A]) extends Trace[M, A] {
  implicit def A: Ring[A] = M.A
  def trace(m: M): A = {
    val n = spire.math.min(M.nRows(m), M.nCols(m))
    var sumDiag = A.zero
    cforRange(0 until n) { i =>
      sumDiag = sumDiag + M.apply(m, i, i)
    }
    sumDiag
  }
}
