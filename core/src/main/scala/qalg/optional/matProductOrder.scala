package com.faacets.qalg
package optional

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra.{Order, PartialOrder}

import algebra._

object matProductOrder {
  final class MatProductOrder[M, @sp(Double, Long) A](implicit M: Mat[M, A], A: Order[A]) extends PartialOrder[M] {
    def partialCompare(x: M, y: M): Double = {
      val nR = M.nRows(x)
      val nC = M.nCols(x)
      if (nR != M.nRows(y) || nC != M.nCols(y)) Double.NaN else {
        @tailrec def iter(r: Int, c: Int, res: Int): Double =
          if (r < nR && c < nC) {
            val cm = A.compare(M.apply(x, r, c), M.apply(y, r, c))
            if (res == 0 || res == cm) {
              if (r == nR - 1) iter(0, c + 1, cm) else iter(r + 1, c, cm)
            } else Double.NaN
          } else res.toDouble
        iter(0, 0, 0)
      }
    }
  }
  implicit def matProductOrder[M, @sp(Double, Long) A](implicit M: Mat[M, A], A: Order[A]): PartialOrder[M] = new MatProductOrder[M, A]
}
