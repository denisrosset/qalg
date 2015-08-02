package com.faacets.qalg
package optional

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra.{Order, PartialOrder}

import algebra._

object vecProductOrder {
  final class VecProductOrder[V, @sp(Double, Long) A](implicit V: Vec[V, A], A: Order[A]) extends PartialOrder[V] {
    def partialCompare(x: V, y: V): Double = {
      val n = V.length(x)
      if (n != V.length(y)) Double.NaN else {
        @tailrec def iter(i: Int, res: Int): Double =
          if (i < n) {
            val c = A.compare(V.apply(x, i), V.apply(y, i))
            if (res == 0 || res == c) iter(i + 1, c) else Double.NaN
          } else res.toDouble
        iter(0, 0)
      }
    }
  }
  implicit def vecProductOrder[V, @sp(Double, Long) A](implicit V: Vec[V, A], A: Order[A]): PartialOrder[V] = new VecProductOrder[V, A]
}
