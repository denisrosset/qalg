package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

final class DenseMutableRrefImpl[M, @sp(Double, Long) A](implicit val M: MatField[M, A], val MM: MatMut[M, A], pivotA: Pivot[A], eqA: Eq[A]) extends MutableRref[M] {
  implicit def A: Field[A] = M.scalar

  def unsafeRref(m: M): RrefDecomposition[M] = {
    val used = collection.mutable.ArrayBuilder.make[Int]
    var r = 0
    cforRange(0 until m.nCols) { c =>
      if (r < m.nRows) {
        var pivotPriority = pivotA.priority(m(r, c))
        var pivot = r
        cforRange((r + 1) until m.nRows) { r1 =>
          val r1Priority = pivotA.priority(m(r1, c))
          if (r1Priority > pivotPriority) {
            pivotPriority = r1Priority
            pivot = r1
          }
        }
        if (pivotPriority != 0) { // if el is zero, skip the column c
          used += c // keep track of bound variables

          // swap current row and pivot row
          cforRange(c until m.nCols) { c1 =>
            val tmp = m(pivot, c1)
            m(pivot, c1) = m(r, c1)
            m(r, c1) = tmp
          }
          // normalize pivot row
          val f = m(r, c)
          cforRange(c until m.nCols) { c1 =>
            m(r, c1) = m(r, c1) / f
          }
          // eliminate current column
          cforRange(0 until m.nRows) { r1 =>
            if (r1 != r) {
              val g = m(r1, c)
              cforRange(c until m.nCols) { c1 =>
                m(r1, c1) = m(r1, c1) - g * m(r, c1)
              }
            }
          }
          r += 1
        } else // set zero terms to exact zero (used for floating point)
          cforRange(r until m.nRows) { r1 =>
            m(r, c) = A.zero
          }
      }
    }
    new RrefDecomposition[M] {
      def reduced = m
      val basis = used.result
    }
  }
}
