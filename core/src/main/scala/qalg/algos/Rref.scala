package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

trait RrefDecomposition[M] extends Any { self =>
  def reduced: M
  def basis: Array[Int]
  def rank: Int = basis.length
  def map[M1](f: M => M1) = new RrefDecomposition[M1] {
    val reduced = f(self.reduced)
    def basis = self.basis
  }
}

trait Rref[M] extends Any {
  def rref(m: M): RrefDecomposition[M]
}

object Rref {
  implicit def fromAlg[M](implicit ev: AlgMVF[M, _, _]): Rref[M] = ev.MRref
}

trait MutableRref[M] extends Any with Rref[M] {
  implicit def MM: MatMutable[M, _]

  def unsafeRref(m: M): RrefDecomposition[M]
  def rref(m: M): RrefDecomposition[M] = unsafeRref(MM.copy(m))
}

object MutableRref {
  implicit def fromAlg[M](implicit ev: AlgUMVF[M, _, _]): MutableRref[M] = ev.MRref
}

final class MutableRrefImpl[M, @sp(Double, Long) A](implicit M: MatInField[M, A], val MM: MatMutable[M, A], pivotA: Pivot[A]) extends MutableRref[M] {
  implicit def A: Field[A] = M.A
  implicit def eqA: Eq[A] = M.eqA

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

