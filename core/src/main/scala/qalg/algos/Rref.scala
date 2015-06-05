package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

trait RrefDecomposition[M] extends Any {
  def reduced: M
  def basis: Array[Int]
  def rank: Int = basis.length
}

trait Rref[M] extends Any {
  def rref(m: M): RrefDecomposition[M]
}

trait MutableRref[M] extends Any with Rref[M] {
  implicit def MM: MatMutable[M, _]

  def unsafeRref(m: M): RrefDecomposition[M]
  def rref(m: M) = unsafeRref(MM.copy(m))
}

trait MutableRrefImpl[M, @sp(Double, Long) A] extends Any with MutableRref[M] {
  implicit def M: MatInField[M, A]
  implicit def A: Field[A] = M.A
  implicit def MM: MatMutable[M, A]
  implicit def orderA: Order[A]
  implicit def signedA: Signed[A]

  def unsafeRref(m: M): RrefDecomposition[M] = {
    val used = collection.mutable.ArrayBuilder.make[Int]
    var r = 0
    cforRange(0 until m.nCols) { c =>
      if (r < m.nRows) {
        var mx = m(r, c).abs
        var pivot = r
        cforRange((r + 1) until m.nRows) { r1 =>
          val current = m(r1, c).abs
          if (current > mx) {
            mx = current
            pivot = r1
          }
        }
        if (!mx.isZero) { // if el is zero, skip the column c
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
          cforRange(0 until m.nRows) { ridx =>
            if (ridx != r)
              cforRange(c until m.nCols) { c1 =>
                m(ridx, c1) = m(ridx, c1) - m(r, c1) * m(ridx, c)
              }
          }
          r += 1
        }
      }
    }
    new RrefDecomposition[M] {
      def reduced = m
      def basis = used.result
    }
  }
}

