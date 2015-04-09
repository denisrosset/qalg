package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

trait RREF {
  case class RREFDecomposition[M, @sp(Double) A](reduced: M, basis: Array[Int]) {
    def rank: Int = basis.size
  }

  def rref[M, @sp(Double) A](m: M)(implicit M: MatInField[M, A], MM: MatMutable[M, A], orderA: Order[A], signedA: Signed[A]): RREFDecomposition[M, A] = {
    import M.scalar
    val a = M.fromFunM(m.view(::, ::))
    val used = collection.mutable.ArrayBuilder.make[Int]
    var r = 0
    cforRange(0 until a.nCols) { c =>
      if (r < a.nRows) {
        var mx = a(r, c).abs
        var pivot = r
        cforRange((r + 1) until a.nRows) { r1 =>
          val current = a(r1, c).abs
          if (current > mx) {
            mx = current
            pivot = r1
          }
        }
        if (!mx.isZero) { // if el is zero, skip the column c
          used += c // keep track of bound variables

          // swap current row and pivot row
          cforRange(c until a.nCols) { c1 =>
            val tmp = a(pivot, c1)
            a(pivot, c1) = a(r, c1)
            a(r, c1) = tmp
          }
          // normalize pivot row
          val f = a(r, c)
          cforRange(c until a.nCols) { c1 =>
            a(r, c1) = a(r, c1) / f
          }
          // eliminate current column
          cforRange(0 until a.nRows) { ridx =>
            if (ridx != r)
              cforRange(c until a.nCols) { c1 =>
                a(ridx, c1) = a(ridx, c1) - a(r, c1) * a(ridx, c)
              }
          }
          r += 1
        }
      }
    }
    RREFDecomposition(a, used.result)
  }
}
