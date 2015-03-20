package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVec[M, V, @sp(Double, Long) A] extends Any with Mat[M, A] { self =>
  implicit def V: Vec[V, A]
}

trait MatVecBuilder[M, V, @sp(Double, Long) A] extends Any with MatVec[M, V, A] with MatBuilder[M, A] { self =>
  implicit def V: VecBuilder[V, A]
  def apply(m: M, rows: ::.type, c: Int): V = V.from(view(m, rows, c))
  def apply(m: M, rows: At1, c: Int): V = V.from(view(m, rows, c))
  def apply(m: M, r: Int, cols: ::.type): V = V.from(view(m, r, cols))
  def apply(m: M, r: Int, cols: At1): V = V.from(view(m, r, cols))
}

trait MatVecInRing[M, V, @sp(Double, Long) A] extends Any with MatVecBuilder[M, V, A] with MatInRing[M, A] { self =>
  implicit def V: VecInRing[V, A]
  def timesl2(v: V, m: M): V = {
    require(nRows(m) == V.length(v))
    V.from(new FunV[A] {
    def len = self.nCols(m)
    def f(c: Int): A = {
      var acc = scalar.zero
      cforRange(0 until self.nRows(m)) { r =>
        acc += V.apply(v, r) * self.apply(m, r, c)
      }
      acc
    }
    })
  }
  def timesr2(m: M, v: V): V = {
    require(nCols(m) == V.length(v))
    V.from(new FunV[A] {
      def len = self.nRows(m)
      def f(r: Int): A = {
        var acc = scalar.zero
        cforRange(0 until self.nCols(m)) { c =>
          acc += self.apply(m, r, c) * V.apply(v, c)
        }
        acc
      }
    })
  }
}

trait MatVecInField[M, V, @sp(Double, Long) A] extends Any with MatInField[M, A] { self =>
  implicit def V: VecInField[V, A]
}

trait MatVecMutable[M, V, @sp(Double, Long) A] extends Any with MatMutable[M, A] { self =>
  implicit def V: VecMutable[V, A]
}
