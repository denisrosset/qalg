package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecInRing[M, V, @sp(Double, Long) A] extends Any with MatVecBuilder[M, V, A] with MatInRing[M, A] { self =>
  implicit def V: VecInRing[V, A]
  def timesl2(v: V, m: M): V = {
    require(nRows(m) == V.length(v))
    V.fromFunV(new FunV[A] {
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
    V.fromFunV(new FunV[A] {
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

trait ConvertedMatVecInRing[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMatVecBuilder[M, V, A, J]
    with ConvertedMatInRing[M, A, J]
    with MatVecInRing[M, V, A] {
  def source: MatVecInRing[M, V, J]
  override def timesl2(v: V, m: M): V = source.timesl2(v, m)
  override def timesr2(m: M, v: V): V = source.timesr2(m, v)
}
