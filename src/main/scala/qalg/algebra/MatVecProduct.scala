package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecProduct[MA, VA, @sp(Double, Long) A] extends Any with MatInRing[MA, A] { self =>
  implicit def VA: VecBuilder[VA, A]
  def timesl2(v: VA, m: MA): VA = {
    require(nRows(m) == VA.length(v))
    VA.from(new FunV[A] {
    def len = self.nCols(m)
    def f(c: Int): A = {
      var acc = scalar.zero
      cforRange(0 until self.nRows(m)) { r =>
        acc += VA.apply(v, r) * self.apply(m, r, c)
      }
      acc
    }
    })
  }
  def timesr2(m: MA, v: VA): VA = {
    require(nCols(m) == VA.length(v))
    VA.from(new FunV[A] {
      def len = self.nRows(m)
      def f(r: Int): A = {
        var acc = scalar.zero
        cforRange(0 until self.nCols(m)) { c =>
          acc += self.apply(m, r, c) * VA.apply(v, c)
        }
        acc
      }
    })
  }
}
