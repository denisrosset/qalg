package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.ring._
import util._

trait MatInRing[M, @sp(Double, Long) A] extends Any with MatBuilder[M, A] with Module[M, A] with MultiplicativeSemigroup[M] { self =>
  implicit def A: Ring[A]
  def scalar = A
  def zero: M = tabulate(0, 0)( (r, c) => sys.error("Cannot provide element for empty matrix"))
  def id: M = fill(1, 1)(A.one)
/*  def op(x: M, y: M): M = {
  })*/
  def plus(x: M, y: M): M = {
    require(nRows(x) == nRows(y))
    require(nCols(x) == nCols(y))
    tabulate(nRows(x), nCols(x))( (r, c) => apply(x, r, c) + apply(y, r, c) )
  }
  override def minus(x: M, y: M): M = {
    require(nRows(x) == nRows(y))
    require(nCols(x) == nCols(y))
    tabulate(nRows(x), nCols(x))( (r, c) => apply(x, r, c) - apply(y, r, c) )
  }
  def times(x: M, y: M): M = {
    val nK = nCols(x)
    require(nK == nRows(y))
    tabulate(nRows(x), nCols(y)) { (r, c) =>
      var acc = A.zero
      cforRange(0 until nK) { k =>
        acc += self.apply(x, r, k) * self.apply(y, k, c)
      }
      acc
    }
  }
  def negate(m: M): M = tabulate(nRows(m), nCols(m))( (r, c) => -apply(m, r, c) )
  def timesl(a: A, m: M): M = tabulate(nRows(m), nCols(m))( (r, c) => a * apply(m, r, c) )
}

object MatInRing {
  def apply[M, @sp(Double, Long) A](implicit M: MatInRing[M, A]): MatInRing[M, A] = M
}
