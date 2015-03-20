package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.ring._
import util._

trait MatInRing[MA, @sp(Double, Long) A] extends Any with MatBuilder[MA, A] with Module[MA, A] with MultiplicativeSemigroup[MA] with Monoid[MA] { self =>
  implicit def scalar: Ring[A]
  def zero: MA = from(FunM.empty[A])
  def id: MA = from(FunM.fill[A](1, 1)(scalar.one))
  def op(x: MA, y: MA): MA = from(new FunM[A] {
    val nrx = self.nRows(x)
    val ncx = self.nCols(x)
    val nry = self.nRows(y)
    val ncy = self.nCols(y)
    def nR = nrx * nry
    def nC = ncx * ncy
    def f(r: Int, c: Int): A = {
      val rx = r / nry
      val cx = c / ncy
      val ry = r % nry
      val cy = c % ncy
      self.apply(x, rx, cx) * self.apply(y, ry, cy)
    }
  })
  def plus(x: MA, y: MA): MA = {
    require(nRows(x) == nRows(y))
    require(nCols(x) == nCols(y))
    from(new FunM[A] {
      def nR = self.nRows(x)
      def nC = self.nCols(x)
      def f(r: Int, c: Int): A = self.apply(x, r, c) + self.apply(y, r, c)
    })
  }
  override def minus(x: MA, y: MA): MA = {
    require(nRows(x) == nRows(y))
    require(nCols(x) == nCols(y))
    from(new FunM[A] {
      def nR = self.nRows(x)
      def nC = self.nCols(x)
      def f(r: Int, c: Int): A = self.apply(x, r, c) - self.apply(y, r, c)
    })
  }
  def times(x: MA, y: MA): MA = {
    val nK = nCols(x)
    require(nK == nRows(y))
    from(new FunM[A] {
      def nR = self.nRows(x)
      def nC = self.nCols(y)
      def f(r: Int, c: Int): A = {
        var acc = scalar.zero
        cforRange(0 until nK) { k =>
          acc += self.apply(x, r, k) * self.apply(y, k, c)
        }
        acc
      }
    })
  }
  def negate(m: MA): MA = {
    from(new FunM[A] {
      def nR = self.nRows(m)
      def nC = self.nCols(m)
      def f(r: Int, c: Int): A = -self.apply(m, r, c)
    })
  }
  def timesl(a: A, m: MA): MA = from(new FunM[A] {
    def nR = self.nRows(m)
    def nC = self.nCols(m)
    def f(r: Int, c: Int): A = a * self.apply(m, r, c)
  })
}
