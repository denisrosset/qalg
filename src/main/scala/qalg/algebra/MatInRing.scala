package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.ring._
import util._

trait MatInRing[M, @sp(Double, Long) A] extends Any with MatBuilder[M, A] with Module[M, A] with MultiplicativeSemigroup[M] with Monoid[M] { self =>
  implicit def scalar: Ring[A]
  def zero: M = from(FunM.empty[A])
  def id: M = from(FunM.fill[A](1, 1)(scalar.one))
  def op(x: M, y: M): M = from(new FunM[A] {
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
  def plus(x: M, y: M): M = {
    require(nRows(x) == nRows(y))
    require(nCols(x) == nCols(y))
    from(new FunM[A] {
      def nR = self.nRows(x)
      def nC = self.nCols(x)
      def f(r: Int, c: Int): A = self.apply(x, r, c) + self.apply(y, r, c)
    })
  }
  override def minus(x: M, y: M): M = {
    require(nRows(x) == nRows(y))
    require(nCols(x) == nCols(y))
    from(new FunM[A] {
      def nR = self.nRows(x)
      def nC = self.nCols(x)
      def f(r: Int, c: Int): A = self.apply(x, r, c) - self.apply(y, r, c)
    })
  }
  def times(x: M, y: M): M = {
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
  def negate(m: M): M = {
    from(new FunM[A] {
      def nR = self.nRows(m)
      def nC = self.nCols(m)
      def f(r: Int, c: Int): A = -self.apply(m, r, c)
    })
  }
  def timesl(a: A, m: M): M = from(new FunM[A] {
    def nR = self.nRows(m)
    def nC = self.nCols(m)
    def f(r: Int, c: Int): A = a * self.apply(m, r, c)
  })
}

trait ConvertedMatInRing[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMat[M, A, J]
    with MatInRing[M, A] {
  def source: MatInRing[M, J]

  override def zero: M = source.zero
  override def id: M = source.id
  override def op(x: M, y: M): M = source.op(x, y)
  override def plus(x: M, y: M): M = source.plus(x, y)
  override def minus(x: M, y: M): M = source.minus(x, y)
  override def times(x: M, y: M): M = source.times(x, y)
  override def negate(m: M): M = source.negate(m)
  override def timesl(a: A, m: M): M = source.timesl(aToJ(a), m)
}
