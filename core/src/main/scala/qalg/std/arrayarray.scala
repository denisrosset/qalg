package com.faacets.qalg
package std

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.std.long._
import spire.syntax.ring._

import algebra._
import syntax.vec._

object ArrayArraySupport {
  def plus[@sp(Double, Long) A: ClassTag: AdditiveMonoid](x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = {
    val nR = spire.math.max(x.length, y.length)
    val nC = spire.math.max(x(0).length, y(0).length)
    val z = new Array[Array[A]](nR)
    var r = 0
    while (r < nR) {
      val xrow = x(r)
      val yrow = y(r)
      var c = 0
      val zrow = ArraySupport.plus(xrow, yrow)
      z(r) = zrow
      r += 1
    }
    z
  }
  def minus[@sp(Double, Long) A: ClassTag: AdditiveGroup](x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = {
    val nR = spire.math.max(x.length, y.length)
    val nC = spire.math.max(x(0).length, y(0).length)
    val z = new Array[Array[A]](nR)
    var r = 0
    while (r < nR) {
      val xrow = x(r)
      val yrow = y(r)
      var c = 0
      val zrow = ArraySupport.minus(xrow, yrow)
      z(r) = zrow
      r += 1
    }
    z
  }

  def timesl[@sp(Double, Long) A: ClassTag: MultiplicativeSemigroup](a: A, x: Array[Array[A]]): Array[Array[A]] = {
    val nR = x.length
    val y = new Array[Array[A]](nR)
    var r = 0
    while (r < nR) {
      y(r) = ArraySupport.timesl(a, x(r))
      r += 1
    }
    y
  }

  def times[@sp(Double, Long) A: ClassTag: Ring](x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = {
    val nR = x.length
    val nC = y(0).length
    val nK = x(0).length
    require(y.length == nK)
    val z = new Array[Array[A]](nR)
    var r = 0
    while (r < nR) {
      val row = new Array[A](nC)
      var c = 0
      while (c < nC) {
        var acc = x(r)(0)*y(0)(c)
        var k = 1
        while (k < nK) {
          acc += x(r)(k)*y(k)(c)
          k += 1
        }
        row(c) = acc
        c += 1
      }
      z(r) = row
      r += 1
    }
    z
  }
  def negate[@sp(Double, Long) A: ClassTag: AdditiveGroup](x: Array[Array[A]]): Array[Array[A]] = {
    val nR = x.length
    val nC = x(0).length
    val y = new Array[Array[A]](nR)
    var r = 0
    while (r < nR) {
      val origRow = y(r)
      val newRow = new Array[A](nC)
      var c = 0
      while (c < nC) {
        newRow(c) = - origRow(c)
        c += 1
      }
      y(r) = newRow
      r += 1
    }
    y
  }
  def timesl2[@sp(Double, Long) A: ClassTag: Ring](x: Array[A], y: Array[Array[A]]): Array[A] = {
    val nC = y(0).length
    val nK = x.length
    require(y.length == nK)
    val z = new Array[A](nC)
    var c = 0
    while (c < nC) {
      var acc = x(0)*y(0)(c)
      var k = 1
      while (k < nK) {
        acc += x(k)*y(k)(c)
        k += 1
      }
      z(c) = acc
      c += 1
    }
    z
  }
  def timesr2[@sp(Double, Long) A: ClassTag: Ring](x: Array[Array[A]], y: Array[A]): Array[A] = {
    val nR = x.length
    val nK = y.length
    require(x(0).length == nK)
    val z = new Array[A](nR)
    var r = 0
    while (r < nR) {
      var acc = x(r)(0)*y(0)
      var k = 1
      while (k < nK) {
        acc += x(r)(k)*y(k)
        k += 1
      }
      z(r) = acc
      r += 1
    }
    z
  }
}

trait ArrayArrayMatVec[@sp(Double, Long) A] extends Any
    with MatVecBuilder[Array[Array[A]], Array[A], A]
    with MatVecMutable[Array[Array[A]], Array[A], A] { self =>
  type M = Array[Array[A]]
  type V = Array[A]
  implicit def classTagA: ClassTag[A]
  implicit def V: VecBuilder[V, A] with VecMutable[V, A]
  def M: Mat[M, A] = self
  def fromFunM(m: FunM[A]): M = Array.tabulate(m.nR, m.nC)( (r, c) => m.f(r, c))
  def nRows(m: M): Int = m.length
  def nCols(m: M): Int = m(0).length
  def apply(m: M, r: Int, c: Int): A = m(r)(c)
  def update(m: M, r: Int, c: Int, a: A): Unit = { m(r)(c) = a }
}

trait ArrayArrayMatVecInRing[@sp(Double, Long) A] extends Any
    with ArrayArrayMatVec[A]
    with MatVecInRing[Array[Array[A]], Array[A], A] {
  implicit def V: VecInRing[V, A] with VecMutable[V, A]
  override def plus(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.plus(x, y)
  override def minus(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.minus(x, y)
  override def negate(m: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.negate(m)
  override def timesl(a: A, m: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.timesl(a, m)
  override def times(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.times(x, y)
  override def timesl2(x: Array[A], y: Array[Array[A]]): Array[A] = ArrayArraySupport.timesl2(x, y)
  override def timesr2(x: Array[Array[A]], y: Array[A]): Array[A] = ArrayArraySupport.timesr2(x, y)
}

trait ArrayArrayMatVecInField[@sp(Double, Long) A] extends Any
    with ArrayArrayMatVecInRing[A]
    with MatVecInField[Array[Array[A]], Array[A], A] {
  implicit def V: VecInField[V, A] with VecMutable[V, A]
}

trait ArrayArrayInstances {
  import array._
  implicit val ArrayArrayDouble = new ArrayArrayMatVecInField[Double] {
    implicit val V = ArrayDouble
    def classTagA = classTag[Double]
    def scalar = Field[Double]
    def eqA = Eq[Double]
  }
  implicit val ArrayArrayRational = new ArrayArrayMatVecInField[Rational] {
    implicit val V = ArrayRational
    def classTagA = classTag[Rational]
    def scalar = Field[Rational]
    def eqA = Eq[Rational]
  }
  implicit val ArrayArrayLong = new ArrayArrayMatVecInRing[Long] {
    implicit val V = ArrayLong
    def classTagA = classTag[Long]
    def scalar = Ring[Long]
    def eqA = Eq[Long]
  }
}
