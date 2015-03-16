package com.faacets.qalg

import scala.language.higherKinds

import scala.{specialized => sp}

import scala.reflect.ClassTag

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

final class ArrayMat[@sp(Double, Long) A: ClassTag](implicit
  val scalar: AdditiveMonoid[A],
  val eqA: Eq[A]) extends MatBuilder[Array[Array[A]], A] with MatMutable[Array[Array[A]], A] {
  def from(m: FunM[A]): Array[Array[A]] = Array.tabulate(m.nR, m.nC)( (r, c) => m.f(r, c))
  def nRows(m: Array[Array[A]]): Int = m.length
  def nCols(m: Array[Array[A]]): Int = m(0).length
  def apply(m: Array[Array[A]], r: Int, c: Int): A = m(r)(c)
  def update(m: Array[Array[A]], r: Int, c: Int, a: A): Unit = { m(r)(c) = a }
}

final class ArrayMatInRing[@sp(Double, Long) A: ClassTag](implicit
  val scalar: Ring[A],
  val eqA: Eq[A]) extends MatInRing[Array[Array[A]], A] with MatVec[Array[Array[A]], Array[A], A] with MatMutable[Array[Array[A]], A] {
  def from(m: FunM[A]): Array[Array[A]] = Array.tabulate(m.nR, m.nC)( (r, c) => m.f(r, c))
  def nRows(m: Array[Array[A]]): Int = m.length
  def nCols(m: Array[Array[A]]): Int = m(0).length
  def apply(m: Array[Array[A]], r: Int, c: Int): A = m(r)(c)
  def plus(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.plus(x, y)
  override def minus(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.minus(x, y)
  // TODO optimize
  def negate(m: Array[Array[A]]): Array[Array[A]] = m.map(_.map(-_))
  def zero: Array[Array[A]] = Array(new Array[A](0))
  def timesl(a: A, m: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.timesl(a, m)
  def times(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.times(x, y)
  def timesl2(x: Array[A], y: Array[Array[A]]): Array[A] = ArrayArraySupport.timesl2(x, y)
  def timesr2(x: Array[Array[A]], y: Array[A]): Array[A] = ArrayArraySupport.timesr2(x, y)
  def update(m: Array[Array[A]], r: Int, c: Int, a: A): Unit = { m(r)(c) = a }
}

final class ArrayMatInField[@sp(Double, Long) A: ClassTag](implicit
  val scalar: Field[A],
  val eqA: Eq[A]) extends MatInField[Array[Array[A]], A] with MatVec[Array[Array[A]], Array[A], A] with MatMutable[Array[Array[A]], A] {
  def from(m: FunM[A]): Array[Array[A]] = Array.tabulate(m.nR, m.nC)( (r, c) => m.f(r, c))
  def nRows(m: Array[Array[A]]): Int = m.length
  def nCols(m: Array[Array[A]]): Int = m(0).length
  def apply(m: Array[Array[A]], r: Int, c: Int): A = m(r)(c)
  def plus(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.plus(x, y)
  override def minus(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.minus(x, y)
  // TODO optimize
  def negate(m: Array[Array[A]]): Array[Array[A]] = m.map(_.map(-_))
  def zero: Array[Array[A]] = Array(new Array[A](0))
  def timesl(a: A, m: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.timesl(a, m)
  def times(x: Array[Array[A]], y: Array[Array[A]]): Array[Array[A]] = ArrayArraySupport.times(x, y)
  def timesl2(x: Array[A], y: Array[Array[A]]): Array[A] = ArrayArraySupport.timesl2(x, y)
  def timesr2(x: Array[Array[A]], y: Array[A]): Array[A] = ArrayArraySupport.timesr2(x, y)
  def update(m: Array[Array[A]], r: Int, c: Int, a: A): Unit = { m(r)(c) = a }
}

trait ArrayArrayInstances {
  implicit val ArrayMatDouble = new ArrayMatInField[Double]
  implicit val ArrayMatRational = new ArrayMatInField[Rational]
  implicit val ArrayMatLong = new ArrayMatInRing[Long]
  implicit def ArrayMat[A: ClassTag: AdditiveMonoid: Eq]: MatBuilder[Array[Array[A]], A] = new ArrayMat[A]
}
