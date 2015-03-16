package com.faacets.qalg
package std

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

object ArraySupport {
  def plus[@sp(Double, Long) A: ClassTag: AdditiveMonoid](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) + y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }
  def negate[@sp(Double, Long) A: ClassTag: AdditiveGroup](x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < x.length) {
      y(i) = -x(i)
      i += 1
    }
    y
  }
  def minus[@sp(Double, Long) A: ClassTag: AdditiveGroup](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) - y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = -y(i); i += 1 }
    z
  }
  def timesl[@sp(Double, Long) A: ClassTag: MultiplicativeSemigroup](r: A, x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < y.length) { y(i) = r * x(i); i += 1 }
    y
  }
}

final class ArrayVec[@sp(Double, Long) A: ClassTag](implicit
  val scalar: AdditiveMonoid[A],
  val eqA: Eq[A]) extends VecBuilder[Array[A], A] {
  def apply(v: Array[A], k: Int): A = v(k)
  def length(v: Array[A]): Int = v.length
  def from(v: FunV[A]): Array[A] = Array.tabulate(v.len)(v.f(_))
}

final class ArrayVecModule[@sp(Double, Long) A: ClassTag](implicit
  val scalar: Ring[A],
  val eqA: Eq[A]) extends VecInRing[Array[A], A] {
  def apply(v: Array[A], k: Int): A = v(k)
  def length(v: Array[A]): Int = v.length
  def from(v: FunV[A]): Array[A] = Array.tabulate(v.len)(v.f(_))
  def negate(v: Array[A]): Array[A] = ArraySupport.negate(v)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, v: Array[A]): Array[A] = ArraySupport.timesl(r, v)
}

final class ArrayVecVectorSpace[@sp(Double, Long) A: ClassTag](implicit
  val scalar: Field[A],
  val eqA: Eq[A]) extends VecInField[Array[A], A] {
  def apply(v: Array[A], k: Int): A = v(k)
  def length(v: Array[A]): Int = v.length
  def from(v: FunV[A]): Array[A] = Array.tabulate(v.len)(v.f(_))
  def negate(v: Array[A]): Array[A] = ArraySupport.negate(v)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, v: Array[A]): Array[A] = ArraySupport.timesl(r, v)
}

trait ArrayInstances {
  implicit val ArrayVecDouble = new ArrayVecVectorSpace[Double]
  implicit val ArrayVecRational = new ArrayVecVectorSpace[Rational]
  implicit val ArrayVecLong = new ArrayVecModule[Long]
  implicit def ArrayVec[A: ClassTag: AdditiveMonoid: Eq]: VecBuilder[Array[A], A] = new ArrayVec[A]
}
