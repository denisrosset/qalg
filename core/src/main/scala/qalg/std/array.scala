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

trait ArrayVec[@sp(Double, Long) A] extends Any
    with VecBuilder[Array[A], A]
    with VecMutable[Array[A], A] {
  implicit def classTagA: ClassTag[A]
  type V = Array[A]
  def V: Vec[V, A] = this
  def apply(v: V, k: Int): A = v(k)
  def length(v: V): Int = v.length
  def update(v: V, k: Int, a: A): Unit = { v(k) = a }
  def tabulate(n: Int)(f: Int => A): Array[A] = Array.tabulate(n)(f)
  def copy(v: V): V = v.clone
}

trait ArrayVecInRing[@sp(Double, Long) A] extends Any
    with ArrayVec[A]
    with VecInRing[Array[A], A] {
  override def negate(v: Array[A]): Array[A] = ArraySupport.negate(v)
  override def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  override def timesl(r: A, v: Array[A]): Array[A] = ArraySupport.timesl(r, v)
}

trait ArrayVecInField[@sp(Double, Long) A] extends Any
    with ArrayVecInRing[A]
    with VecInField[Array[A], A]

trait ArrayInstances {
  implicit val ArrayDouble = new ArrayVecInField[Double] {
    def classTagA = classTag[Double]
    def eqA = Eq[Double]
    def A = Field[Double]
  }
  implicit val ArrayRational = new ArrayVecInField[Rational] {
    def classTagA = classTag[Rational]
    def eqA = Eq[Rational]
    def A = Field[Rational]
  }
  implicit val ArrayLong = new ArrayVecInRing[Long] {
    def classTagA = classTag[Long]
    def eqA = Eq[Long]
    def A = Ring[Long]
  }
}
