package com.faacets.qalg
package math

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.std.long._
import spire.syntax.field._
import spire.syntax.cfor._

import algebra._

final class DenseV[@sp(Double, Long) A: ClassTag, M <: Mutability](val array: Array[A]) { lhs =>
  def length: Int = array.length
  override def toString = math.MatrixPrinting.print(1, array.length, (r: Int, c: Int) => array(c).toString)
  override def equals(other: Any): Boolean = other match {
    case that: DenseV[A, _] =>
      (this.array.length == that.array.length) && {
        cforRange(0 until array.length) { k =>
          if (this.array(k) != that.array(k)) return false
        }
        true
      }
    case _ => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3
    var h = DenseV.seed
    cforRange(0 until array.length) { k =>
      h = MurmurHash3.mix(h, array(k).##)
    }
    MurmurHash3.finalizeHash(h, array.length)
  }

  def unary_-(implicit A: Ring[A]): DenseV[A, M] = new DenseV[A, M](std.ArraySupport.negate(lhs.array))
  def +(rhs: DenseV[A, M])(implicit A: Ring[A]): DenseV[A, M] = new DenseV[A, M](std.ArraySupport.plus(lhs.array, rhs.array))
  def -(rhs: DenseV[A, M])(implicit A: Ring[A]): DenseV[A, M] = new DenseV[A, M](std.ArraySupport.minus(lhs.array, rhs.array))
  def *:(lhs: A)(implicit A: Ring[A]): DenseV[A, M] =
    new DenseV[A, M](std.ArraySupport.timesl(lhs, array))
  def :*(rhs: A)(implicit A: Ring[A]): DenseV[A, M] =
    new DenseV[A, M](std.ArraySupport.timesr(lhs.array, rhs))
}

object DenseV {
  def seed = 0x5EED5EED
  def tabulate[@sp(Double, Long) A: ClassTag, M <: Mutability](n: Int)(f: Int => A): DenseV[A, M] = new DenseV[A, M](Array.tabulate(n)(f))
  def fromFunM[@sp(Double, Long) A: ClassTag, M <: Mutability](v: FunV[A]): DenseV[A, M] = {
    val n = v.len
    val array = new Array[A](n)
    var i = 0
    while (i < n) {
      array(i) = v.f(i)
      i += 1
    }
    new DenseV[A, M](array)
  }
}

final class DenseVecMutable[@sp(Double, Long) A](implicit val V: Vec[DenseV[A, Mutable], A], val ctA: ClassTag[A]) extends VecMutable[DenseV[A, Mutable], A] {
  type V = DenseV[A, Mutable]
  def update(v: V, k: Int, a: A): Unit = { v.array(k) = a }
  def copy(v: V): V = new DenseV[A, Mutable](v.array.clone)
}

/*
trait DenseVec[@sp(Double, Long) A, M <: Mutability] extends Any
    with VecBuilder[DenseV[A, M], A] { self =>
//  implicit def V: Vec[DenseV[A, M], A] = self
  implicit def ctA: ClassTag[A]
  type V = DenseV[A, M]
  def length(v: V): Int = v.array.length
  def apply(v: V, k: Int): A = v.array(k)
}

trait DenseVecInRing[@sp(Double, Long) A, M <: Mutability] extends Any with DenseVec[A, M] with VecInRing[DenseV[A, M], A] {
  override def negate(v: DenseV[A, M]): DenseV[A, M] = new DenseV(std.ArraySupport.negate(v.array))
  override def plus(x: DenseV[A, M], y: DenseV[A, M]): DenseV[A, M] =
    new DenseV(std.ArraySupport.plus(x.array, y.array))
  override def minus(x: DenseV[A, M], y: DenseV[A, M]): DenseV[A, M] =
    new DenseV(std.ArraySupport.minus(x.array, y.array))
  override def timesl(x: A, y: DenseV[A, M]): DenseV[A, M] =
    new DenseV(std.ArraySupport.timesl(x, y.array))
}

object DenseV {

  /*
  object longInstance extends DenseVecInRing[Long, Mutability] with VecInEuclideanRing[DenseV[Long, Mutability], Long] {
    def ctA = classTag[Long]
    def A = EuclideanRing[Long]
    def eqA = Eq[Long]
  }

  object doubleInstance extends DenseVecInRing[Double, Mutability] with VecInField[DenseV[Double, Mutability], Double] {
    def ctA = classTag[Double]
    def A = Field[Double]
    def eqA = Eq[Double]
  }

  object rationalInstance extends DenseVecInRing[Rational, Mutability] with VecInField[DenseV[Rational, Mutability], Rational] {
    def ctA = classTag[Rational]
    def A = Field[Rational]
    def eqA = Eq[Rational]
  }

  implicit def long[M <: Mutability]: VecInEuclideanRing[DenseV[Long, M], Long] =
    longInstance.asInstanceOf[VecInEuclideanRing[DenseV[Long, M], Long]]

  implicit def double[M <: Mutability]: VecInField[DenseV[Double, M], Double] =
    doubleInstance.asInstanceOf[VecInField[DenseV[Double, M], Double]]

  implicit def rational[M <: Mutability]: VecInField[DenseV[Rational, M], Rational] =
    rationalInstance.asInstanceOf[VecInField[DenseV[Rational, M], Rational]]

  implicit object longMutable extends DenseVecMutable[Long] {
    def V = long[Mutable]
    def ctA = classTag[Long]
  }

  implicit object doubleMutable extends DenseVecMutable[Double] {
    def V = double[Mutable]
    def ctA = classTag[Double]
  }

  implicit object rationalMutable extends DenseVecMutable[Rational] {
    def V = rational[Mutable]
    def ctA = classTag[Rational]
  }

  implicit object longConv extends ConvV[DenseV[Long, Immutable], DenseV[Long, Mutable]] {
    def UV = longMutable
    type IV = DenseV[Long, Immutable]
    type UV = DenseV[Long, Mutable]
    def unsafeToIV(v: UV): IV = v.asInstanceOf[IV]
    def unsafeToUV(v: IV): UV = v.asInstanceOf[UV]
  }*/
}
 */
