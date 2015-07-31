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

final class DenseVInRing[@sp(Long) A: ClassTag, M <: Mutability](implicit val A: Ring[A], val eqA: Eq[A]) extends VecInRing[DenseV[A, M], A] {
  type V = DenseV[A, M]
  def apply(v: V, k: Int): A = v.array(k)
  def length(v: V): Int = v.array.length
  override def negate(v: V): V = -v
  override def plus(x: V, y: V): V = x + y
  override def minus(x: V, y: V): V = x - y
  override def timesl(x: A, y: V): V = x *: y
  override def timesr(x: V, y: A): V = x :* y
  def tabulate(n: Int)(f: Int => A): V = DenseV.tabulate[A, M](n)(f)
}

final class DenseVInField[@sp(Double) A: ClassTag, M <: Mutability](implicit val A: Field[A], val eqA: Eq[A]) extends VecInField[DenseV[A, M], A] {
  type V = DenseV[A, M]
  def apply(v: V, k: Int): A = v.array(k)
  def length(v: V): Int = v.array.length
  override def negate(v: V): V = -v
  override def plus(x: V, y: V): V = x + y
  override def minus(x: V, y: V): V = x - y
  override def timesl(x: A, y: V): V = x *: y
  override def timesr(x: V, y: A): V = x :* y
  def tabulate(n: Int)(f: Int => A): V = DenseV.tabulate[A, M](n)(f)
}


final class DenseVMutable[@sp(Double, Long) A](implicit val V: Vec[DenseV[A, Mutable], A], val ctA: ClassTag[A]) extends VecMutable[DenseV[A, Mutable], A] {
  type V = DenseV[A, Mutable]
  def update(v: V, k: Int, a: A): Unit = { v.array(k) = a }
  def copy(v: V): V = new DenseV[A, Mutable](v.array.clone)
}

final class DenseVConv[@sp(Double, Long) A](implicit val UV: VecMutable[DenseV[A, Mutable], A]) extends ConvV[DenseV[A, Immutable], DenseV[A, Mutable], A] {
  type IV = DenseV[A, Immutable]
  type UV = DenseV[A, Mutable]
  def toUV(v: IV): UV = UV.copy(unsafeToUV(v))
  def toIV(v: UV): IV = unsafeToIV(UV.copy(v))
  def unsafeToIV(v: UV): IV = v.asInstanceOf[IV]
  def unsafeToUV(v: IV): UV = v.asInstanceOf[UV]
}

trait DenseV1 {
  implicit def ring[A: Ring: Eq: ClassTag, M <: Mutability]: VecInRing[DenseV[A, M], A] = new DenseVInRing[A, M]
}

trait DenseV0 extends DenseV1 {
  implicit def field[A: Field: Eq: ClassTag, M <: Mutability]: VecInField[DenseV[A, M], A] = new DenseVInField[A, M]
}

object DenseV extends DenseV0 {
  def seed = 0x5EED5EED

  def tabulate[@sp(Double, Long) A: ClassTag, M <: Mutability](n: Int)(f: Int => A): DenseV[A, M] = new DenseV[A, M](Array.tabulate(n)(f))

  val longInstance: VecInRing[DenseV[Long, Mutability], Long] = new DenseVInRing[Long, Mutability]
  val doubleInstance: VecInField[DenseV[Double, Mutability], Double] = new DenseVInField[Double, Mutability]
  val rationalInstance: VecInField[DenseV[Rational, Mutability], Rational] = new DenseVInField[Rational, Mutability]

  implicit def long[M <: Mutability]: VecInRing[DenseV[Long, M], Long] =
    longInstance.asInstanceOf[VecInRing[DenseV[Long, M], Long]]

  implicit def double[M <: Mutability]: VecInField[DenseV[Double, M], Double] =
    doubleInstance.asInstanceOf[VecInField[DenseV[Double, M], Double]]

  implicit def rational[M <: Mutability]: VecInField[DenseV[Rational, M], Rational] =
    rationalInstance.asInstanceOf[VecInField[DenseV[Rational, M], Rational]]

  implicit val longMutableInstance: VecMutable[DenseV[Long, Mutable], Long] = new DenseVMutable[Long]
  implicit val doubleMutableInstance: VecMutable[DenseV[Double, Mutable], Double] = new DenseVMutable[Double]
  implicit val rationalMutableInstance: VecMutable[DenseV[Rational, Mutable], Rational] = new DenseVMutable[Rational]

  implicit val longConv: ConvV[DenseV[Long, Immutable], DenseV[Long, Mutable], Long] = new DenseVConv[Long]
  implicit val doubleConv: ConvV[DenseV[Double, Immutable], DenseV[Double, Mutable], Double] = new DenseVConv[Double]
  implicit val rationalConv: ConvV[DenseV[Rational, Immutable], DenseV[Rational, Mutable], Rational] = new DenseVConv[Rational]
}
