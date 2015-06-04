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

final class DenseV[@sp(Double, Long) A: ClassTag, M <: Mutability](val array: Array[A]) {
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
}

trait DenseVec[@sp(Double, Long) A, M <: Mutability] extends Any
    with VecBuilder[DenseV[A, M], A] { self =>
//  implicit def V: Vec[DenseV[A, M], A] = self
  implicit def ctA: ClassTag[A]
  type V = DenseV[A, M]
  def length(v: V): Int = v.array.length
  def apply(v: V, k: Int): A = v.array(k)
  def tabulate(n: Int)(f: Int => A): V = new DenseV[A, M](Array.tabulate(n)(f))
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

trait DenseVecInField[@sp(Double, Long) A, M <: Mutability] extends Any with DenseVecInRing[A, M] with VecInField[DenseV[A, M], A]

trait DenseVecMutable[@sp(Double, Long) A] extends Any 
    with VecMutable[DenseV[A, Mutable], A] { self =>
  implicit def ctA: ClassTag[A]
  type V = DenseV[A, Mutable]
  def update(v: V, k: Int, a: A): Unit = { v.array(k) = a }
  def copy(v: V): V = new DenseV[A, Mutable](v.array.clone)
}

object DenseV {
  def seed = 0x5EED5EED

  object longInstance extends DenseVecInRing[Long, Mutability] {
    def ctA = classTag[Long]
    def scalar = Ring[Long]
    def eqA = Eq[Long]
  }

  object doubleInstance extends DenseVecInField[Double, Mutability] {
    def ctA = classTag[Double]
    def scalar = Field[Double]
    def eqA = Eq[Double]
  }

  object rationalInstance extends DenseVecInField[Rational, Mutability] {
    def ctA = classTag[Rational]
    def scalar = Field[Rational]
    def eqA = Eq[Rational]
  }

  @inline implicit def forLong[M <: Mutability]: DenseVecInRing[Long, M] =
    longInstance.asInstanceOf[DenseVecInRing[Long, M]]

  @inline implicit def forDouble[M <: Mutability]: DenseVecInField[Double, M] =
    doubleInstance.asInstanceOf[DenseVecInField[Double, M]]

  @inline implicit def forRational[M <: Mutability]: DenseVecInField[Rational, M] =
    rationalInstance.asInstanceOf[DenseVecInField[Rational, M]]

  implicit object longMutInstance extends DenseVecMutable[Long] {
    def V = forLong[Mutable]
    def ctA = classTag[Long]
  }

  implicit object doubleMutInstance extends DenseVecMutable[Double] {
    def V = forDouble[Mutable]
    def ctA = classTag[Double]
  }

  implicit object rationalMutInstance extends DenseVecMutable[Rational] {
    def V = forRational[Mutable]
    def ctA = classTag[Rational]
  }

  implicit def matType[@sp(Double, Long) A, M <: Mutability]: MatType[A, DenseV[A, M], DenseM[A, M]] = new MatType[A, DenseV[A, M], DenseM[A, M]] { }
}
