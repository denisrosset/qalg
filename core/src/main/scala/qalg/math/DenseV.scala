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

trait DenseVecMutable[@sp(Double, Long) A] extends Any 
    with VecMutable[DenseV[A, Mutable], A] { self =>
  implicit def ctA: ClassTag[A]
  type V = DenseV[A, Mutable]
  def update(v: V, k: Int, a: A): Unit = { v.array(k) = a }
  def copy(v: V): V = new DenseV[A, Mutable](v.array.clone)
}

object DenseV {
  def seed = 0x5EED5EED

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

  implicit object longMutInstance extends DenseVecMutable[Long] {
    def V = long[Mutable]
    def ctA = classTag[Long]
  }

  implicit object doubleMutInstance extends DenseVecMutable[Double] {
    def V = double[Mutable]
    def ctA = classTag[Double]
  }

  implicit object rationalMutInstance extends DenseVecMutable[Rational] {
    def V = rational[Mutable]
    def ctA = classTag[Rational]
  }

  implicit def rationalPack: PackVField[DenseV[Rational, Immutable], Rational] = DenseM.rationalPack

  implicit def matType[@sp(Double, Long) A, M <: Mutability]: MatType[A, DenseV[A, M], DenseM[A, M]] = new MatType[A, DenseV[A, M], DenseM[A, M]] { }
}
