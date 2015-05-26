package com.faacets.qalg
package math

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.std.long._
import spire.syntax.field._
import spire.syntax.cfor._

import algebra._

final class DenseV[@sp(Double, Long) A: ClassTag](val array: Array[A]) {
  override def toString = math.MatrixPrinting.print(1, array.length, (r: Int, c: Int) => array(c).toString)
  override def equals(other: Any): Boolean = other match {
    case that: DenseV[A] =>
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

trait DenseVec[@sp(Double, Long) A] extends Any
    with VecBuilder[DenseV[A], A]
    with VecMutable[DenseV[A], A] { self =>
  implicit def V: Vec[DenseV[A], A] = self
  implicit def classTag: ClassTag[A]
  type V = DenseV[A]
  def length(v: V): Int = v.array.length
  def apply(v: V, k: Int): A = v.array(k)
  def update(v: V, k: Int, a: A): Unit = { v.array(k) = a }
  def tabulate(n: Int)(f: Int => A): V = new DenseV(Array.tabulate(n)(f))
  def copy(v: V): V = new DenseV(v.array.clone)
}

trait DenseVecInRing[@sp(Double, Long) A] extends Any with DenseVec[A] with VecInRing[DenseV[A], A] {
  override def negate(v: DenseV[A]): DenseV[A] = new DenseV(std.ArraySupport.negate(v.array))
  override def plus(x: DenseV[A], y: DenseV[A]): DenseV[A] =
    new DenseV(std.ArraySupport.plus(x.array, y.array))
  override def minus(x: DenseV[A], y: DenseV[A]): DenseV[A] =
    new DenseV(std.ArraySupport.minus(x.array, y.array))
  override def timesl(x: A, y: DenseV[A]): DenseV[A] =
    new DenseV(std.ArraySupport.timesl(x, y.array))
}

trait DenseVecInField[@sp(Double, Long) A] extends Any with DenseVecInRing[A] with VecInField[DenseV[A], A]

object DenseV {
  def seed = 0x5EED5EED
  implicit val forLong = new DenseVecInRing[Long] {
    def classTag = scala.reflect.classTag[Long]
    def scalar = Ring[Long]
    def eqA = Eq[Long]
  }
  implicit val forDouble = new DenseVecInField[Double] {
    def classTag = scala.reflect.classTag[Double]
    def scalar = Field[Double]
    def eqA = Eq[Double]
  }
  implicit val forRational = new DenseVecInField[Rational] {
    def classTag = scala.reflect.classTag[Rational]
    def scalar = Field[Rational]
    def eqA = Eq[Rational]
  }
  implicit def matType[@sp(Double, Long) A]: MatType[DenseM[A], DenseV[A], A] = new MatType[DenseM[A], DenseV[A], A] { }
}
