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

final class DenseV[@sp(Double, Long) A: ClassTag](val len: Int, val array: Array[A]) {
  override def toString = math.MatrixPrinting.print(1, len, (r: Int, c: Int) => array(c).toString)
  override def equals(other: Any): Boolean = other match {
    case that: DenseV[A] =>
      (this.len == that.len) && {
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
    cforRange(0 until len) { k =>
      h = MurmurHash3.mix(h, array(k).##)
    }
    MurmurHash3.finalizeHash(h, len)
  }
}

trait DenseVec[@sp(Double, Long) A] extends Any
    with VecBuilder[DenseV[A], A]
    with VecMutable[DenseV[A], A] { self =>
  implicit def V: Vec[DenseV[A], A] = self
  implicit def classTag: ClassTag[A]
  def length(v: DenseV[A]): Int = v.len
  def apply(v: DenseV[A], k: Int): A = v.array(k)
  def update(v: DenseV[A], k: Int, a: A): Unit = { v.array(k) = a }
  def fromArray(a: Array[A]): DenseV[A] = new DenseV(a.length, a)
}

trait DenseVecInRing[@sp(Double, Long) A] extends Any with DenseVec[A] with VecInRing[DenseV[A], A] {
  def fromFunV(v: FunV[A]): DenseV[A] = new DenseV(v.len, Array.tabulate(v.len)(k => v.f(k)))
  override def negate(v: DenseV[A]): DenseV[A] = fromArray(std.ArraySupport.negate(v.array))
  override def plus(x: DenseV[A], y: DenseV[A]): DenseV[A] =
    fromArray(std.ArraySupport.plus(x.array, y.array))
  override def minus(x: DenseV[A], y: DenseV[A]): DenseV[A] =
    fromArray(std.ArraySupport.minus(x.array, y.array))
  override def timesl(x: A, y: DenseV[A]): DenseV[A] =
    fromArray(std.ArraySupport.timesl(x, y.array))
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
}
