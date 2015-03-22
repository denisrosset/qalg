package com.faacets.qalg
package math

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.syntax.field._
import spire.syntax.cfor._

import algebra._

/** Dense matrix stored in row-major order. */
final class DenseM[@sp(Double, Long) A: ClassTag](val nR: Int, val nC: Int, val array: Array[A]) {
  override def toString = math.MatrixPrinting.print(nR, nC, (r: Int, c: Int) => array(r * nC + c).toString)
  override def equals(other: Any): Boolean = other match {
    case that: DenseM[A] =>
      (this.nR == that.nR) && (this.nC == that.nC) && {
        cforRange(0 until array.length) { k =>
          if (this.array(k) != that.array(k)) return false
        }
        true
      }
    case _ => false
  }
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3
    var h = DenseM.seed
    cforRange(0 until nR * nC) { k =>
      h = MurmurHash3.mix(h, array(k).##)
    }
    h = MurmurHash3.mix(h, nR)
    MurmurHash3.finalizeHash(h, nC)
  }
}

trait DenseMatVec[@sp(Double, Long) A] extends Any
    with MatVecBuilder[DenseM[A], DenseV[A], A]
    with MatVecMutable[DenseM[A], DenseV[A], A] { self =>
  implicit def V: VecBuilder[DenseV[A], A] with VecMutable[DenseV[A], A]
  implicit def M: Mat[DenseM[A], A] = self
  implicit def classTag: ClassTag[A]
  def apply(m: DenseM[A], r: Int, c: Int): A = m.array(r * m.nC + c)
  def update(m: DenseM[A], r: Int, c: Int, a: A): Unit = { m.array(r * m.nC + c) = a }
  def nRows(m: DenseM[A]): Int = m.nR
  def nCols(m: DenseM[A]): Int = m.nC
}

trait DenseMatVecInRing[@sp(Double, Long) A] extends Any
    with DenseMatVec[A]
    with MatVecInRing[DenseM[A], DenseV[A], A] {
  implicit def V: VecInRing[DenseV[A], A] with VecMutable[DenseV[A], A]

  def fromFunM(m: FunM[A]): DenseM[A] = {
    val nR = m.nR
    val nC = m.nC
    val array = new Array[A](nR * nC)
    var i = 0
    var r = 0
    while (r < nR) {
      var c = 0
      while (c < nC) {
        array(i) = m.f(r, c)
        c += 1
        i += 1
      }
      r += 1
    }
    new DenseM(nR, nC, array)
  }
  override def negate(m: DenseM[A]): DenseM[A] = new DenseM(m.nR, m.nC, std.ArraySupport.negate(m.array))
  override def plus(x: DenseM[A], y: DenseM[A]): DenseM[A] = {
    require(x.nR == y.nR && x.nC == y.nC)
    new DenseM(x.nR, x.nC, std.ArraySupport.plus(x.array, y.array))
  }
  override def minus(x: DenseM[A], y: DenseM[A]): DenseM[A] = {
    require(x.nR == y.nR && x.nC == y.nC)
    new DenseM(x.nR, x.nC, std.ArraySupport.minus(x.array, y.array))
  }
  override def timesl(x: A, y: DenseM[A]): DenseM[A] =
    new DenseM(y.nR, y.nC, std.ArraySupport.timesl(x, y.array))
  override def times(x: DenseM[A], y: DenseM[A]): DenseM[A] = {
    val xR = x.nR
    val yR = y.nR
    val xC = x.nC
    val yC = y.nC
    val xa = x.array
    val ya = y.array
    val nR = xR
    val nC = yC
    val nK = xC
    require(yR == nK)
    val array = new Array[A](nR * nC)
    var r = 0
    while (r < nR) {
      var c = 0
      while (c < nC) {
        var acc = xa(r * xC)*ya(c) // x(r)(0)*y(0)(c)
        var k = 1
        while (k < nK) {
          acc += xa(r * xC + k)*ya(k * yC + c) // x(r)(k)*y(k)(c)
          k += 1
        }
        array(r * nC + c) = acc
        c += 1
      }
      r += 1
    }
    new DenseM(nR, nC, array)
  }
}

trait DenseMatVecInField[@sp(Double, Long) A] extends Any
    with DenseMatVecInRing[A]
    with MatVecInField[DenseM[A], DenseV[A], A] {
  implicit def V: VecInField[DenseV[A], A] with VecMutable[DenseV[A], A]
}

object DenseM {
  def seed = 0x3EED4E43
  implicit val forDouble = new DenseMatVecInField[Double] {
    def V = DenseV.forDouble
    def classTag = scala.reflect.classTag[Double]
    def scalar = Field[Double]
    def eqA = Eq[Double]
  }
  implicit val forRational = new DenseMatVecInField[Rational] {
    def V = DenseV.forRational
    def classTag = scala.reflect.classTag[Rational]
    def scalar = Field[Rational]
    def eqA = Eq[Rational]
  }
}
