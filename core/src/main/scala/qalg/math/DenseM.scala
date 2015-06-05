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

/** Dense matrix stored in row-major order. */
final class DenseM[@sp(Double, Long) A: ClassTag, M <: Mutability](val nR: Int, val nC: Int, val array: Array[A]) {
  override def toString = math.MatrixPrinting.print(nR, nC, (r: Int, c: Int) => array(r * nC + c).toString)
  override def equals(other: Any): Boolean = other match {
    case that: DenseM[A, _] =>
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

trait DenseMat[@sp(Double, Long) A, M <: Mutability] extends Any
    with MatBuilder[DenseM[A, M], A] { self =>
  type DM = DenseM[A, M]
  implicit def ctA: ClassTag[A]


  def apply(m: DM, r: Int, c: Int): A = m.array(r * m.nC + c)
  def nRows(m: DM): Int = m.nR
  def nCols(m: DM): Int = m.nC
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): DM = {
    val array = new Array[A](nRows * nCols)
    var i = 0
    cforRange(0 until nRows) { r =>
      cforRange(0 until nCols) { c =>
        array(i) = f(r, c)
        i += 1
      }
    }
    new DenseM[A, M](nRows, nCols, array)
  }
}

trait DenseMatInRing[@sp(Double, Long) A, M <: Mutability] extends Any
    with DenseMat[A, M]
    with MatInRing[DenseM[A, M], A]
    with MatSlicerImpl[DenseM[A, M], DenseV[A, M], A]
    with MatVecProductImpl[DenseM[A, M], DenseV[A, M], A] { self =>
  def M = self
  def fromFunM(m: FunM[A]): DM = {
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
    new DenseM[A, M](nR, nC, array)
  }

  override def negate(m: DM): DM = new DenseM[A, M](m.nR, m.nC, std.ArraySupport.negate(m.array))

  override def plus(x: DM, y: DM): DM = {
    require(x.nR == y.nR && x.nC == y.nC)
    new DenseM[A, M](x.nR, x.nC, std.ArraySupport.plus(x.array, y.array))
  }

  override def minus(x: DM, y: DM): DM = {
    require(x.nR == y.nR && x.nC == y.nC)
    new DenseM[A, M](x.nR, x.nC, std.ArraySupport.minus(x.array, y.array))
  }

  override def timesl(x: A, y: DM): DM =
    new DenseM[A, M](y.nR, y.nC, std.ArraySupport.timesl(x, y.array))

  override def times(x: DM, y: DM): DM = {
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
    new DenseM[A, M](nR, nC, array)
  }
}

trait DenseMatMutable[@sp(Double, Long) A] extends Any
    with MatMutable[DenseM[A, Mutable], A] { self =>
  implicit def ctA: ClassTag[A]
  type DM = DenseM[A, Mutable]
  def update(m: DM, r: Int, c: Int, a: A): Unit = { m.array(r * m.nC + c) = a }
  def copy(m: DM): DM = new DenseM(m.nR, m.nC, m.array.clone)
}

trait DenseMatInField[@sp(Double, Long) A, M <: Mutability] extends Any
    with DenseMatInRing[A, M]
    with MatInField[DenseM[A, M], A]

object DenseM {
  def seed = 0x3EED4E43

  object longInstance extends DenseMatInRing[Long, Mutability] {
    def ctA = classTag[Long]
    def eqA = Eq[Long]
    def A = Ring[Long]
    def V = DenseV.longInstance
  }

  object doubleInstance extends DenseMatInField[Double, Mutability] {
    def ctA = classTag[Double]
    def eqA = Eq[Double]
    def A = Field[Double]
    def V = DenseV.doubleInstance
  }

  object rationalInstance extends DenseMatInField[Rational, Mutability] {
    def ctA = classTag[Rational]
    def eqA = Eq[Rational]
    def A = Field[Rational]
    def V = DenseV.rationalInstance
  }

  implicit def long[M <: Mutability]: DenseMatInRing[Long, M] =
    longInstance.asInstanceOf[DenseMatInRing[Long, M]]

  implicit def double[M <: Mutability]: DenseMatInField[Double, M] =
    doubleInstance.asInstanceOf[DenseMatInField[Double, M]]

  implicit def rational[M <: Mutability]: DenseMatInField[Rational, M] =
    rationalInstance.asInstanceOf[DenseMatInField[Rational, M]]

  implicit object longMutInstance extends DenseMatMutable[Long] {
    def M = long[Mutable]
    def ctA = classTag[Long]
  }

  implicit object doubleMutInstance extends DenseMatMutable[Double] {
    def M = double[Mutable]
    def ctA = classTag[Double]
  }

  implicit object rationalMutInstance extends DenseMatMutable[Rational] {
    def M = rational[Mutable]
    def ctA = classTag[Rational]
  }

  implicit object rationalPack extends PackMVField[DenseM[Rational, Immutable], DenseV[Rational, Immutable], Rational] {
    type MutM = DenseM[Rational, Mutable]
    type MutV = DenseV[Rational, Mutable]
    def unsafeFromMutM(m: MutM): DenseM[Rational, Immutable] = m.asInstanceOf[DenseM[Rational, Immutable]]
    def unsafeFromMutV(v: MutV): DenseV[Rational, Immutable] = v.asInstanceOf[DenseV[Rational, Immutable]]
    def unsafeToMutM(m: DenseM[Rational, Immutable]): MutM = m.asInstanceOf[MutM]
    def unsafeToMutV(v: DenseV[Rational, Immutable]): MutV = v.asInstanceOf[MutV]
    def M = rational[Immutable]
    def V = DenseV.rational[Immutable]
    def MutMutM = rationalMutInstance
    def MutMutV = DenseV.rationalMutInstance
    def MutM = rational[Mutable]
    def MutV = DenseV.rational[Mutable]
    def MVProduct = rational[Immutable]
    def MutProduct = rational[Mutable]
    def MVSlicer = rational[Immutable]
    def MutSlicer = rational[Mutable]
  }
}
