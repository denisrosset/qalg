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
import algos._

/** Dense matrix stored in row-major order. */
final class DenseM[@sp(Double, Long) A: ClassTag, M <: Mutability](val nRows: Int, val nCols: Int, val array: Array[A]) { lhs =>
  override def toString = math.MatrixPrinting.print(nRows, nCols, (r: Int, c: Int) => array(r * nCols + c).toString)
  override def equals(other: Any): Boolean = other match {
    case that: DenseM[A, _] =>
      (this.nRows == that.nRows) && (this.nCols == that.nCols) && {
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
    cforRange(0 until nRows * nCols) { k =>
      h = MurmurHash3.mix(h, array(k).##)
    }
    h = MurmurHash3.mix(h, nRows)
    MurmurHash3.finalizeHash(h, nCols)
  }
  type DM = DenseM[A, M]
  def unary_-(implicit A: Ring[A]): DM = new DenseM[A, M](nRows, nCols, std.ArraySupport.negate(array))

  @inline def +(rhs: DM)(implicit A: Ring[A]): DM = {
    require(lhs.nRows == rhs.nRows && lhs.nCols == rhs.nCols)
    new DenseM[A, M](lhs.nRows, lhs.nCols, std.ArraySupport.plus(lhs.array, rhs.array))
  }

  @inline def -(rhs: DM)(implicit A: Ring[A]): DM = {
    require(lhs.nRows == rhs.nRows && lhs.nCols == rhs.nCols)
    new DenseM[A, M](lhs.nRows, lhs.nCols, std.ArraySupport.minus(lhs.array, rhs.array))
  }

  @inline def *:(lhs: A)(implicit A: Ring[A]): DM =
    new DenseM[A, M](nRows, nCols, std.ArraySupport.timesl(lhs, array))

  @inline def :*(rhs: A)(implicit A: Ring[A]): DM =
    new DenseM[A, M](nRows, nCols, std.ArraySupport.timesr(array, rhs))

  @inline def at(r: Int, c: Int): A = array(r * nCols + c)

  @inline def *(rhs: DM)(implicit A: Ring[A]): DM = {
    val xR = lhs.nRows
    val yR = rhs.nRows
    val xC = lhs.nCols
    val yC = rhs.nCols
    val xa = lhs.array
    val ya = rhs.array
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

  @inline def *::(lhs: DenseV[A, M])(implicit A: Ring[A]): DenseV[A, M] = {
    require(nRows == lhs.array.length)
    val array = new Array[A](nCols)
    cforRange(0 until nCols) { c =>
      var acc = A.zero
      cforRange(0 until nRows) { r =>
        acc += lhs.array(r) * at(r, c)
      }
      acc
    }
    new DenseV[A, M](array)
  }

  @inline def ::*(rhs: DenseV[A, M])(implicit V: Vec[DenseV[A, M], A], A: Ring[A]): DenseV[A, M] = {
    require(nCols == rhs.array.length)
    val array = new Array[A](nRows)
    cforRange(0 until nRows) { r =>
      var acc = A.zero
      cforRange(0 until nCols) { c =>
        acc += at(r, c) * rhs.array(c)
      }
      acc
    }
    new DenseV[A, M](array)
  }
}

final class DenseMInRing[@sp(Long) A: ClassTag, Mut <: Mutability](implicit val A: Ring[A], val eqA: Eq[A]) extends MatInRing[DenseM[A, Mut], A] {
  type M = DenseM[A, Mut]
  def apply(m: M, r: Int, c: Int): A = m.at(r, c)
  override def negate(m: M): M = -m
  override def plus(x: M, y: M): M = x + y
  override def minus(x: M, y: M): M = x - y
  override def timesl(x: A, y: M): M = x *: y
  override def timesr(x: M, y: A): M = x :* y
  override def times(x: M, y: M): M = x * y
  def tabulate(nR: Int, nC: Int)(f: (Int, Int) => A): M = DenseM.tabulate[A, Mut](nR, nC)(f)
  def nRows(m: M): Int = m.nRows
  def nCols(m: M): Int = m.nCols
}

final class DenseMInField[@sp(Double) A: ClassTag, Mut <: Mutability](implicit val A: Field[A], val eqA: Eq[A]) extends MatInField[DenseM[A, Mut], A] {
  type M = DenseM[A, Mut]
  def apply(m: M, r: Int, c: Int): A = m.at(r, c)
  override def negate(m: M): M = -m
  override def plus(x: M, y: M): M = x + y
  override def minus(x: M, y: M): M = x - y
  override def timesl(x: A, y: M): M = x *: y
  override def timesr(x: M, y: A): M = x :* y
  override def times(x: M, y: M): M = x * y
  def tabulate(nR: Int, nC: Int)(f: (Int, Int) => A): M = DenseM.tabulate[A, Mut](nR, nC)(f)
  def nRows(m: M): Int = m.nRows
  def nCols(m: M): Int = m.nCols
}

final class DenseMMutable[@sp(Double, Long) A](implicit val M: Mat[DenseM[A, Mutable], A], val ctA: ClassTag[A]) extends MatMutable[DenseM[A, Mutable], A] { self =>
  type DM = DenseM[A, Mutable]
  def update(m: DM, r: Int, c: Int, a: A): Unit = { m.array(r * m.nCols + c) = a }
  def copy(m: DM): DM = new DenseM(m.nRows, m.nCols, m.array.clone)
}

final class DenseMatVec[@sp(Double, Long) A, Mut <: Mutability](implicit val M: Mat[DenseM[A, Mut], A], val V: VecInRing[DenseV[A, Mut], A], val A: Ring[A]) extends MatVecProductImpl[DenseM[A, Mut], DenseV[A, Mut], A] with MatSlicerImpl[DenseM[A, Mut], DenseV[A, Mut], A] {
  type M = DenseM[A, Mut]
  type V = DenseV[A, Mut]
  override def timesl2(x: V, y: M): V = x *:: y
  override def timesr2(x: M, y: V): V = x ::* y
}

final class DenseMConv[@sp(Double, Long) A](implicit val UM: MatMutable[DenseM[A, Mutable], A]) extends ConvM[DenseM[A, Immutable], DenseM[A, Mutable], A] {
  type IM = DenseM[A, Immutable]
  type UM = DenseM[A, Mutable]
  def toUM(m: IM): UM = UM.copy(unsafeToUM(m))
  def toIM(m: UM): IM = unsafeToIM(UM.copy(m))
  def unsafeToIM(m: UM): IM = m.asInstanceOf[IM]
  def unsafeToUM(m: IM): UM = m.asInstanceOf[UM]
}

object DenseM {
  val longInstance: MatInRing[DenseM[Long, Mutability], Long] = new DenseMInRing[Long, Mutability]
  val doubleInstance: MatInField[DenseM[Double, Mutability], Double] = new DenseMInField[Double, Mutability]
  val rationalInstance: MatInField[DenseM[Rational, Mutability], Rational] = new DenseMInField[Rational, Mutability]

  implicit def long[M <: Mutability]: MatInRing[DenseM[Long, M], Long] =
    longInstance.asInstanceOf[MatInRing[DenseM[Long, M], Long]]
  implicit def double[M <: Mutability]: MatInField[DenseM[Double, M], Double] =
    doubleInstance.asInstanceOf[MatInField[DenseM[Double, M], Double]]
  implicit def rational[M <: Mutability]: MatInField[DenseM[Rational, M], Rational] =
    rationalInstance.asInstanceOf[MatInField[DenseM[Rational, M], Rational]]

  implicit val longMutableInstance: MatMutable[DenseM[Long, Mutable], Long] = new DenseMMutable[Long]
  implicit val doubleMutableInstance: MatMutable[DenseM[Double, Mutable], Double] = new DenseMMutable[Double]
  implicit val rationalMutableInstance: MatMutable[DenseM[Rational, Mutable], Rational] = new DenseMMutable[Rational]

  type MatVec[A, Mut <: Mutability] = MatVecProduct[DenseM[A, Mut], DenseV[A, Mut]] with MatSlicer[DenseM[A, Mut], DenseV[A, Mut]]

  val longMVInstance: MatVec[Long, Mutability] = new DenseMatVec[Long, Mutability]
  val doubleMVInstance: MatVec[Double, Mutability] = new DenseMatVec[Double, Mutability]
  val rationalMVInstance: MatVec[Rational, Mutability] = new DenseMatVec[Rational, Mutability]

  implicit def longMV[M <: Mutability]: MatVec[Long, M] =
    longMVInstance.asInstanceOf[MatVec[Long, M]]
  implicit def doubleMV[M <: Mutability]: MatVec[Double, M] =
    doubleMVInstance.asInstanceOf[MatVec[Double, M]]
  implicit def rationalMV[M <: Mutability]: MatVec[Rational, M] =
    rationalMVInstance.asInstanceOf[MatVec[Rational, M]]

  implicit val longMutableAlg: AlgUMVE[DenseM[Long, Mutable], DenseV[Long, Mutable], Long] = new AlgUMVEImpl[DenseM[Long, Mutable], DenseV[Long, Mutable], Long]
  implicit val doubleMutableAlg: AlgUMVF[DenseM[Double, Mutable], DenseV[Double, Mutable], Double] = new AlgUMVFImpl[DenseM[Double, Mutable], DenseV[Double, Mutable], Double]
  implicit val rationalMutableAlg: AlgUMVF[DenseM[Rational, Mutable], DenseV[Rational, Mutable], Rational] = new AlgUMVFImpl[DenseM[Rational, Mutable], DenseV[Rational, Mutable], Rational]

  implicit val longConv: ConvM[DenseM[Long, Immutable], DenseM[Long, Mutable], Long] = new DenseMConv[Long]
  implicit val doubleConv: ConvM[DenseM[Double, Immutable], DenseM[Double, Mutable], Double] = new DenseMConv[Double]
  implicit val rationalConv: ConvM[DenseM[Rational, Immutable], DenseM[Rational, Mutable], Rational] = new DenseMConv[Rational]

  implicit val longImmutableAlg: AlgMVE[DenseM[Long, Immutable], DenseV[Long, Immutable], Long] = new AlgMVEImpl[DenseM[Long, Immutable], DenseV[Long, Immutable], Long, DenseM[Long, Mutable]]
  implicit val doubleImmutableAlg: AlgMVF[DenseM[Double, Immutable], DenseV[Double, Immutable], Double] = new AlgMVFImpl[DenseM[Double, Immutable], DenseV[Double, Immutable], Double, DenseM[Double, Mutable], DenseV[Double, Mutable]]
  implicit val rationalImmutableAlg: AlgMVF[DenseM[Rational, Immutable], DenseV[Rational, Immutable], Rational] = new AlgMVFImpl[DenseM[Rational, Immutable], DenseV[Rational, Immutable], Rational, DenseM[Rational, Mutable], DenseV[Rational, Mutable]]

  def seed = 0x3EED4E43

  def tabulate[@sp(Double, Long) A: ClassTag, M <: Mutability](nRows: Int, nCols: Int)(f: (Int, Int) => A): DenseM[A, M] = {
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
