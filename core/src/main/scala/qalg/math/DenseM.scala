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

object DenseM {
  def seed = 0x3EED4E43

  def fromFunM[@sp(Double, Long) A: ClassTag, M <: Mutability](m: FunM[A]): DenseM[A, M] = {
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

final class DenseMatInRing[@sp(Double, Long) A, M <: Mutability]
  /*

}

trait DenseMat[@sp(Double, Long) A, M <: Mutability] extends Any
    with MatBuilder[DenseM[A, M], A] { self =>
  type DM = DenseM[A, M]
  implicit def ctA: ClassTag[A]

}

trait DenseMatSlicer[@sp(Double, Long) A, M <: Mutability] extends Any {

}

trait DenseMatInRing[@sp(Double, Long) A, M <: Mutability] extends Any
    with DenseMat[A, M]
    with MatInRing[DenseM[A, M], A]
    with MatSlicerImpl[DenseM[A, M], DenseV[A, M], A]
    with MatVecProductImpl[DenseM[A, M], DenseV[A, M], A] { self =>
  def M = self
  def fromFunM(m: FunM[A]): DM = {



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

  lazy val longInstance = new DenseMatInRing[Long, Mutability]
      with MatInEuclideanRing[DenseM[Long, Mutability], Long]
      with MatVecProduct[DenseM[Long, Mutability], DenseV[Long, Mutablity]]
  with MatSlicer[{
    def ctA = classTag[Long]
    def A = EuclideanRing[Long]
    def eqA = Eq[Long]
    def V = DenseV.longInstance
  }

  lazy val doubleInstance = new DenseMatInField[Double, Mutability] {
    def ctA = classTag[Double]
    def eqA = Eq[Double]
    def A = Field[Double]
    def V = DenseV.doubleInstance
  }

  lazy val rationalInstance = new DenseMatInField[Rational, Mutability] {
    def ctA = classTag[Rational]
    def eqA = Eq[Rational]
    def A = Field[Rational]
    def V = DenseV.rationalInstance
  }

  implicit def long[M <: Mutability]: MatInEuclideanRing[DenseM[Long, M], Long] =
    longInstance.asInstanceOf[MatInEuclideanRing[DenseM[Long, M], Long]]

  implicit def double[M <: Mutability]: MatInField[DenseM[Double, M], Double] =
    doubleInstance.asInstanceOf[MatInField[DenseM[Double, M], Double]]

  implicit def rational[M <: Mutability]: MatInField[DenseM[Rational, M], Rational] =
    rationalInstance.asInstanceOf[MatInField[DenseM[Rational, M], Rational]]

  implicit lazy val longMutable = new DenseMatMutable[Long] {
    def M = long[Mutable]
    def ctA = classTag[Long]
  }

  implicit lazy val doubleMutable = new DenseMatMutable[Double] {
    def M = double[Mutable]
    def ctA = classTag[Double]
  }

  implicit lazy val rationalMutable = new DenseMatMutable[Rational] {
    def M = rational[Mutable]
    def ctA = classTag[Rational]
  }
/*
  implicit object rationalPack extends AlgMVFieldImpl[DenseM[Rational, Immutable], DenseV[Rational, Immutable], Rational] {
    type MutM = DenseM[Rational, Mutable]
    type MutV = DenseV[Rational, Mutable]
    def classTagMutM = classTag[MutM]
    def pivotA = Pivot.rational
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
 */
  class DenseMConv[@sp(Double, Long) A](implicit val UM: MatMutable[DenseM[A, Mutable], A]) extends ConvM[DenseM[A, Immutable], DenseM[A, Mutable]] {
    type IM = DenseM[A, Immutable]
    type UM = DenseM[A, Mutable]
    def unsafeToIM(m: UM): IM = m.asInstanceOf[IM]
    def unsafeToUM(m: IM): UM = m.asInstanceOf[UM]
  }

  implicit lazy val longConv = new DenseMConv[Long]
  implicit lazy val doubleConv = new DenseMConv[Double]
  implicit lazy val rationalConv = new DenseMConv[Rational]
  implicit lazy val longMutableAlg: AlgUMVR[DenseM[Long, Mutable], DenseV[Long, Mutable], Long] = new AlgUMVR[DenseM[Long, Mutable], DenseV[Long, Mutable], Long] {
    def classTagM = classTag[M]
    type A = Long
    def M = long[Mutable]
    def V = DenseV.long[Mutable]
    def UM: MatMutable[M, A] = longMutable
    def UV: VecMutable[V, A] = DenseV.longMutable
    def MVProduct = long[Mutable]
    def MVSlicer = long[Mutable]
    implicit val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
    implicit val MCat: MatCat[M, A] = new MatCatImpl[M, A]
    implicit val MKron: Kron[M] = new MatKronImpl[M, A]
    implicit val MShift: MutableMatShift[M] = new MutableMatShiftImpl[M, A]
    implicit val MDeterminant: Determinant[M, A] = new DeterminantMutableRingImpl[M, A]
    implicit val MTrace: Trace[M, A] = new TraceImpl[M, A]
    implicit val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
    implicit val VCat: VecCat[V, A] = new VecCatImpl[V, A]
    implicit val VKron: Kron[V] = new VecKronImpl[V, A]
    implicit val VShift: MutableVecShift[V] = new MutableVecShiftImpl[V, A]
  }
  implicit lazy val longImmutableAlg: AlgMVR[DenseM[Long, Immutable], DenseV[Long, Immutable], Long] = new AlgMVR[DenseM[Long, Immutable], DenseV[Long, Immutable], Long] {
    type A = Long
    def M = long[Immutable]
    def V = DenseV.long[Immutable]
    def MVProduct = long[Immutable]
    def MVSlicer = long[Immutable]
    implicit val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
    implicit val MCat: MatCat[M, A] = new MatCatImpl[M, A]
    implicit val MKron: Kron[M] = new MatKronImpl[M, A]
    implicit val MShift: MatShift[M] = new MatShiftImpl[M, A]
    implicit val MDeterminant: Determinant[M, A] = new Determinant[M, A] {
      def determinant(m: M): A = longMutableAlg.MDeterminant.determinant(longConv.unsafeToUM(m))
    }
    implicit val MTrace: Trace[M, A] = new TraceImpl[M, A]
    implicit val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
    implicit val VCat: VecCat[V, A] = new VecCatImpl[V, A]
    implicit val VKron: Kron[V] = new VecKronImpl[V, A]
    implicit val VShift: VecShift[V] = new VecShiftImpl[V, A]
  }
}
 */
