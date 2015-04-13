package com.faacets.qalg
package std

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._

import org.jscience.mathematics.number.{Rational => JRational}
import org.jscience.mathematics.number.{Float64, LargeInteger}
import org.jscience.mathematics.vector.{Matrix => JMatrix, Vector => JVector, Float64Vector, DenseVector => JDenseVector, DenseMatrix => JDenseMatrix, Float64Matrix}
import org.jscience.mathematics.structure.{Field => JField}

import algebra._

final class JScienceFloat64Field extends DoubleField[Float64] {
  def toDouble(j: Float64): Double = j.doubleValue
  override def fromDouble(a: Double): Float64 = Float64.valueOf(a)

  // Order
  override def eqv(x: Float64, y: Float64): Boolean = (x == y)
  def compare(x: Float64, y: Float64): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: Float64 = Float64.ZERO
  def plus(x: Float64, y: Float64): Float64 = x.plus(y)
  override def minus(x: Float64, y: Float64): Float64 = x.minus(y)
  def negate(x: Float64): Float64 = x.opposite

  // MultiplicativeGroup
  def one: Float64 = Float64.ONE
  def times(x: Float64, y: Float64): Float64 = x.times(y)
  override def reciprocal(x: Float64): Float64 = x.inverse
  def div(x: Float64, y: Float64): Float64 = x.divide(y)

  // Ring
  override def fromInt(n: Int): Float64 = Float64.valueOf(n)
}

final class JScienceRationalField extends RationalField[JRational] {
  // Order
  override def eqv(x: JRational, y: JRational): Boolean = (x == y)
  def compare(x: JRational, y: JRational): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: JRational = JRational.ZERO
  def plus(x: JRational, y: JRational): JRational = x.plus(y)
  override def minus(x: JRational, y: JRational): JRational = x.minus(y)
  def negate(x: JRational): JRational = x.opposite

  // MultiplicativeGroup
  def one: JRational = JRational.ONE
  def times(x: JRational, y: JRational): JRational = x.times(y)
  override def reciprocal(x: JRational): JRational = x.inverse
  def div(x: JRational, y: JRational): JRational = x.divide(y)

  // Ring
  override def fromInt(n: Int): JRational = JRational.valueOf(n, 1L)

  import jscience._
  import JScienceLargeIntegerRing.{toBigInt, fromBigInt}

  def numerator(x: JRational): BigInt = toBigInt(x.getDividend)
  def denominator(x: JRational): BigInt = toBigInt(x.getDivisor)
  def ratio(numerator: BigInt, denominator: BigInt): JRational =
    JRational.valueOf(fromBigInt(numerator), fromBigInt(denominator))
}

final class JScienceLargeIntegerRing extends EuclideanRing[LargeInteger] with Order[LargeInteger] {
  def toBigInt(x: LargeInteger): BigInt = {
    val nBits = x.bitLength
    val bytes = new Array[Byte](scala.math.max(1, (nBits + 7) / 8))
    val written = x.toByteArray(bytes, 0)
    BigInt(new java.math.BigInteger(bytes))
  }
  def fromBigInt(x: BigInt): LargeInteger = LargeInteger.valueOf(x.bigInteger)

  override def eqv(x: LargeInteger, y: LargeInteger): Boolean = (x == y)
  def compare(x: LargeInteger, y: LargeInteger): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: LargeInteger = LargeInteger.ZERO
  def plus(x: LargeInteger, y: LargeInteger): LargeInteger = x.plus(y)
  override def minus(x: LargeInteger, y: LargeInteger): LargeInteger = x.minus(y)
  def negate(x: LargeInteger): LargeInteger = x.opposite

  // MultiplicativeGroup
  def one: LargeInteger = LargeInteger.ONE
  def times(x: LargeInteger, y: LargeInteger): LargeInteger = x.times(y)

  // Ring
  override def fromInt(n: Int): LargeInteger = LargeInteger.valueOf(n)

  // EuclideanRIng
  def gcd(a: LargeInteger, b: LargeInteger): LargeInteger = a.gcd(b)
  def mod(a: LargeInteger, b: LargeInteger): LargeInteger = a.mod(b)
  def quot(a: LargeInteger, b: LargeInteger): LargeInteger = a.divide(b)
}

final class JScienceFloat64Vec(implicit val eqA: Eq[Float64], val scalar: Field[Float64]) extends VecInField[Float64Vector, Float64] {
  type V = Float64Vector
  type A = Float64
  def apply(v: V, k: Int) = v.get(k)
  def length(v: V) = v.getDimension
  override def plus(x: V, y: V): V = x.plus(y)
  override def minus(x: V, y: V): V = x.minus(y)
  override def negate(v: V): V = v.opposite
  override def timesl(a: Float64, v: V): V = v.times(a)
  def tabulate(n: Int)(f: Int => A): V = Float64Vector.valueOf(new java.util.AbstractList[Float64] {
    def get(k: Int) = f(k)
    def size = n
  })
  def copy(v: V): V = v.copy
}

final class JScienceFloat64MatVec(implicit val eqA: Eq[Float64], val scalar: Field[Float64], val V: VecInField[Float64Vector, Float64]) extends MatVecInField[Float64Matrix, Float64Vector, Float64] {
  type M = Float64Matrix
  type V = Float64Vector
  type A = Float64
  def apply(m: M, r: Int, c: Int): A = m.get(r, c)
  def nRows(m: M): Int = m.getNumberOfRows
  def nCols(m: M): Int = m.getNumberOfColumns
  override def plus(x: M, y: M): M = x.plus(y)
  override def minus(x: M, y: M): M = x.minus(y)
  override def negate(v: M): M = v.opposite
  override def timesl(a: A, v: M): M = v.times(a)
  def fromFunM(m: FunM[A]): M = Float64Matrix.valueOf(Array.tabulate[Double](m.nR, m.nC)( (r, c) => m.f(r, c).doubleValue))
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M = Float64Matrix.valueOf(Array.tabulate[Double](nRows, nCols)( (r, c) => f(r, c).doubleValue ))
  override def times(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.times(y)
  def copy(m: M): M = m.copy
}

trait JScienceDenseVectorVec[A <: JField[A]] extends Any
    with VecInField[JDenseVector[A], A] {
  type V = JDenseVector[A]
  implicit def classTagA: ClassTag[A]
  def apply(v: V, k: Int): A = v.get(k)
  def length(v: V): Int = v.getDimension
  def tabulate(n: Int)(f: Int => A): V = JDenseVector.valueOf(new java.util.AbstractList[A] {
    def get(k: Int) = f(k)
    def size = n
  })
  override def plus(x: V, y: V): V = x.plus(y)
  override def minus(x: V, y: V): V = x.minus(y)
  override def negate(v: V): V = v.opposite
  override def timesl(a: A, v: V): V = v.times(a)
  def copy(v: V): V = v.copy
}


trait JScienceDenseMatrixMatVec[A <: JField[A]] extends Any
    with MatVecInField[JDenseMatrix[A], JDenseVector[A], A] {
  type M = JDenseMatrix[A]
  type V = JDenseVector[A]
  implicit def classTagA: ClassTag[A]
  def apply(m: M, r: Int, c: Int): A = m.get(r, c)
  def nRows(m: M): Int = m.getNumberOfRows
  def nCols(m: M): Int = m.getNumberOfColumns
  override def plus(x: M, y: M): M = x.plus(y)
  override def minus(x: M, y: M): M = x.minus(y)
  override def negate(m: M): M = m.opposite
  override def timesl(a: A, m: M): M = m.times(a)
  override def times(x: M, y: M): M = x.times(y)
  override def t(m: M): M = m.transpose
  def fromFunM(m: FunM[A]): M = JDenseMatrix.valueOf(Array.tabulate[A](m.nR, m.nC)( (r, c) => m.f(r, c)))
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M = JDenseMatrix.valueOf(Array.tabulate[A](nRows, nCols)(f))
  def copy(m: M): M = m.copy
}

trait JScienceInstances {
  implicit val JScienceFloat64Field = new JScienceFloat64Field
  implicit val JScienceLargeIntegerRing = new JScienceLargeIntegerRing
  implicit val JScienceRationalField = new JScienceRationalField

  object native {
    implicit val JScienceRationalDenseVec = new JScienceDenseVectorVec[JRational] {
      def classTagA = classTag[JRational]
      def eqA = JScienceRationalField
      def scalar = JScienceRationalField
    }

    implicit val JScienceRationalDenseMatVec = new JScienceDenseMatrixMatVec[JRational] {
      def V = JScienceRationalDenseVec
      def classTagA = classTag[JRational]
      def eqA = JScienceRationalField
      def scalar = JScienceRationalField
    }

    implicit val JScienceFloat64Vec = new JScienceFloat64Vec
    implicit val JScienceFloat64MatVec = new JScienceFloat64MatVec
  }

  object converted {
    implicit val JScienceRationalDenseVec: VecInField[JDenseVector[JRational], Rational] =
      new RationalConverted[JRational]
          with ConvertedVecInField[JDenseVector[JRational], Rational, JRational] { self =>
        def rationalFieldJ = JScienceRationalField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def source: JScienceDenseVectorVec[JRational] = native.JScienceRationalDenseVec
      }
    implicit val JScienceRationalDenseMatVec: MatVecInField[JDenseMatrix[JRational], JDenseVector[JRational], Rational] =
      new RationalConverted[JRational]
          with ConvertedMatVecInField[JDenseMatrix[JRational], JDenseVector[JRational], Rational, JRational] { self =>
        def rationalFieldJ = JScienceRationalField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def V = JScienceRationalDenseVec
        def source: JScienceDenseMatrixMatVec[JRational] = native.JScienceRationalDenseMatVec
      }
    implicit val JScienceFloat64Vec: VecInField[Float64Vector, Double] =
      new DoubleConverted[Float64]
          with ConvertedVecInField[Float64Vector, Double, Float64] {
        def doubleFieldJ = JScienceFloat64Field
        def classTagA = classTag[Double]
        def eqA = Eq[Double]
        def scalar = Field[Double]
        def source = native.JScienceFloat64Vec
      }
    implicit val JScienceFloat64MatVec: MatVecInField[Float64Matrix, Float64Vector, Double] =
      new DoubleConverted[Float64]
          with ConvertedMatVecInField[Float64Matrix, Float64Vector, Double, Float64] {
        def doubleFieldJ = JScienceFloat64Field
        def classTagA = classTag[Double]
        def eqA = Eq[Double]
        def scalar = Field[Double]
        def V = JScienceFloat64Vec
        def source = native.JScienceFloat64MatVec
      }
  }
}
