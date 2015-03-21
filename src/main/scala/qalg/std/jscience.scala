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

final class JScienceFloat64Field extends Field[Float64] with Order[Float64] {
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

  // EuclideanRing

  def gcd(x: Float64, y: Float64): Float64 =
    Float64.valueOf(Field[Double].gcd(x.doubleValue, y.doubleValue))
  def mod(x: Float64, y: Float64): Float64 =
    Float64.valueOf(Field[Double].mod(x.doubleValue, y.doubleValue))
  def quot(x: Float64, y: Float64): Float64 =
    Float64.valueOf(Field[Double].mod(x.doubleValue, y.doubleValue))
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

  import jScience._
  import JScienceLargeIntegerRing.{toBigInt, fromBigInt}

  def numerator(x: JRational): BigInt = toBigInt(x.getDividend)
  def denominator(x: JRational): BigInt = toBigInt(x.getDivisor)
  def ratio(numerator: BigInt, denominator: BigInt): JRational =
    JRational.valueOf(fromBigInt(numerator), fromBigInt(denominator))
}

final class JScienceLargeIntegerRing extends EuclideanRing[LargeInteger] with Order[LargeInteger] {
  def toBigInt(x: LargeInteger): BigInt = {
    val nBits = x.bitLength
    val bytes = new Array[Byte]((nBits + 7) / 8)
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
  def from(v: FunV[Float64]): V = Float64Vector.valueOf(new java.util.AbstractList[Float64] {
    def get(k: Int) = v.f(k)
    def size = v.len
  })
}

final class JScienceFloat64MatVec(implicit val eqA: Eq[Float64], val scalar: Field[Float64], val V: VecInField[Float64Vector, Float64]) extends MatVecInField[Float64Matrix, Float64Vector, Float64] {
  type M = Float64Matrix
  type V = Float64Vector
  type A = Float64
  def apply(m: M, r: Int, c: Int): A = m.get(r, c)
  def nRows(m: M): Int = m.getNumberOfRows
  def nCols(m: M): Int = m.getNumberOfColumns
  override def plus(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.plus(y)
  override def minus(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.minus(y)
  override def negate(v: Float64Matrix): Float64Matrix = v.opposite
  override def timesl(a: Float64, v: Float64Matrix): Float64Matrix = v.times(a)
  def from(m: FunM[Float64]): Float64Matrix = Float64Matrix.valueOf(Array.tabulate[Double](m.nR, m.nC)( (r, c) => m.f(r, c).doubleValue))
  override def times(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.times(y)
}

trait JScienceDenseVectorVec[A <: JField[A]] extends Any
    with VecInField[JDenseVector[A], A] {
  type V = JDenseVector[A]
  implicit def classTagA: ClassTag[A]
  def apply(v: V, k: Int): A = v.get(k)
  def length(v: V): Int = v.getDimension
  def from(v: FunV[A]): JDenseVector[A] = JDenseVector.valueOf(new java.util.AbstractList[A] {
    def get(k: Int) = v.f(k)
    def size = v.len
  })
  override def plus(x: V, y: V): V = x.plus(y)
  override def minus(x: V, y: V): V = x.minus(y)
  override def negate(v: V): V = v.opposite
  override def timesl(a: A, v: V): V = v.times(a)
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
  def from(m: FunM[A]): M = JDenseMatrix.valueOf(Array.tabulate[A](m.nR, m.nC)( (r, c) => m.f(r, c)))
}

trait JScienceInstances {
  implicit val JScienceFloat64Field = new JScienceFloat64Field
  implicit val JScienceLargeIntegerRing = new JScienceLargeIntegerRing
  implicit val JScienceRationalField = new JScienceRationalField

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
  implicit val JScienceFloat64MatVec = new JScienceFloat64Vec
}
