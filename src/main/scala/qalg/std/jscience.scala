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

trait JScienceBase[@sp(Double, Long) A, J <: JField[J]] extends Any {
  implicit def jClassTag: ClassTag[J]
  implicit def eqA: Eq[A]
  implicit def scalar: AdditiveMonoid[A]
  def toJ(a: A): J
  def fromJ(j: J): A
}

object JScienceConv {
  implicit class LargeIntegerConv(val lhs: LargeInteger) extends AnyVal {
    def toBigInt: BigInt = ???
  }
  implicit class BigIntegerConv(val lhs: BigInt) extends AnyVal {
    def toLargeInteger: LargeInteger = ???
  }
}

trait JScienceRational extends Any with JScienceBase[Rational, JRational] {
  import JScienceConv._
  def jClassTag: ClassTag[JRational] = implicitly[ClassTag[JRational]]
  def eqA: Eq[Rational] = implicitly[Eq[Rational]]
  def scalar: Field[Rational] = implicitly[Field[Rational]]
  def fromJ(j: JRational): Rational = Rational(j.getDividend.toBigInt, j.getDivisor.toBigInt)
  def toJ(a: Rational): JRational = JRational.valueOf(a.numerator.toLargeInteger, a.denominator.toLargeInteger)
}

trait JScienceDouble extends Any with JScienceBase[Double, Float64] {
  def jClassTag: ClassTag[Float64] = implicitly[ClassTag[Float64]]
  def eqA: Eq[Double] = implicitly[Eq[Double]]
  def scalar: Field[Double] = implicitly[Field[Double]]
  def fromJ(j: Float64): Double = j.doubleValue
  def toJ(a: Double): Float64 = Float64.valueOf(a)
}

trait JScienceVec[@sp(Double, Long) A, J <: JField[J], JV <: JVector[J]] extends Any
    with JScienceBase[A, J]
    with VecInField[JV, A] {

  def apply(v: JV, k: Int): A = fromJ(v.get(k))
  def length(v: JV): Int = v.getDimension
}

trait JScienceMat[@sp(Double, Long) A, J <: JField[J], JM <: JMatrix[J]] extends Any
    with JScienceBase[A, J]
    with MatInField[JM, A]
    with MatInFieldAlg[JM, A] {
  def fromGenMatrix(m: JMatrix[J]): JM
  def apply(m: JM, r: Int, c: Int): A = fromJ(m.get(r, c))
  def nRows(m: JM): Int = m.getNumberOfRows
  def nCols(m: JM): Int = m.getNumberOfColumns
  def pinv(m: JM): JM = fromGenMatrix(m.pseudoInverse)
  def inv(m: JM): JM = fromGenMatrix(m.inverse)
  def det(m: JM): A = fromJ(m.determinant)
  def trace(m: JM): A = fromJ(m.trace)
  override def op(x: JM, y: JM): JM = fromGenMatrix(x.tensor(y))
  def rref(m: JM): JM = throw new UnsupportedOperationException("rref not implemented")
  def rank(m: JM): Int = throw new UnsupportedOperationException("rank not implemented")
}

final class JScienceFloat64MatrixMat extends JScienceMat[Double, Float64, Float64Matrix] with JScienceDouble {
  def fromGenMatrix(m: JMatrix[Float64]): Float64Matrix = Float64Matrix.valueOf(m)
  override def plus(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.plus(y)
  override def minus(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.minus(y)
  override def negate(v: Float64Matrix): Float64Matrix = v.opposite
  override def timesl(a: Double, v: Float64Matrix): Float64Matrix = v.times(toJ(a))
  // TODO: optimize conversion
  def from(m: FunM[Double]): Float64Matrix = Float64Matrix.valueOf(Array.tabulate[Double](m.nR, m.nC)( (r, c) => m.f(r, c)))
  override def times(x: Float64Matrix, y: Float64Matrix): Float64Matrix = x.times(y)
  override def inv(m: Float64Matrix): Float64Matrix = m.inverse
}

final class JScienceFloat64VectorVec extends JScienceVec[Double, Float64, Float64Vector] with JScienceDouble {
  override def plus(x: Float64Vector, y: Float64Vector): Float64Vector = x.plus(y)
  override def minus(x: Float64Vector, y: Float64Vector): Float64Vector = x.minus(y)
  override def negate(v: Float64Vector): Float64Vector = v.opposite
  override def timesl(a: Double, v: Float64Vector): Float64Vector = v.times(toJ(a))
  def from(v: FunV[Double]): Float64Vector = Float64Vector.valueOf(new java.util.AbstractList[Float64] {
    def get(k: Int) = toJ(v.f(k))
    def size = v.len
  })
}

trait JScienceDenseVectorVec[@sp(Double, Long) A, J <: JField[J]] extends Any
    with JScienceVec[A, J, JDenseVector[J]] {
  override def plus(x: JDenseVector[J], y: JDenseVector[J]): JDenseVector[J] = x.plus(y)
  override def minus(x: JDenseVector[J], y: JDenseVector[J]): JDenseVector[J] = x.minus(y)
  override def negate(v: JDenseVector[J]): JDenseVector[J] = v.opposite
  override def timesl(a: A, v: JDenseVector[J]): JDenseVector[J] = v.times(toJ(a))
  def from(v: FunV[A]): JDenseVector[J] = JDenseVector.valueOf(new java.util.AbstractList[J] {
    def get(k: Int) = toJ(v.f(k))
    def size = v.len
  })
}

trait JScienceDenseMatrixMat[@sp(Double, Long) A, J <: JField[J]] extends Any
    with JScienceMat[A, J, JDenseMatrix[J]] {
  def fromGenMatrix(m: JMatrix[J]): JDenseMatrix[J] = JDenseMatrix.valueOf(m)

  override def plus(x: JDenseMatrix[J], y: JDenseMatrix[J]): JDenseMatrix[J] = x.plus(y)
  override def minus(x: JDenseMatrix[J], y: JDenseMatrix[J]): JDenseMatrix[J] = x.minus(y)
  override def negate(m: JDenseMatrix[J]): JDenseMatrix[J] = m.opposite
  override def timesl(a: A, m: JDenseMatrix[J]): JDenseMatrix[J] = m.times(toJ(a))
  override def times(x: JDenseMatrix[J], y: JDenseMatrix[J]): JDenseMatrix[J] = x.times(y)
  override def t(m: JDenseMatrix[J]): JDenseMatrix[J] = m.transpose
  def from(m: FunM[A]): JDenseMatrix[J] = JDenseMatrix.valueOf(Array.tabulate[J](m.nR, m.nC)( (r, c) => toJ(m.f(r, c))))
  override def inv(m: JDenseMatrix[J]): JDenseMatrix[J] = m.inverse
}

final class JScienceDenseVectorRationalVec extends JScienceDenseVectorVec[Rational, JRational] with JScienceRational

final class JScienceDenseMatrixRationalMat extends JScienceDenseMatrixMat[Rational, JRational] with JScienceRational

trait JScienceInstances {
  implicit val JScienceFloat64Vector: JScienceFloat64VectorVec = new JScienceFloat64VectorVec
  implicit val JScienceDenseVectorRational: JScienceDenseVectorRationalVec = new JScienceDenseVectorRationalVec
}
