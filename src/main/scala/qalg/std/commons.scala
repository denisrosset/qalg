package com.faacets.qalg
package std

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._

import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import org.apache.commons.math3.linear.{BlockFieldMatrix, FieldMatrix, MatrixUtils, FieldLUDecomposition}
import algebra._

trait CommonsBase[@sp(Double, Long) A, J <: FieldElement[J]] extends Any {
  implicit def jClassTag: ClassTag[J]
  implicit def eqA: Eq[A]
  implicit def scalar: AdditiveMonoid[A]
  def toJ(a: A): J
  def fromJ(j: J): A
}

trait CommonsFraction extends Any with CommonsBase[Rational, Fraction] {
  def jClassTag: ClassTag[Fraction] = implicitly[ClassTag[Fraction]]
  def eqA: Eq[Rational] = implicitly[Eq[Rational]]
  def scalar: Field[Rational] = implicitly[Field[Rational]]
  def fromJ(j: Fraction): Rational = Rational(j.getNumerator, j.getDenominator)
  def toJ(a: Rational): Fraction = {
    val num = a.numerator
    val den = a.denominator
    require(num.isValidInt && den.isValidInt)
    new Fraction(a.numerator.toInt, a.denominator.toInt)
  }
}

trait CommonsBigFraction extends Any with CommonsBase[Rational, BigFraction] {
  def jClassTag: ClassTag[BigFraction] = implicitly[ClassTag[BigFraction]]
  def eqA: Eq[Rational] = implicitly[Eq[Rational]]
  def scalar: Field[Rational] = implicitly[Field[Rational]]
  def fromJ(j: BigFraction): Rational = Rational(j.getNumerator, j.getDenominator)
  def toJ(a: Rational): BigFraction = new BigFraction(a.numerator.bigInteger, a.denominator.bigInteger)
}

trait CommonsFieldMatrixMat[A, J <: FieldElement[J]] extends Any
    with CommonsBase[A, J]
    with Mat[FieldMatrix[J], A]
    with MatInField[FieldMatrix[J], A]
    with MatMutable[FieldMatrix[J], A]
    with MatInFieldAlg[FieldMatrix[J], A] {
  type M = FieldMatrix[J]
  def nRows(m: M): Int = m.getRowDimension
  def nCols(m: M): Int = m.getColumnDimension
  def apply(m: M, r: Int, c: Int): A = fromJ(m.getEntry(r, c))
  def update(m: M, r: Int, c: Int, a: A): Unit = m.setEntry(r, c, toJ(a))
  def from(m: FunM[A]): M = MatrixUtils.createFieldMatrix(Array.tabulate[J](m.nR, m.nC) { (r, c) => toJ(m.f(r, c)) })
  override def plus(x: M, y: M): M = x.add(y)
  override def minus(x: M, y: M): M = x.subtract(y)
  override def times(x: M, y: M): M = x.multiply(y)
  override def timesl(a: A, m: M): M = m.scalarMultiply(toJ(a))
  def trace(m: M): A = fromJ(m.getTrace)
  def det(m: M): A = fromJ((new FieldLUDecomposition(m)).getDeterminant)
  def inv(m: M): M = (new FieldLUDecomposition(m)).getSolver.getInverse
  override def pinv(m: M) = (new FieldLUDecomposition(m)).getSolver.getInverse
  def rank(m: M): Int = throw new UnsupportedOperationException("rank not implemented")
  def rref(m: M): M = throw new UnsupportedOperationException("rref not implemented")
}

final class CommonsFieldMatrixFractionMat extends CommonsFieldMatrixMat[Rational, Fraction] with CommonsFraction

final class CommonsFieldMatrixBigFractionMat extends CommonsFieldMatrixMat[Rational, BigFraction] with CommonsBigFraction

trait CommonsInstances {
  implicit val CommonsFieldMatrixFractionMat: CommonsFieldMatrixFractionMat =
    new CommonsFieldMatrixFractionMat
  implicit val CommonsFieldMatrixBigFractionMat: CommonsFieldMatrixBigFractionMat =
    new CommonsFieldMatrixBigFractionMat
}
