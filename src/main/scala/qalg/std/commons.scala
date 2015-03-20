package com.faacets.qalg
package std

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._

import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.fraction.{BigFraction, Fraction}
import org.apache.commons.math3.linear.{BlockFieldMatrix, FieldMatrix, FieldVector,
  MatrixUtils, FieldLUDecomposition}
import algebra._

trait CommonsBase[A, J <: FieldElement[J]] extends Any {
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

trait CommonsVec[A, J <: FieldElement[J]] extends Any
    with CommonsBase[A, J]
    with VecInField[FieldVector[J], A]
    with VecMutable[FieldVector[J], A] { self =>
  type V = FieldVector[J]
  implicit def V: Vec[V, A] = self

  def length(v: V): Int = v.getDimension
  def apply(v: V, k: Int): A = fromJ(v.getEntry(k))
  def from(v: FunV[A]): V = MatrixUtils.createFieldVector(Array.tabulate[J](v.len) { k => toJ(v.f(k)) })
  def update(v: V, k: Int, a: A): Unit = v.setEntry(k, toJ(a))
}

trait CommonsMatVec[A, J <: FieldElement[J]] extends Any
    with CommonsBase[A, J]
    with MatVecInField[FieldMatrix[J], FieldVector[J], A]
    with MatVecMutable[FieldMatrix[J], FieldVector[J], A] { self =>
  type M = FieldMatrix[J]
  type V = FieldVector[J]
  implicit def M: MatInField[FieldMatrix[J], A] with MatMutable[FieldMatrix[J], A] = self
  implicit def V: VecInField[FieldVector[J], A] with VecMutable[FieldVector[J], A]
  def nRows(m: M): Int = m.getRowDimension
  def nCols(m: M): Int = m.getColumnDimension
  def apply(m: M, r: Int, c: Int): A = fromJ(m.getEntry(r, c))
  def update(m: M, r: Int, c: Int, a: A): Unit = m.setEntry(r, c, toJ(a))
  def from(m: FunM[A]): M = MatrixUtils.createFieldMatrix(Array.tabulate[J](m.nR, m.nC) { (r, c) => toJ(m.f(r, c)) })
  override def plus(x: M, y: M): M = x.add(y)
  override def minus(x: M, y: M): M = x.subtract(y)
  override def times(x: M, y: M): M = x.multiply(y)
  override def timesl(a: A, m: M): M = m.scalarMultiply(toJ(a))
}

trait CommonsInstances {
  implicit val FractionMV = new CommonsMatVec[Rational, Fraction] with CommonsFraction {
    implicit val V = new CommonsVec[Rational, Fraction] with CommonsFraction
  }
  implicit val FractionV = FractionMV.V
//  implicit val CommonsFieldMatrixFractionMat: CommonsFieldMatrixFractionMat =
//    new CommonsFieldMatrixFractionMat
//  implicit val CommonsFieldMatrixBigFractionMat: CommonsFieldMatrixBigFractionMat =
//    new CommonsFieldMatrixBigFractionMat
}

/*

final class CommonsFieldMatrixFractionMat extends CommonsFieldMatrixMat[Rational, Fraction] with CommonsFraction

final class CommonsFieldMatrixBigFractionMat extends CommonsFieldMatrixMat[Rational, BigFraction] with CommonsBigFraction
 */
