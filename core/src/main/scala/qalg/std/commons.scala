package com.faacets.qalg
package std

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._

import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.fraction._
import org.apache.commons.math3.linear._
import algebra._
import algebra.converted._

final class CommonsRealVec
    extends VecInField[RealVector, Double]
    with VecMutable[RealVector, Double] {
  type V = RealVector
  type A = Double
  def classTagA = classTag[A]
  def eqA = Eq[A]
  def scalar = Field[A]
  def V: Vec[V, A] = this
  def length(v: V): Int = v.getDimension
  def apply(v: V, k: Int): A = v.getEntry(k)
  def tabulate(n: Int)(f: Int => A): V = MatrixUtils.createRealVector(Array.tabulate[A](n)(f))
  def update(v: V, k: Int, a: A): Unit = v.setEntry(k, a)
  override def plus(x: V, y: V): V = x.add(y)
  override def minus(x: V, y: V): V = x.subtract(y)
  override def negate(x: V): V = x.mapMultiply(-1.0)
  override def timesl(a: A, v: V): V = v.mapMultiply(a)
  def copy(v: V): V = v.copy
}

final class CommonsRealMatVec(val V: VecInField[RealVector, Double] with VecMutable[RealVector, Double])
    extends MatVecInField[RealMatrix, RealVector, Double]
    with MatVecMutable[RealMatrix, RealVector, Double] { self =>
  type M = RealMatrix
  type V = RealVector
  type A = Double
  def classTagA = classTag[A]
  def eqA = Eq[A]
  def scalar = Field[A]
  def M: MatInField[M, A] with MatMutable[M, A] = self
  def nRows(m: M): Int = m.getRowDimension
  def nCols(m: M): Int = m.getColumnDimension
  def apply(m: M, r: Int, c: Int): A = m.getEntry(r, c)
  def update(m: M, r: Int, c: Int, a: A): Unit = m.setEntry(r, c, a)
  def fromFunM(m: FunM[A]): M = MatrixUtils.createRealMatrix(Array.tabulate[A](m.nR, m.nC) { (r, c) => m.f(r, c) })
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M = MatrixUtils.createRealMatrix(Array.tabulate[A](nRows, nCols)(f))
  override def plus(x: M, y: M): M = x.add(y)
  override def minus(x: M, y: M): M = x.subtract(y)
  override def negate(x: M): M = x.scalarMultiply(scalar.fromInt(-1))
  override def times(x: M, y: M): M = x.multiply(y)
  override def timesl(a: A, m: M): M = m.scalarMultiply(a)
  override def timesl2(v: V, m: M): V = m.preMultiply(v)
  override def timesr2(m: M, v: V): V = m.operate(v)
  override def t(m: M): M = m.transpose
  def copy(m: M): M = m.copy
}

final class CommonsFractionField extends RationalField[Fraction] {
  // Order
  override def eqv(x: Fraction, y: Fraction): Boolean = (x == y)
  def compare(x: Fraction, y: Fraction): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: Fraction = Fraction.ZERO
  def plus(x: Fraction, y: Fraction): Fraction = x.add(y)
  override def minus(x: Fraction, y: Fraction): Fraction = x.subtract(y)
  def negate(x: Fraction): Fraction = x.negate

  // MultiplicativeGroup
  def one: Fraction = Fraction.ONE
  def times(x: Fraction, y: Fraction): Fraction = x.multiply(y)
  override def reciprocal(x: Fraction): Fraction = x.reciprocal
  def div(x: Fraction, y: Fraction): Fraction = x.divide(y)

  // Ring
  override def fromInt(n: Int): Fraction = new Fraction(n)

  def numerator(x: Fraction): BigInt = x.getNumerator
  def denominator(x: Fraction): BigInt = x.getDenominator
  def ratio(numerator: BigInt, denominator: BigInt): Fraction = {
    require(numerator.isValidInt && denominator.isValidInt)
    new Fraction(numerator.toInt, denominator.toInt)
  }
}

final class CommonsBigFractionField extends RationalField[BigFraction] {
  // Order
  override def eqv(x: BigFraction, y: BigFraction): Boolean = (x == y)
  def compare(x: BigFraction, y: BigFraction): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: BigFraction = BigFraction.ZERO
  def plus(x: BigFraction, y: BigFraction): BigFraction = x.add(y)
  override def minus(x: BigFraction, y: BigFraction): BigFraction = x.subtract(y)
  def negate(x: BigFraction): BigFraction = x.negate

  // MultiplicativeGroup
  def one: BigFraction = BigFraction.ONE
  def times(x: BigFraction, y: BigFraction): BigFraction = x.multiply(y)
  override def reciprocal(x: BigFraction): BigFraction = x.reciprocal
  def div(x: BigFraction, y: BigFraction): BigFraction = x.divide(y)

  // Ring
  override def fromInt(n: Int): BigFraction = new BigFraction(n)

  def numerator(x: BigFraction): BigInt = x.getNumerator
  def denominator(x: BigFraction): BigInt = x.getDenominator
  def ratio(numerator: BigInt, denominator: BigInt): BigFraction =
    new BigFraction(numerator.bigInteger, denominator.bigInteger)
}

trait CommonsVec[A <: FieldElement[A]] extends Any
    with VecInField[FieldVector[A], A]
    with VecMutable[FieldVector[A], A] { self =>
  implicit def classTagA: ClassTag[A]
  type V = FieldVector[A]
  def V: Vec[V, A] = self
  def length(v: V): Int = v.getDimension
  def apply(v: V, k: Int): A = v.getEntry(k)
  def tabulate(n: Int)(f: Int => A): V = MatrixUtils.createFieldVector(Array.tabulate[A](n)(f))
  def update(v: V, k: Int, a: A): Unit = v.setEntry(k, a)
  override def plus(x: V, y: V): V = x.add(y)
  override def minus(x: V, y: V): V = x.subtract(y)
  override def negate(x: V): V = x.mapMultiply(scalar.fromInt(-1))
  override def timesl(a: A, v: V): V = v.mapMultiply(a)
  def copy(v: V): V = v.copy
}

trait CommonsMatVec[A <: FieldElement[A]] extends Any
    with MatVecInField[FieldMatrix[A], FieldVector[A], A]
    with MatVecMutable[FieldMatrix[A], FieldVector[A], A] { self =>
  implicit def classTagA: ClassTag[A]
  type M = FieldMatrix[A]
  type V = FieldVector[A]
  def M: MatInField[M, A] with MatMutable[M, A] = self
  def V: VecInField[V, A] with VecMutable[V, A]
  def nRows(m: M): Int = m.getRowDimension
  def nCols(m: M): Int = m.getColumnDimension
  def apply(m: M, r: Int, c: Int): A = m.getEntry(r, c)
  def update(m: M, r: Int, c: Int, a: A): Unit = m.setEntry(r, c, a)
  def fromFunM(m: FunM[A]): M = MatrixUtils.createFieldMatrix(Array.tabulate[A](m.nR, m.nC) { (r, c) => m.f(r, c) })
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M = MatrixUtils.createFieldMatrix(Array.tabulate[A](nRows, nCols)(f))

  override def plus(x: M, y: M): M = x.add(y)
  override def minus(x: M, y: M): M = x.subtract(y)
  override def negate(x: M): M = x.scalarMultiply(scalar.fromInt(-1))
  override def times(x: M, y: M): M = x.multiply(y)
  override def timesl(a: A, m: M): M = m.scalarMultiply(a)
  override def timesl2(v: V, m: M): V = m.preMultiply(v)
  override def timesr2(m: M, v: V): V = m.operate(v)
  override def t(m: M): M = m.transpose
  def copy(m: M): M = m.copy
}

trait CommonsInstances {
  implicit val CommonsRealVec = new CommonsRealVec
  implicit val CommonsRealMatVec = new CommonsRealMatVec(CommonsRealVec)
  implicit val CommonsFractionField = new CommonsFractionField
  implicit val CommonsBigFractionField = new CommonsBigFractionField

  object native {
    implicit val CommonsFractionVec = new CommonsVec[Fraction] {
      def classTagA = classTag[Fraction]
      def eqA = CommonsFractionField
      def scalar = CommonsFractionField
    }

    implicit val CommonsBigFractionVec = new CommonsVec[BigFraction] {
      def classTagA = classTag[BigFraction]
      def eqA = CommonsBigFractionField
      def scalar = CommonsBigFractionField
    }

    implicit val CommonsFractionMatVec = new CommonsMatVec[Fraction] {
      def classTagA = classTag[Fraction]
      def eqA = CommonsFractionField
      def scalar = CommonsFractionField
      def V = CommonsFractionVec
    }

    implicit val CommonsBigFractionMatVec = new CommonsMatVec[BigFraction] {
      def classTagA = classTag[BigFraction]
      def eqA = CommonsBigFractionField
      def scalar = CommonsBigFractionField
      def V = CommonsBigFractionVec
    }
  }

  object converted {
    implicit val CommonsBigFractionVec: VecInField[FieldVector[BigFraction], Rational] with VecMutable[FieldVector[BigFraction], Rational] =
      new RationalConverted[BigFraction]
          with ConvertedVecInField[FieldVector[BigFraction], Rational, BigFraction]
          with ConvertedVecMutable[FieldVector[BigFraction], Rational, BigFraction] { self =>
        def rationalFieldJ = CommonsBigFractionField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def V = self
        def source: CommonsVec[BigFraction] = native.CommonsBigFractionVec
      }
    implicit val CommonsFractionVec: VecInField[FieldVector[Fraction], Rational] with VecMutable[FieldVector[Fraction], Rational] =
      new RationalConverted[Fraction]
          with ConvertedVecInField[FieldVector[Fraction], Rational, Fraction]
          with ConvertedVecMutable[FieldVector[Fraction], Rational, Fraction] { self =>
        def rationalFieldJ = CommonsFractionField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def V = self
        def source: CommonsVec[Fraction] = native.CommonsFractionVec
      }
    implicit val CommonsBigFractionMatVec: MatVecInField[FieldMatrix[BigFraction], FieldVector[BigFraction], Rational] with MatVecMutable[FieldMatrix[BigFraction], FieldVector[BigFraction], Rational] =
      new RationalConverted[BigFraction]
          with ConvertedMatVecInField[FieldMatrix[BigFraction], FieldVector[BigFraction], Rational, BigFraction]
          with ConvertedMatVecMutable[FieldMatrix[BigFraction], FieldVector[BigFraction], Rational, BigFraction] { self =>
        def rationalFieldJ = CommonsBigFractionField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def M = self
        def V = CommonsBigFractionVec
        def source: CommonsMatVec[BigFraction] = native.CommonsBigFractionMatVec
      }
    implicit val CommonsFractionMatVec: MatVecInField[FieldMatrix[Fraction], FieldVector[Fraction], Rational] with MatVecMutable[FieldMatrix[Fraction], FieldVector[Fraction], Rational] =
      new RationalConverted[Fraction]
          with ConvertedMatVecInField[FieldMatrix[Fraction], FieldVector[Fraction], Rational, Fraction]
          with ConvertedMatVecMutable[FieldMatrix[Fraction], FieldVector[Fraction], Rational, Fraction] { self =>
        def rationalFieldJ = CommonsFractionField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def M = self
        def V = CommonsFractionVec
        def source: CommonsMatVec[Fraction] = native.CommonsFractionMatVec
      }
  }
}
