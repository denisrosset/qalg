package com.faacets.qalg
package std

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._

import org.jlinalg.{Vector => JVector, Matrix => JMatrix, IRingElement, IRingElementFactory}
import org.jlinalg.rational.{Rational => JRational}
import org.jlinalg.doublewrapper.DoubleWrapper
import org.jlinalg.operator.MonadicOperator

import algebra._
import algebra.converted._

final class JLinAlgRationalField extends RationalField[JRational] {
  // Order
  override def eqv(x: JRational, y: JRational): Boolean = (x == y)
  def compare(x: JRational, y: JRational): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: JRational = JRational.FACTORY.zero
  def plus(x: JRational, y: JRational): JRational = x.add(y)
  override def minus(x: JRational, y: JRational): JRational = x.subtract(y)
  def negate(x: JRational): JRational = x.negate

  // MultiplicativeGroup
  def one: JRational = JRational.FACTORY.one
  def times(x: JRational, y: JRational): JRational = x.multiply(y)
  override def reciprocal(x: JRational): JRational = x.invert
  def div(x: JRational, y: JRational): JRational = x.divide(y)

  // Ring
  override def fromInt(n: Int): JRational = JRational.FACTORY.get(n)

  def numerator(x: JRational): BigInt = x.getNumerator
  def denominator(x: JRational): BigInt = x.getDenominator
  def ratio(numerator: BigInt, denominator: BigInt): JRational =
    JRational.FACTORY.get(numerator.bigInteger, denominator.bigInteger, true)
}

final class JLinAlgDoubleWrapperField extends DoubleField[DoubleWrapper] {
  override def fromDouble(a: Double) = DoubleWrapper.FACTORY.get(a)
  def toDouble(j: DoubleWrapper) = j.doubleValue

  // Order
  override def eqv(x: DoubleWrapper, y: DoubleWrapper): Boolean = (x == y)
  def compare(x: DoubleWrapper, y: DoubleWrapper): Int = x.compareTo(y)

  // AdditiveGroup
  def zero: DoubleWrapper = DoubleWrapper.FACTORY.zero
  def plus(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper = x.add(y)
  override def minus(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper = x.subtract(y)
  def negate(x: DoubleWrapper): DoubleWrapper = x.negate

  // MultiplicativeGroup
  def one: DoubleWrapper = DoubleWrapper.FACTORY.one
  def times(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper = x.multiply(y)
  override def reciprocal(x: DoubleWrapper): DoubleWrapper = x.invert
  def div(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper = x.divide(y)

  // Ring
  override def fromInt(n: Int): DoubleWrapper = DoubleWrapper.FACTORY.get(n)
}

trait JLinAlgVec[A <: IRingElement[A]] extends Any
    with VecInField[JVector[A], A]
    with VecMutable[JVector[A], A] { self =>
  implicit def classTagA: ClassTag[A]
  type V = JVector[A]
  def V: Vec[V, A] = self
  def apply(v: V, k: Int): A = v.getEntry(k + 1)
  def update(v: V, k: Int, a: A): Unit = { v.set(k + 1, a) }
  def length(v: V): Int = v.length
  def tabulate(n: Int)(f: Int => A): V = new V(Array.tabulate(n)(f))
  override def plus(x: V, y: V): V = x.add(y)
  override def minus(x: V, y: V): V = x.subtract(y)
  override def negate(v: V): V = v.multiply(scalar.fromInt(-1))
  override def timesl(a: A, v: V): V = v.multiply(a)
  def copy(v: V): V = v.copy

}

trait JLinAlgMatVec[A <: IRingElement[A]] extends Any
    with MatVecInField[JMatrix[A], JVector[A], A]
    with MatVecMutable[JMatrix[A], JVector[A], A] { self =>
  type M = JMatrix[A]
  type V = JVector[A]
  implicit def classTagA: ClassTag[A]
  def M: MatInField[M, A] with MatMutable[M, A] = self
  def V: VecInField[V, A] with VecMutable[V, A]
  def nRows(m: M): Int = m.getRows
  def nCols(m: M): Int = m.getCols
  def apply(m: M, r: Int, c: Int): A = m.get(r + 1, c + 1)
  def update(m: M, r: Int, c: Int, a: A): Unit = { m.set(r + 1, c + 1, a) }
  def fromFunM(m: FunM[A]): M = new M(Array.tabulate[A](m.nR, m.nC)( (r, c) => m.f(r, c)))
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M = new M(Array.tabulate[A](nRows, nCols)(f))
  override def plus(x: M, y: M): M = x.add(y)
  override def minus(x: M, y: M): M = x.subtract(y)
  override def negate(m: M): M = m.multiply(scalar.fromInt(-1))
  override def timesl(a: A, m: M): M = m.multiply(a)
  override def times(x: M, y: M): M = x.multiply(y)
  override def timesl2(v: V, m: M): V = v.multiply(m)
  override def timesr2(m: M, v: V): V = m.multiply(v)
  override def t(m: M): M = m.transpose
  def copy(m: M): M = m.copy
}

trait JLinAlgInstances {
  implicit val JLinAlgRationalField = new JLinAlgRationalField
  implicit val JLinAlgDoubleWrapperField = new JLinAlgDoubleWrapperField
  object native {
    implicit val JLinAlgRationalVec = new JLinAlgVec[JRational] {
      def classTagA = classTag[JRational]
      def eqA = JLinAlgRationalField
      def scalar = JLinAlgRationalField
    }
    implicit val JLinAlgRationalMatVec = new JLinAlgMatVec[JRational] {
      def V = JLinAlgRationalVec
      def classTagA = classTag[JRational]
      def eqA = JLinAlgRationalField
      def scalar = JLinAlgRationalField
    }
    implicit val JLinAlgDoubleWrapperVec = new JLinAlgVec[DoubleWrapper] {
      def classTagA = classTag[DoubleWrapper]
      def eqA = JLinAlgDoubleWrapperField
      def scalar = JLinAlgDoubleWrapperField
    }
    implicit val JLinAlgDoubleWrapperMatVec = new JLinAlgMatVec[DoubleWrapper] {
      def V = JLinAlgDoubleWrapperVec
      def classTagA = classTag[DoubleWrapper]
      def eqA = JLinAlgDoubleWrapperField
      def scalar = JLinAlgDoubleWrapperField
    }
  }
  object converted {
    implicit val JLinAlgRationalVec: VecInField[JVector[JRational], Rational] with VecMutable[JVector[JRational], Rational] =
      new RationalConverted[JRational]
          with ConvertedVecInField[JVector[JRational], Rational, JRational]
          with ConvertedVecMutable[JVector[JRational], Rational, JRational] { self =>
        def rationalFieldJ = JLinAlgRationalField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def V = self
        def source = native.JLinAlgRationalVec
      }
    implicit val JLinAlgRationalMatVec: MatVecInField[JMatrix[JRational], JVector[JRational], Rational] with MatVecMutable[JMatrix[JRational], JVector[JRational], Rational] =
      new RationalConverted[JRational]
          with ConvertedMatVecInField[JMatrix[JRational], JVector[JRational], Rational, JRational]
          with ConvertedMatVecMutable[JMatrix[JRational], JVector[JRational], Rational, JRational] { self =>
        def rationalFieldJ = JLinAlgRationalField
        def classTagA = classTag[Rational]
        def eqA = Eq[Rational]
        def scalar = Field[Rational]
        def M = self
        def V = JLinAlgRationalVec
        def source = native.JLinAlgRationalMatVec
      }
    implicit val JLinAlgDoubleVec: VecInField[JVector[DoubleWrapper], Double] with VecMutable[JVector[DoubleWrapper], Double] =
      new DoubleConverted[DoubleWrapper]
          with ConvertedVecInField[JVector[DoubleWrapper], Double, DoubleWrapper]
          with ConvertedVecMutable[JVector[DoubleWrapper], Double, DoubleWrapper] { self =>
        def doubleFieldJ = JLinAlgDoubleWrapperField
        def classTagA = classTag[Double]
        def eqA = Eq[Double]
        def scalar = Field[Double]
        def V = self
        def source = native.JLinAlgDoubleWrapperVec
      }
    implicit val JLinAlgDoubleMatVec: MatVecInField[JMatrix[DoubleWrapper], JVector[DoubleWrapper], Double] with MatVecMutable[JMatrix[DoubleWrapper], JVector[DoubleWrapper], Double] =
      new DoubleConverted[DoubleWrapper]
          with ConvertedMatVecInField[JMatrix[DoubleWrapper], JVector[DoubleWrapper], Double, DoubleWrapper]
          with ConvertedMatVecMutable[JMatrix[DoubleWrapper], JVector[DoubleWrapper], Double, DoubleWrapper] { self => 
        def doubleFieldJ = JLinAlgDoubleWrapperField
        def classTagA = classTag[Double]
        def eqA = Eq[Double]
        def scalar = Field[Double]
        def M = self
        def V = JLinAlgDoubleVec
        def source = native.JLinAlgDoubleWrapperMatVec
      }
  }
}
