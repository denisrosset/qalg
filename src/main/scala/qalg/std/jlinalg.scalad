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

trait JLinAlgBase[@sp(Double, Long) A, J <: IRingElement[J]] extends Any {
  implicit def jClassTag: ClassTag[J]
  implicit def eqA: Eq[A]
  implicit def scalar: AdditiveMonoid[A]
  def toJ(a: A): J
  def fromJ(j: J): A
}

class NegateOperator[RE <: IRingElement[RE]] extends MonadicOperator[RE] {
  def apply(re: RE): RE = re.negate
}

trait JLinAlgVec[@sp(Double, Long) A, J <: IRingElement[J]] extends Any
    with JLinAlgBase[A, J]
    with VecInField[JVector[J], A]
    with VecMutable[JVector[J], A] {
  def apply(v: JVector[J], k: Int): A = fromJ(v.getEntry(k + 1))
  def update(v: JVector[J], k: Int, a: A): Unit = { v.set(k + 1, toJ(a)) }
  def length(v: JVector[J]): Int = v.length
  def from(v: FunV[A]): JVector[J] = new JVector[J](Array.tabulate[J](v.len)(k => toJ(v.f(k))))
  override def plus(x: JVector[J], y: JVector[J]): JVector[J] = x.add(y)
  override def minus(x: JVector[J], y: JVector[J]): JVector[J] = x.subtract(y)
  override def negate(v: JVector[J]): JVector[J] = v.apply(new NegateOperator[J])
  override def timesl(a: A, v: JVector[J]): JVector[J] = v.multiply(toJ(a))
}

trait JLinAlgMat[@sp(Double, Long) A, J <: IRingElement[J]] extends Any
    with JLinAlgBase[A, J]
    with MatInField[JMatrix[J], A]
    with MatMutable[JMatrix[J], A]
    with MatInFieldAlg[JMatrix[J], A] {
  def nRows(m: JMatrix[J]): Int = m.getRows
  def nCols(m: JMatrix[J]): Int = m.getCols
  def apply(m: JMatrix[J], r: Int, c: Int): A = fromJ(m.get(r + 1, c + 1))
  def update(m: JMatrix[J], r: Int, c: Int, a: A): Unit = { m.set(r + 1, c + 1, toJ(a)) }
  def from(m: FunM[A]): JMatrix[J] = new JMatrix[J](Array.tabulate[J](m.nR, m.nC)( (r, c) => toJ(m.f(r, c))))
  override def plus(x: JMatrix[J], y: JMatrix[J]): JMatrix[J] = x.add(y)
  override def minus(x: JMatrix[J], y: JMatrix[J]): JMatrix[J] = x.subtract(y)
  override def negate(m: JMatrix[J]): JMatrix[J] = m.apply(new NegateOperator[J])
  override def timesl(a: A, m: JMatrix[J]): JMatrix[J] = m.multiply(toJ(a))
  override def times(x: JMatrix[J], y: JMatrix[J]): JMatrix[J] = x.multiply(y)
  def rank(m: JMatrix[J]): Int = m.rank
  def rref(m: JMatrix[J]): JMatrix[J] = m.gaussjord
  def det(m: JMatrix[J]): A = fromJ(m.det)
  def trace(m: JMatrix[J]): A = fromJ(m.trace)
  def inv(m: JMatrix[J]): JMatrix[J] = m.inverse
  def pinv(m: JMatrix[J]): JMatrix[J] = throw new UnsupportedOperationException("pinv not implemented")
  override def t(m: JMatrix[J]): JMatrix[J] = m.transpose
}

trait JLinAlgRational extends JLinAlgBase[Rational, JRational] {
  def jClassTag: ClassTag[JRational] = implicitly[ClassTag[JRational]]
  def eqA: Eq[Rational] = implicitly[Eq[Rational]]
  def scalar: Field[Rational] = implicitly[Field[Rational]]
  def fromJ(j: JRational): Rational = Rational(j.getNumerator, j.getDenominator)
  def toJ(a: Rational): JRational = JRational.FACTORY.get(a.numerator.bigInteger, a.denominator.bigInteger, true)
}

trait JLinAlgDouble extends JLinAlgBase[Double, DoubleWrapper] {
  def jClassTag: ClassTag[DoubleWrapper] = implicitly[ClassTag[DoubleWrapper]]
  def eqA: Eq[Double] = implicitly[Eq[Double]]
  def scalar: Field[Double] = implicitly[Field[Double]]
  def fromJ(j: DoubleWrapper): Double = j.getValue
  def toJ(a: Double): DoubleWrapper = new DoubleWrapper(a)
}

final class JLinAlgRationalVec extends JLinAlgVec[Rational, JRational] with JLinAlgRational

final class JLinAlgDoubleVec extends JLinAlgVec[Double, DoubleWrapper] with JLinAlgDouble

final class JLinAlgRationalMat extends JLinAlgMat[Rational, JRational] with JLinAlgRational

final class JLinAlgDoubleMat extends JLinAlgMat[Double, DoubleWrapper] with JLinAlgDouble

trait JLinAlgInstances {
  implicit val JLinAlgRationalVec: JLinAlgVec[Rational, JRational] = new JLinAlgRationalVec
  implicit val JLinAlgDoubleVec: JLinAlgVec[Double, DoubleWrapper] = new JLinAlgDoubleVec
  implicit val JLinAlgRationalMat: JLinAlgMat[Rational, JRational] = new JLinAlgRationalMat
  implicit val JLinAlgDoubleMat: JLinAlgMat[Double, DoubleWrapper] = new JLinAlgDoubleMat
}
