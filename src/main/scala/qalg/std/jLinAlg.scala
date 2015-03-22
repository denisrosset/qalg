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

final class JLinAlgDoubleWrapperField extends Field[DoubleWrapper] with Order[DoubleWrapper] {
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

  // EuclideanRing
  
  def gcd(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper =
    DoubleWrapper.FACTORY.get(Field[Double].gcd(x.getValue, y.getValue))
  def mod(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper =
    DoubleWrapper.FACTORY.get(Field[Double].mod(x.getValue, y.getValue))
  def quot(x: DoubleWrapper, y: DoubleWrapper): DoubleWrapper =
    DoubleWrapper.FACTORY.get(Field[Double].mod(x.getValue, y.getValue))
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
  def fromFunV(v: FunV[A]): V = new V(Array.tabulate[A](v.len)(k => v.f(k)))
  override def plus(x: V, y: V): V = x.add(y)
  override def minus(x: V, y: V): V = x.subtract(y)
  override def negate(v: V): V = v.multiply(scalar.fromInt(-1))
  override def timesl(a: A, v: V): V = v.multiply(a)
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
  override def plus(x: M, y: M): M = x.add(y)
  override def minus(x: M, y: M): M = x.subtract(y)
  override def negate(m: M): M = m.multiply(scalar.fromInt(-1))
  override def timesl(a: A, m: M): M = m.multiply(a)
  override def times(x: M, y: M): M = x.multiply(y)
  override def timesl2(v: V, m: M): V = v.multiply(m)
  override def timesr2(m: M, v: V): V = m.multiply(v)
  override def t(m: M): M = m.transpose
}

trait JLinAlgInstances {
  implicit val JLinAlgRationalField = new JLinAlgRationalField
  implicit val JLinAlgRationalVec = new JLinAlgVec[JRational] {
    def classTagA = classTag[JRational]
    def eqA = JLinAlgRationalField
    def scalar = JLinAlgRationalField
  }
  implicit val JLinALgRationalMatVec = new JLinAlgMatVec[JRational] {
    def V = JLinAlgRationalVec
    def classTagA = classTag[JRational]
    def eqA = JLinAlgRationalField
    def scalar = JLinAlgRationalField
  }
  implicit val JLinAlgDoubleWrapperField = new JLinAlgDoubleWrapperField
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
