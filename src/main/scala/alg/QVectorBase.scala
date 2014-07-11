package com.faacets
package alg

import spire.algebra.{Field, Order, InnerProductSpace, VectorSpace, Monoid}
import spire.math.{Rational, SafeLong, lcm, gcd}
import net.alasc._
import spire.syntax.vectorSpace._
import spire.syntax.module._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.syntax.order._
import net.alasc.actionSyntax._

/** Base trait for mutable or immutable vectors. */
abstract class QVectorBase[V <: QVectorBase[V, M], M <: QMatrixBase[M, V]] extends GenQVector with QTensorBase[V] {
  lhs: V =>
  def factory: QVectorFactory[V]
  def matrixFactory: QMatrixFactory[M]

  def toMutable: mutable.QVector
  def toImmutable: immutable.QVector

  def apply(all: ::.type): V = copy
  def apply(is: Seq[Int]): V = factory.tabulate(is.length)( i => this(i) )

  def mapElements(f: Rational => Rational): V = factory.tabulate(length)(i => f(this(i)))

  def mapWithIndex(f: (Int, Rational) => Rational): V = factory.tabulate(length)( i => f(i, this(i)))

  /** Returns a row QMatrix containing a copy of this vector elements. */
  def toRowQMatrix: M = matrixFactory.tabulate(1, length)( (r,c) => this(c) )

  /** Returns a column QMatrix containing a copy of this vector elements. */
  def toColQMatrix: M = matrixFactory.tabulate(length, 1)( (r,c) => this(r) )

  /** Transpose of that vector, i.e. vector as row matrix. */
  def t: M = toRowQMatrix
}

class QVectorPermutingAction[V <: QVectorBase[V, _], P <: Permuting[P]](implicit factory: QVectorFactory[V]) extends Action[V, P] {
  import Dom.ZeroBased._
  def actr(v: V, p: P): V = {
    val newData = new Array[Rational](v.length)
    for (i <- 0 until v.length)
      newData(p.image(i)) = v(i)
    factory.build(newData)
  }
  def actrinv(v: V, pinv: P): V = factory.tabulate(v.length)(i => v.apply(pinv.image(i)))
}

class QVectorPReprAction[V <: QVectorBase[V, _], F <: Finite[F]](
  implicit factory: QVectorFactory[V], prepr: PRepr[F])
    extends Action[V, F] {
  import Dom.ZeroBased._

  def actr(v: V, f: F): V = {
    val newData = new Array[Rational](v.length)
    for (i <- 0 until v.length)
      newData(i <|+| f) = v(i)
    factory.build(newData)
  }

  def actrinv(v: V, finv: F): V = factory.tabulate(v.length)(i => v(i <|+| finv))
}

class QVectorBaseInnerProductSpace[V <: QVectorBase[V, _]](factory: QVectorFactory[V]) extends InnerProductSpace[V, Rational] {
  implicit def scalar = Rational.RationalAlgebra
  def negate(v: V): V = factory.tabulate(v.length)(-v(_))
  def zero: V = factory.fill(0)(Rational.zero)
  def plus(x: V, y: V): V = {
    require(x.length == y.length)
    factory.tabulate(x.length)(i => x(i) + y(i))
  }
  override def minus(x: V, y: V): V = {
    require(x.length == y.length)
    factory.tabulate(x.length)(i => x(i) - y(i))
  }
  def timesl(r: Rational, v: V): V = factory.tabulate(v.length)(i => r * v(i))
  def dot(v: V, w: V): Rational = {
    val length = v.length
    require(length == w.length)
    var i = 0
    var sum = Rational.zero
    while (i < length) {
      sum += v(i) * w(i)
      i += 1
    }
    sum
  }
}

class QVectorKron[V <: QVectorBase[V, _]](implicit factory: QVectorFactory[V], vs: VectorSpace[V, Rational]) extends Monoid[V] {
  def id = factory.fill(1)(Rational.one)

  def op(a: V, b: V): V = {
    val res = alg.mutable.QVector.zeros(a.length * b.length)
    // TODO cfor
    for (i <- 0 until a.length; av = a(i)) {
      val prod: V = vs.timesl(av, b)
      res(i * b.length until (i+1) * b.length) = prod
    }
    factory.unsafeBuild(res)
  }
}
