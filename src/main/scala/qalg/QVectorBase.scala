package com.faacets
package qalg

import spire.algebra.{Field, Order, InnerProductSpace, VectorSpace, Monoid, GroupAction}
import spire.math.{Rational, SafeLong, lcm, gcd}
import net.alasc._
import spire.implicits._

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

  def integerCoefficients: (Array[BigInt], BigInt) = {
    val array = toArray
    val commonDenominator = (BigInt(1) /: array)( (lSoFar, el) => spire.math.lcm(lSoFar, el.denominator) )
    val coeffsArray = array.map( r => r.numerator * (commonDenominator/r.denominator) )
    (coeffsArray, commonDenominator)
  }

  def longCoefficients: (Array[Long], Long) = {
    val (coeffsArray, commonDenominator) = integerCoefficients
    if (coeffsArray.exists(!_.isValidLong))
      throw new IllegalArgumentException("Coefficients do not fit into a long.")
    if (!commonDenominator.isValidLong)
      throw new IllegalArgumentException("Denominator does not fit into a long.")
    (coeffsArray.map(_.longValue), commonDenominator.longValue)
  }
}

class QVectorBasePermutingAction[V <: QVectorBase[V, _], P <: Permuting[P]](factory: QVectorFactory[V]) extends GroupAction[V, P] {
  import Dom.ZeroBased._
  implicit def scalar = net.alasc.all.FiniteSemigroup[P]

  def actr(v: V, p: P): V = {
    val newData = new Array[Rational](v.length)
    for (i <- 0 until v.length)
      newData(p.image(i)) = v(i)
    factory.build(newData)
  }
  def actl(pinv: P, v: V): V = factory.tabulate(v.length)(i => v.apply(pinv.image(i)))
}

class QVectorBasePReprAction[V <: QVectorBase[V, _], F <: Finite[F]](factory: QVectorFactory[V])(implicit prepr: PRepr[F])
    extends GroupAction[V, F] {
  import Dom.ZeroBased._
  implicit def scalar = net.alasc.all.FiniteSemigroup[F]

  def actr(v: V, f: F): V = {
    val newData = new Array[Rational](v.length)
    for (i <- 0 until v.length)
      newData(i <|+| f) = v(i)
    factory.build(newData)
  }

  def actl(finv: F, v: V): V = factory.tabulate(v.length)(i => v(i <|+| finv))
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

class QVectorBaseKron[V <: QVectorBase[V, _]](factory: QVectorFactory[V])(implicit ev: VectorSpace[V, Rational]) extends Monoid[V] {
  def id = factory.fill(1)(Rational.one)

  def op(a: V, b: V): V = {
    import qVectorInstances._
    val res = mutable.QVector.zeros(a.length * b.length)
    // TODO cfor
    for (i <- 0 until a.length; av = a(i)) {
      val prod: V = ev.timesl(av, b)
      res(i * b.length until (i+1) * b.length) = prod
    }
    factory.unsafeBuild(res)
  }
}

class QVectorBaseOrder[V <: QVectorBase[V, _]] extends Order[V] {
  def compare(u: V, v: V): Int = {
    assert(u.length == v.length)
    for(i <- 0 until u.length) {
      val compareValue = Rational.RationalAlgebra.compare(u(i), v(i))
      if (compareValue != 0)
        return compareValue
    }
    0
  }
}
