package com.faacets
package alg

import spire.math.Rational
import spire.syntax.cfor._

abstract class QVectorFactory[V <: QVectorBase[V, _]] {
  /** Constructs a vector from a copy of a data array. */
  def build(data: Array[Rational]): V = unsafeBuild(data.clone)

  /** Constructs a copy of a vector. */
  def build(vec: GenQVector): V = unsafeBuild(vec.toArray)

  /** Constructs a vector using the given data array, which should be used only once here. */
  protected[alg] def unsafeBuild(data: Array[Rational]): V

  /** Constructs a vector stealing the data array of the given vector. */
  protected[alg] def unsafeBuild(vec: GenQVector): V = unsafeBuild(vec.unsafeToArray)

  def apply(intArray: Array[Int]): V = unsafeBuild(intArray.map(RationalCache(_, 1)))

  def apply[R <% Rational](data: R*): V =
    unsafeBuild(Array.tabulate[Rational](data.length)( i => data(i): Rational ))

  def apply(data: Array[Rational]): V = build(data)

  def apply(mat: GenQMatrix): V = ??? // TODO tabulate(mat.rows*mat.cols)( i => mat(i) )

  def apply(vec: GenQVector): V = build(vec)

  def fill(length: Int)(v: Rational): V =
    unsafeBuild(Array.fill(length)(v))

  def tabulate(length: Int)(f: Int => Rational): V = {
    val data = new Array[Rational](length)
    cfor(0)(_ < length, _ + 1) { index =>
      data(index) = f(index)
    }
    unsafeBuild(data)
  }

  def zeros(length: Int): V = fill(length)(Rational.zero)
  def ones(length: Int): V = fill(length)(Rational.one)
}
