package com.faacets
package qalg
package mutable

import spire.math.Rational

abstract class QVector extends QVectorBase[QVector, QMatrix] with QTensor[QVector, GenQVector] {
  def factory: QVectorFactory[QVector] = QVector

  def toMutable = this
  def toImmutable = immutable.QVector(this)

  /** Updates the i-th element (zero-based) with the given value. */
  def update(i: Int, v: Rational): Unit

  /** Returns a clone of this vector. Performs a deep copy. */
  override def clone: QVector = copy

  def copy: QVector

  def update(all: ::.type, v: Rational) { update(0 until length, v) }
  def update(is: Seq[Int], v: Rational) { for(i <- is) update(i, v) }

  def update(all: ::.type, m: GenQMatrix) { update(0 until length, m) }
  def update(is: Seq[Int], m: GenQMatrix) {
    require(is.length == m.length && (m.rows == 1 || m.cols == 1))
    for ((dest, src) <- is.view.zipWithIndex) update(dest, m(src))
  }

  def update(all: ::.type, v: GenQVector) { update(0 until length, v) }
  def update(is: Seq[Int], v: GenQVector) {
    require(is.length == v.length)
    for ((dest, src) <- is.view.zipWithIndex) update(dest, v(src))
  }
}

class RationalVector(val data: Array[Rational]) extends QVector {
  def toArray = data.clone
  protected[qalg] def unsafeToArray = data
  def length = data.length
  def apply(k: Int) = data(k)
  def update(k: Int, v: Rational) { data(k) = v }
  def copy: RationalVector = new RationalVector(data.clone)
}

object QVector extends QVectorFactory[QVector] {
  protected[qalg] def unsafeBuild(data: Array[Rational]) = new RationalVector(data)
}
