package com.faacets
package qalg
package immutable

import spire.math.Rational

abstract class QVector extends QVectorBase[QVector, QMatrix] with QTensor[QVector] {
  def factory: QVectorFactory[QVector] = QVector
  def toMutable = mutable.QVector(this)
  def toImmutable = this
}

class RationalVector(val data: Array[Rational]) extends QVector {
  def length = data.length
  def apply(k: Int) = data(k)
  def toArray = data.clone
  protected[qalg] def unsafeToArray = data
}

object QVector extends QVectorFactory[QVector] {
  protected[qalg] def unsafeBuild(data: Array[Rational]) = new RationalVector(data)
}
