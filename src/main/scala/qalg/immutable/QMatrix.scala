package com.faacets
package qalg
package immutable

import spire.math.Rational

abstract class QMatrix extends QMatrixBase[immutable.QMatrix, immutable.QVector] with QTensor[QMatrix] {
  def factory: QMatrixFactory[QMatrix] = QMatrix
  def toMutable = mutable.QMatrix(this)
  def toImmutable = this
}

class RationalMatrix(val rows: Int, val cols: Int, val data: Array[Rational]) extends QMatrix {
  require(rows * cols == data.length)
  def toArray = data.clone
  protected[qalg] def unsafeToArray = data
  def apply(i: Int) = data(i)
}

object QMatrix extends QMatrixFactory[QMatrix] {
  protected[qalg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]) = 
    new RationalMatrix(rows, cols, data)
}
