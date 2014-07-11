package com.faacets
package alg
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
  protected[alg] def unsafeToArray = data
  def apply(i: Int) = data(i)
}

object QMatrix extends QMatrixFactory[QMatrix] {
  protected[alg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]) = 
    new RationalMatrix(rows, cols, data)
}
