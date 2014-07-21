package com.faacets
package alg
package immutable

import spire.math.Rational

trait QTensor[T <: QTensor[T]] extends QTensorBase[T] {
  self: T =>
  def vectorFactory: QVectorFactory[QVector] = QVector
  def matrixFactory: QMatrixFactory[QMatrix] = QMatrix
  def copy = this
}
