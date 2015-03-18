package com.faacets
package qalg
package mutable

import spire.math.Rational

trait QTensor[T <: QTensor[T, G], G <: GenQTensor] extends QTensorBase[T] with Cloneable {
  self: T =>
  def vectorFactory: QVectorFactory[QVector] = QVector
  def matrixFactory: QMatrixFactory[QMatrix] = QMatrix

  override def clone: T = copy

  def copy: T

  /** Updates the i-th element (in zero-based column-major order for matrices) with the given value. */
  def update(i: Int, v: Rational): Unit

  def updateElements(f: Rational => Rational) {
    var i = 0
    while (i < length) {
      this(i) = f(this(i))
      i += 1
    }
  }

  def updateElementsWithIndex(f: (Int, Rational) => Rational) {
    var i = 0
    while (i < length) {
      this(i) = f(i, this(i))
      i += 1
    }
  }

  def +=(rhs: G) {
    require(sameShape(rhs))
    updateElementsWithIndex( (i, v) => v + rhs(i) ) 
  }
  def -=(rhs: G) { 
    require(sameShape(rhs))
    updateElementsWithIndex( (i: Int, v: Rational) => v - rhs(i) ) 
  }

  def *=(rhs: Rational) {
    updateElements(_ * rhs)
  }
  def /=(rhs: Rational) {
    updateElements(_ / rhs)
  }
  def :+=(rhs: Rational) {
    updateElements(_ + rhs)
  }
  def :-=(rhs: Rational) {
    updateElements(_ - rhs)
  }
}
