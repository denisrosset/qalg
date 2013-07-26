package com.faacets
package alg
package immutable

import spire.math.Rational

abstract class QMatrix extends alg.QMatrix with alg.QMatrixLike[alg.immutable.QMatrix, alg.immutable.QVector] with Cloneable {
  // vvv Abstract methods

  /** Returns a clone of this matrix. No need to copy an immutable object. */
  override def clone: alg.immutable.QMatrix = copy

  def copy: alg.immutable.QMatrix

  def factory: MatrixFactory[immutable.QMatrix] = immutable.QMatrix

  def vectorFactory: VectorFactory[immutable.QVector] = immutable.QVector

  // ^^^ Abstract methods
  /////////////////////////
}


class RationalMatrix(val rows: Int, val cols: Int, val data: Array[Rational]) extends alg.immutable.QMatrix {
  require(rows * cols == data.length)
  def copy: RationalMatrix = this
  def apply(i: Int) = data(i)
  def toQVector: alg.immutable.QVector = {
    require(rows == 1 || cols == 1)
    alg.immutable.QVector(data)
  }
  def elements = data.toIndexedSeq
  override def hashCode: Int = scala.util.hashing.MurmurHash3.arrayHash(data)
  def toMutable = alg.mutable.QMatrix(this)
  def toImmutable = this
}

object QMatrix extends MatrixFactory[alg.immutable.QMatrix] {
  def build(rows: Int, cols: Int, data: Array[Rational]) = new RationalMatrix(rows, cols, data.clone)
  def build(mat: alg.QMatrix) = new RationalMatrix(mat.rows, mat.cols, mat.elements.toArray)

  protected[alg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]) = new RationalMatrix(rows, cols, data)
  protected[alg] def unsafeBuild(mat: alg.QMatrix) = new RationalMatrix(mat.rows, mat.cols, mat.elements.toArray)
}
