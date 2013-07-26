package com.faacets
package alg
package mutable

import spire.math.Rational

abstract class QMatrix extends alg.QMatrix with alg.QMatrixLike[alg.mutable.QMatrix, alg.mutable.QVector] with Cloneable {
  // vvv Abstract methods

  /** Updates the i-th element in zero-based column-major order with the given value. */
  def update(i: Int, v: Rational): Unit

  /** Returns a clone of this matrix. Performs a deep copy. */
  override def clone: alg.mutable.QMatrix = copy

  def copy: alg.mutable.QMatrix

  def factory: MatrixFactory[mutable.QMatrix] = mutable.QMatrix

  def vectorFactory: VectorFactory[mutable.QVector] = mutable.QVector

  // ^^^ Abstract methods
  /////////////////////////
  // vvv Update with a scalar

  def update(r: Int, c: Int, v: Rational) { update(index(r, c), v) }

  def update(r: ::.type, c: ::.type, v: Rational) { update(0 until rows, 0 until cols, v) }

  def update(r: Int, all: ::.type, v: Rational) { update(r, 0 until cols, v) }

  def update(all: ::.type, c: Int, v: Rational) { update(0 until rows, c, v) }

  def update(rs: Seq[Int], all: ::.type, v: Rational) { update(rs, 0 until cols, v) }

  def update(all: ::.type, cs: Seq[Int], v: Rational) { update(0 until rows, cs, v) }

  def update(rs: Seq[Int], c: Int, v: Rational) { for (r <- rs) update(r, c, v) }

  def update(r: Int, cs: Seq[Int], v: Rational) { for (c <- cs) update(r, c, v) }

  def update(rs: Seq[Int], cs: Seq[Int], v: Rational) { for (r <- rs; c <- cs) update(r, c, v) }

  // ^^^ Update with a scalar
  ///////////////////////////
  // vvv Update with a matrix

  def update(r: ::.type, c: ::.type, m: alg.QMatrix) { update(0 until rows, 0 until cols, m) }

  def update(r: Int, all: ::.type, m: alg.QMatrix) { update(r, 0 until cols, m) }

  def update(all: ::.type, c: Int, m: alg.QMatrix) { update(0 until rows, c, m) }

  def update(rs: Seq[Int], all: ::.type, m: alg.QMatrix) { update(rs, 0 until cols, m) }

  def update(all: ::.type, cs: Seq[Int], m: alg.QMatrix) { update(0 until rows, cs, m) }

  def update(rs: Seq[Int], c: Int, m: alg.QMatrix) {
    require(m.rows == rs.length && m.cols == 1)
    for ((r, i) <- rs.view.zipWithIndex)
      update(r, c, m(i, 0))
  }

  def update(r: Int, cs: Seq[Int], m: alg.QMatrix) {
    require(m.rows == 1 && m.cols == cs.length)
    for ((c, i) <- cs.view.zipWithIndex)
      update(r, c, m(0, i))
  }

  def update(rs: Seq[Int], cs: Seq[Int], m: alg.QMatrix) {
    require(m.rows == rs.length && m.cols == cs.length)
    for ((r, i) <- rs.view.zipWithIndex; (c, j) <- cs.view.zipWithIndex)
      update(r, c, m(i, j))
  }

  // ^^^ Update with a matrix
  ///////////////////////////
  // vvv Update with a vector

  def update(r: Int, all: ::.type, v: alg.QVector) { update(r, 0 until cols, v) }

  def update(all: ::.type, c: Int, v: alg.QVector) { update(0 until rows, c, v) }

  def update(rs: Seq[Int], c: Int, v: alg.QVector) {
    require(v.length == rs.length)
    for ((r, i) <- rs.view.zipWithIndex)
      update(r, c, v(i))
  }
  def update(r: Int, cs: Seq[Int], v: alg.QVector) {
    require(v.length == cs.length)
    for ((c, i) <- cs.view.zipWithIndex)
      update(r, c, v(i))
  }
  // ^^^ Update with a vector
}


class RationalMatrix(val rows: Int, val cols: Int, val data: Array[Rational]) extends alg.mutable.QMatrix {
  require(rows * cols == data.length)
  def copy: RationalMatrix = new RationalMatrix(rows, cols, data.clone)
  def apply(i: Int) = data(i)
  def update(i: Int, v: Rational) { data(i) = v }
  def toQVector: alg.mutable.QVector = {
    require(rows == 1 || cols == 1)
    alg.mutable.QVector(data)
  }
  def elements = data.toIndexedSeq
  def toMutable = alg.mutable.QMatrix(this)
  def toImmutable = alg.immutable.QMatrix(this)
}

object QMatrix extends MatrixFactory[alg.mutable.QMatrix] {
  def build(rows: Int, cols: Int, data: Array[Rational]) = new RationalMatrix(rows, cols, data.clone)
  def build(mat: alg.QMatrix) = new RationalMatrix(mat.rows, mat.cols, mat.elements.toArray)

  protected[alg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]) = new RationalMatrix(rows, cols, data)
  protected[alg] def unsafeBuild(mat: alg.QMatrix) = new RationalMatrix(mat.rows, mat.cols, mat.elements.toArray)
}
