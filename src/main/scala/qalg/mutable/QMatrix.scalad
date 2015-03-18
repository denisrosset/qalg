package com.faacets
package qalg
package mutable

import spire.math.Rational

abstract class QMatrix extends QMatrixBase[QMatrix, QVector] with QTensor[QMatrix, GenQMatrix] {
  def factory: QMatrixFactory[mutable.QMatrix] = mutable.QMatrix

  def toMutable = this
  def toImmutable = immutable.QMatrix(this)

  def update(r: Int, c: Int, v: Rational) { update(index(r, c), v) }
  def update(r: ::.type, c: ::.type, v: Rational) { update(0 until rows, 0 until cols, v) }
  def update(r: Int, all: ::.type, v: Rational) { update(r, 0 until cols, v) }
  def update(all: ::.type, c: Int, v: Rational) { update(0 until rows, c, v) }
  def update(rs: Seq[Int], all: ::.type, v: Rational) { update(rs, 0 until cols, v) }
  def update(all: ::.type, cs: Seq[Int], v: Rational) { update(0 until rows, cs, v) }
  def update(rs: Seq[Int], c: Int, v: Rational) { for (r <- rs) update(r, c, v) }
  def update(r: Int, cs: Seq[Int], v: Rational) { for (c <- cs) update(r, c, v) }
  def update(rs: Seq[Int], cs: Seq[Int], v: Rational) { for (r <- rs; c <- cs) update(r, c, v) }


  def update(r: ::.type, c: ::.type, m: GenQMatrix) { update(0 until rows, 0 until cols, m) }

  def update(r: Int, all: ::.type, m: GenQMatrix) { update(r, 0 until cols, m) }

  def update(all: ::.type, c: Int, m: GenQMatrix) { update(0 until rows, c, m) }

  def update(rs: Seq[Int], all: ::.type, m: GenQMatrix) { update(rs, 0 until cols, m) }

  def update(all: ::.type, cs: Seq[Int], m: GenQMatrix) { update(0 until rows, cs, m) }

  def update(rs: Seq[Int], c: Int, m: GenQMatrix) {
    require(m.rows == rs.length && m.cols == 1)
    for ((r, i) <- rs.view.zipWithIndex)
      update(r, c, m(i, 0))
  }

  def update(r: Int, cs: Seq[Int], m: GenQMatrix) {
    require(m.rows == 1 && m.cols == cs.length)
    for ((c, i) <- cs.view.zipWithIndex)
      update(r, c, m(0, i))
  }

  def update(rs: Seq[Int], cs: Seq[Int], m: GenQMatrix) {
    require(m.rows == rs.length && m.cols == cs.length)
    for ((r, i) <- rs.view.zipWithIndex; (c, j) <- cs.view.zipWithIndex)
      update(r, c, m(i, j))
  }

  def update(r: Int, all: ::.type, v: GenQVector) { update(r, 0 until cols, v) }

  def update(all: ::.type, c: Int, v: GenQVector) { update(0 until rows, c, v) }

  def update(rs: Seq[Int], c: Int, v: GenQVector) {
    require(v.length == rs.length)
    for ((r, i) <- rs.view.zipWithIndex)
      update(r, c, v(i))
  }

  def update(r: Int, cs: Seq[Int], v: GenQVector) {
    require(v.length == cs.length)
    for ((c, i) <- cs.view.zipWithIndex)
      update(r, c, v(i))
  }
}


class RationalMatrix(val rows: Int, val cols: Int, val data: Array[Rational]) extends mutable.QMatrix {
  def toArray = data.clone
  protected[qalg] def unsafeToArray = data
  def copy: RationalMatrix = new RationalMatrix(rows, cols, data.clone)
  override def apply(i: Int) = data(i)
  def update(i: Int, v: Rational) { data(i) = v }
}

object QMatrix extends QMatrixFactory[QMatrix] {
  protected[qalg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]) = new RationalMatrix(rows, cols, data)
}
