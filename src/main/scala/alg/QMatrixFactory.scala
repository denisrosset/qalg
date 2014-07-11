package com.faacets
package alg

import spire.math.Rational
import spire.syntax.cfor._

abstract class QMatrixFactory[M <: QMatrixBase[M, _]] {
  /** Constructs a matrix from rows/cols and a column-major data array. */
  def build(rows: Int, cols: Int, data: Array[Rational]): M = unsafeBuild(rows, cols, data.clone)

  /** Constructs a copy of a matrix. */
  def build(mat: GenQMatrix): M = unsafeBuild(mat.rows, mat.cols, mat.toArray)

  protected[alg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]): M

  protected[alg] def unsafeBuild(mat: GenQMatrix): M = unsafeBuild(mat.rows, mat.cols, mat.unsafeToArray)

  def fromColVectors(vectors: GenQVector*): M = tabulate(vectors.head.length, vectors.length)( (r, c) => vectors(c)(r) )

  def fromRowVectors(vectors: GenQVector*): M = tabulate(vectors.length, vectors.head.length)( (r, c) => vectors(r)(c) )

  def colMajor[R <% Rational](size: (Int, Int), data: R*): M = build(size._1, size._2, data.map(r => (r: Rational)).toArray)

  def rowMajor[R <% Rational](size: (Int, Int), transposedData: R*): M = tabulate(size._1, size._2)( (r, c) => ((transposedData(c + r * size._2)): Rational) )

  def apply[R <% Rational](size: (Int, Int), data: R*): M = colMajor(size, data:_*)

  def apply(rows: Int, cols: Int, data: Array[Int]): M = build(rows, cols, data.map(RationalCache(_,1)))

  def apply(rows: Int, cols: Int, data: Array[Rational]): M = build(rows, cols, data)

  def apply(rows: Int, cols: Int, vector: GenQVector): M =
    tabulate(rows, cols)( (r, c) => vector(r + c * rows) )

  def apply(mat: GenQMatrix): M = build(mat)

  def fill(rows: Int, cols: Int)(v: Rational): M = 
    unsafeBuild(rows, cols, Array.fill(rows*cols)(v))

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => Rational): M = {
    val data = new Array[Rational](rows * cols)
    var index = 0
    cfor(0)(_ < cols, _ + 1) { c =>
      cfor(0)(_ < rows, _ + 1) { r =>
        data(index) = f(r, c)
        index += 1
      }
    }
    unsafeBuild(rows, cols, data)
  }

  def eye(d: Int): M = tabulate(d, d)( (r, c) => if (r == c) Rational.one else Rational.zero )
  def zeros(rows: Int, cols: Int): M = fill(rows, cols)(Rational.zero)
  def ones(rows: Int, cols: Int): M = fill(rows, cols)(Rational.one)
}
