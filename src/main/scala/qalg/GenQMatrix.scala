package com.faacets
package qalg

import spire.algebra.Eq
import spire.math.Rational
import spire.syntax.eq._

abstract class GenQMatrix extends GenQTensor {
  /** Number of rows. */
  def rows: Int
  /** Number of columns. */
  def cols: Int

  def length = rows * cols

  def sameDimensions(rhs: GenQMatrix) = rows == rhs.rows && cols == rhs.cols

  /** Retrieves the element (r, c). */
  def apply(r: Int, c: Int): Rational = apply(index(r, c))

  def index(r: Int, c: Int): Int = r + c * rows

  override def toString = MatrixPrinting.print(this)
}

trait GenQMatrixInstances {
  implicit object GenQMatrixEq extends Eq[GenQMatrix] {
    def eqv(a: GenQMatrix, b: GenQMatrix): Boolean = {
      if (a.rows == b.rows && a.cols == b.cols) {
        var i = 0
        val length = a.rows * a.cols
        while (i < length) {
          if (a(i) =!= b(i))
            return false
          i += 1
        }
        true
      } else false
    }
  }
}
