package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait MatInRingAlg[MA, @sp(Double, Long) A] extends Any with MatInRing[MA, A] { self =>
  /** Computes the determinant of the given matrix. */
  def det(m: MA): A
  /** Computes the trace of the given matrix. */
  def trace(m: MA): A
}

trait MatInFieldAlg[MA, @sp(Double, Long) A] extends Any with MatInRingAlg[MA, A] with MatInField[MA, A] { self =>
  /** Computes the rank of the given matrix. */
  def rank(m: MA): Int
  /** Computes the reduced row echelon form of the given matrix. */
  def rref(m: MA): MA
  /** Computes the inverse of the given square matrix, or throws IllegalArgumentException
    * if the matrix is singular.
    */ 
  def inv(m: MA): MA
  /** Computes the Moore-Penrose pseudo-inverse of the matrix. */
  def pinv(m: MA): MA /*= {
    val mt = t(m)
    if (nRows(m) >= nCols(m))
      times(inv(times(mt, m)), mt)
    else
      times(mt, inv(times(m, mt)))
  }*/
}
