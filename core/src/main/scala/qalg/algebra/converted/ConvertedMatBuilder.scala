package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait ConvertedMatBuilder[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMat[M, A, J]
    with MatBuilder[M, A] {
  def source: MatBuilder[M, J]

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M = source.tabulate(nRows, nCols)( (r, c) => aToJ(f(r, c)) )

  def from[M1](m1: M1)(implicit M1: Mat[M1, A]): M = tabulate(M1.nRows(m1), M1.nCols(m1))( (r, c) => M1.apply(m1, r, c) )

  override def apply(m: M, rows: At1, cols: At1): M = source(m, rows, cols)

  override def t(m: M): M = source.t(m)
}
