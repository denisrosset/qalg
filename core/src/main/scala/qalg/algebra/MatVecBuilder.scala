package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecBuilder[M, V, @sp(Double, Long) A] extends Any with MatVec[M, V, A] with MatBuilder[M, A] { self =>
  implicit def V: VecBuilder[V, A]
  def apply(m: M, rows: ::.type, c: Int): V = V.tabulate(nRows(m))( apply(m, _, c) )
  def apply(m: M, rows: At1, c: Int): V = V.tabulate(rows.length)( k => apply(m, rows(k), c) )
  def apply(m: M, r: Int, cols: ::.type): V = V.tabulate(nCols(m))( apply(m, r, _) )
  def apply(m: M, r: Int, cols: At1): V = V.tabulate(cols.length)( k => apply(m, r, cols(k)) )
}

object MatVecBuilder {
  def apply[M, V, @sp(Double, Long) A](implicit MV: MatVecBuilder[M, V, A]): MatVecBuilder[M, V, A] = MV
}
