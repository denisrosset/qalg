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
  def fromCols(cols: V*): M = {
    require(cols.nonEmpty)
    val nRows = V.length(cols.head)
    require(cols.forall(V.length(_) == nRows))
    tabulate(nRows, cols.size)( (r, c) => V.apply(cols(c), r) )
  }
  def fromRows(rows: V*): M = {
    require(rows.nonEmpty)
    val nCols = V.length(rows.head)
    require(rows.forall(V.length(_) == nCols))
    tabulate(rows.size, nCols)( (r, c) => V.apply(rows(r), c) )
  }
}

object MatVecBuilder {
  def apply[M, V, @sp(Double, Long) A](implicit MV: MatVecBuilder[M, V, A]): MatVecBuilder[M, V, A] = MV
}
