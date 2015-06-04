package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatSlicer[M, V] extends Any { self =>
  def apply(m: M, rows: ::.type, c: Int): V
  def apply(m: M, rows: At1, c: Int): V
  def apply(m: M, r: Int, cols: ::.type): V
  def apply(m: M, r: Int, cols: At1): V
}

trait MatSlicerImpl[M, V, @sp(Double, Long) A] extends Any with MatSlicer[M, V] { self =>
  implicit def V: VecBuilder[V, A]
  implicit def M: Mat[M, A]
  def apply(m: M, rows: ::.type, c: Int): V = V.tabulate(M.nRows(m))( M(m, _, c) )
  def apply(m: M, rows: At1, c: Int): V = V.tabulate(rows.length)( k => M(m, rows(k), c) )
  def apply(m: M, r: Int, cols: ::.type): V = V.tabulate(M.nCols(m))( M(m, r, _) )
  def apply(m: M, r: Int, cols: At1): V = V.tabulate(cols.length)( k => M(m, r, cols(k)) )
}

object MatSlicer {
  def apply[M, V](implicit S: MatSlicer[M, V]): MatSlicer[M, V] = S
}
