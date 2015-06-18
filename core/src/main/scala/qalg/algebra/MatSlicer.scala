package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatSlicer[M, V] extends Any { self =>
  def apply(m: M, rows: At1, c: Int): V
  def apply(m: M, r: Int, cols: At1): V
}

object MatSlicer {
  def apply[M, V](implicit S: MatSlicer[M, V]): MatSlicer[M, V] = S
  implicit def fromPack[M, V](implicit ev: PackMVR[M, V, _]): MatSlicer[M, V] = ev.MVSlicer
}

trait MatSlicerImpl[M, V, @sp(Double, Long) A] extends Any with MatSlicer[M, V] { self =>
  implicit def V: VecBuilder[V, A]
  implicit def M: Mat[M, A]
  def apply(m: M, genRows: At1, c: Int): V = {
    val rows = genRows.forRowsOf(m)(M)
    V.tabulate(rows.length)( k => M(m, rows(k), c) )
  }
  def apply(m: M, r: Int, genCols: At1): V = {
    val cols = genCols.forColsOf(m)(M)
    V.tabulate(cols.length)( k => M(m, r, cols(k)) )
  }
}
