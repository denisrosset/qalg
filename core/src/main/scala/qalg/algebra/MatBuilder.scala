package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatBuilder[M, @sp(Double, Long) A] extends Any with Mat[M, A] with LinBuilder[M, A] { self =>
  implicit def A: AdditiveMonoid[A]

  def apply(m: M, rows: At1, cols: At1): M = tabulate(rows.length, cols.length)( (r, c) => apply(m, rows(r), cols(c)) )
  def apply(m: M, rows: ::.type, cols: ::.type): M = m
  def apply(m: M, rows: ::.type, cols: At1): M = tabulate(nRows(m), cols.length)( (r, c) => apply(m, r, cols(c)) )
  def apply(m: M, rows: At1, cols: ::.type): M = tabulate(rows.length, nCols(m)) ( (r, c) => apply(m, rows(r), c) )

  def t(m: M): M = tabulate(nCols(m), nRows(m))( (r, c) => apply(m, c, r) )

  def build(nRows: Int, nCols: Int, elements: A*): M = tabulate(nRows, nCols)( (r, c) => elements(r * nCols + c) )

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M

  def map(m: M)(f: A => A) =
    tabulate(nRows(m), nCols(m))( (r, c) => f(apply(m, r, c)) )

  def fill(nRows: Int, nCols: Int)(a: A): M = tabulate(nRows, nCols)( (r, c) => a )

  def fromCols[V](dim: Int, cols: V*)(implicit V: Vec[V, A]): M =
    if (cols.isEmpty) tabulate(dim, 0)( (r, c) => sys.error("Empty matrix")) else {
      val nRows = V.length(cols.head)
      require(cols.forall(V.length(_) == nRows))
      tabulate(nRows, cols.size)( (r, c) => V.apply(cols(c), r) )
    }

  def fromRows[V](dim: Int, rows: V*)(implicit V: Vec[V, A]): M =
    if (rows.isEmpty) tabulate(0, dim)( (r, c) => sys.error("Empty matrix")) else {
      require(rows.nonEmpty)
      val nCols = V.length(rows.head)
      require(rows.forall(V.length(_) == nCols))
      tabulate(rows.size, nCols)( (r, c) => V.apply(rows(r), c) )
    }
}

object MatBuilder {
  def apply[M, @sp(Double, Long) A](implicit M: MatBuilder[M, A]): MatBuilder[M, A] = M
}
