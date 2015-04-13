package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatBuilder[M, @sp(Double, Long) A] extends Any with Mat[M, A] { self =>
  implicit def scalar: AdditiveMonoid[A]

  def apply(m: M, rows: At1, cols: At1): M = tabulate(rows.length, cols.length)( (r, c) => apply(m, rows(r), cols(c)) )
  def apply(m: M, rows: ::.type, cols: ::.type): M = m
  def apply(m: M, rows: ::.type, cols: At1): M = tabulate(nRows(m), cols.length)( (r, c) => apply(m, r, cols(c)) )
  def apply(m: M, rows: At1, cols: ::.type): M = tabulate(rows.length, nCols(m)) ( (r, c) => apply(m, rows(r), c) )

  def t(m: M): M = tabulate(nCols(m), nRows(m))( (r, c) => apply(m, c, r) )

  def build(nRows: Int, nCols: Int, elements: A*): M = tabulate(nRows, nCols)( (r, c) => elements(r * nCols + c) )

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): M
  def fill(nRows: Int, nCols: Int)(a: A): M = tabulate(nRows, nCols)( (r, c) => a )
}

object MatBuilder {
  def apply[M, @sp(Double, Long) A](implicit M: MatBuilder[M, A]): MatBuilder[M, A] = M
}
