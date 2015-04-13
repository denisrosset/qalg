package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait ConvertedMatMutable[M, @sp(Double, Long) A, J] extends Any
    with Converted[A, J]
    with MatMutable[M, A] {
  def source: MatMutable[M, J]
  def update(m: M, r: Int, c: Int, a: A): Unit = source.update(m, r, c, aToJ(a))
  override def update(m: M, rows: At1, c: Int, a: A): Unit = source.update(m, rows, c, aToJ(a))
  override def update(m: M, r: Int, cols: At1, a: A): Unit = source.update(m, r, cols, aToJ(a))
  override def update(m: M, rows: ::.type, c: Int, a: A): Unit = source.update(m, rows, c, aToJ(a))
  override def update(m: M, r: Int, cols: ::.type, a: A): Unit = source.update(m, r, cols, aToJ(a))
  override def update(m: M, rows: At1, cols: At1, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  override def update(m: M, rows: ::.type, cols: ::.type, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  override def update(m: M, rows: ::.type, cols: At1, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  override def update(m: M, rows: At1, cols: ::.type, a: A): Unit = source.update(m, rows, cols, aToJ(a))
  def copy(m: M): M = source.copy(m)
}
