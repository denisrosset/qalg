package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait ConvertedMatVecBuilder[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMatVec[M, V, A, J]
    with ConvertedMatBuilder[M, A, J]
    with MatVecBuilder[M, V, A] {
  def source: MatVecBuilder[M, V, J]
  override def apply(m: M, rows: ::.type, c: Int): V = source(m, rows, c)
  override def apply(m: M, rows: At1, c: Int): V = source(m, rows, c)
  override def apply(m: M, r: Int, cols: ::.type): V = source(m, r, cols)
  override def apply(m: M, r: Int, cols: At1): V = source(m, r, cols)
}
