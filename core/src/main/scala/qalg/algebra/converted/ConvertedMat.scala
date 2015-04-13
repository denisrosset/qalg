package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait ConvertedMat[M, @sp(Double, Long) A, J] extends Any
    with Converted[A, J]
    with Mat[M, A] {
  def source: Mat[M, J]

  override def sameShape(x: M, y: M): Boolean = source.sameShape(x, y)
  override def linearLength(m: M): Int = source.linearLength(m)
  override def linearApply(m: M, k: Int): A = jToA(source.linearApply(m, k))
  override def size(m: M): IntInt = source.size(m)
  def nRows(m: M): Int = source.nRows(m)
  def nCols(m: M): Int = source.nCols(m)
  def apply(m: M, r: Int, c: Int): A = jToA(source(m, r, c))
}
