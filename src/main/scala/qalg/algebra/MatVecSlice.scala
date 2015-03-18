package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.cfor._
import util._

trait MatVecSlice[MA, VA, @sp(Double, Long) A] extends Any with Mat[MA, A] {
  implicit def VA: VecBuilder[VA, A]
  def apply(m: MA, rows: ::.type, c: Int): VA = VA.from(view(m, rows, c))
  def apply(m: MA, rows: At1, c: Int): VA = VA.from(view(m, rows, c))
  def apply(m: MA, r: Int, cols: ::.type): VA = VA.from(view(m, r, cols))
  def apply(m: MA, r: Int, cols: At1): VA = VA.from(view(m, r, cols))
}
