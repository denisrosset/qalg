package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecInField[M, V, @sp(Double, Long) A] extends Any
    with MatVecInRing[M, V, A]
    with MatInField[M, A] { self =>
  implicit def V: VecInField[V, A]
}

object MatVecInField {
  def apply[M, V, @sp(Double, Long) A](implicit MV: MatVecInField[M, V, A]): MatVecInField[M, V, A] = MV
}

trait ConvertedMatVecInField[M, V, @sp(Double, Long) A, J] extends Any
    with ConvertedMatVecInRing[M, V, A, J]
    with ConvertedMatInField[M, A, J]
    with MatVecInField[M, V, A] {
  def source: MatVecInField[M, V, J]
}
