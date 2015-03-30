package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecInField[V, @sp(Double, Long) A] extends Any with VecInRing[V, A] with VectorSpace[V, A] {
  implicit def scalar: Field[A]
}

object VecInField {
  def apply[V, @sp(Double, Long) A](implicit V: VecInField[V, A]): VecInField[V, A] = V
}

trait ConvertedVecInField[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVecInRing[V, A, J]
    with VecInField[V, A] {
  def source: VecInField[V, J]
}
