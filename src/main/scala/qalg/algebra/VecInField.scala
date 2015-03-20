package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecInField[VA, @sp(Double, Long) A] extends Any with VecInRing[VA, A] with VectorSpace[VA, A] {
  implicit def scalar: Field[A]
}

object VecInField {
  implicit def fromMatVecInField[M, V, @sp(Double, Long) A](implicit MV: MatVecInField[M, V, A]): Vec[V, A] = MV.V
}
