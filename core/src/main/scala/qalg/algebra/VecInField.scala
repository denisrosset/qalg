package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.field._
import spire.syntax.cfor._
import util._

trait VecInField[V, @sp(Double, Long) A] extends Any with VecInEuclideanRing[V, A] with InnerProductSpace[V, A] {
  implicit def A: Field[A]
  override def scalar = A
  
  def dot(x: V, y: V): A = {
    require(length(x) == length(y))
    var acc = A.zero
    cforRange(0 until length(x)) { k =>
      acc += apply(x, k) * apply(y, k)
    }
    acc
  }
}

object VecInField {
  def apply[V, @sp(Double, Long) A](implicit V: VecInField[V, A]): VecInField[V, A] = V
}
