package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecBuilder[VA, @sp(Double, Long) A] extends Any with Vec[VA, A] { self =>
  implicit def scalar: AdditiveMonoid[A]
  def from(v: FunV[A]): VA
  def apply(v: VA, at: At1): VA = from(view(v, at))
  def apply(v: VA, at: ::.type): VA = from(view(v, at))
}

object VecBuilder {
  implicit def fromMatVecBuilder[M, V, @sp(Double, Long) A](implicit MV: MatVecBuilder[M, V, A]): VecBuilder[V, A] = MV.V

  def apply[VA, @sp(Double, Long) A](implicit VA: VecBuilder[VA, A]): VecBuilder[VA, A] = VA
}
