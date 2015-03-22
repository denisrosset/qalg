package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecBuilder[V, @sp(Double, Long) A] extends Any with Vec[V, A] { self =>
  implicit def scalar: AdditiveMonoid[A]
  def from(v: FunV[A]): V
  def apply(v: V, at: At1): V = from(view(v, at))
  def apply(v: V, at: ::.type): V = from(view(v, at))
}

object VecBuilder {
  def apply[V, @sp(Double, Long) A](implicit V: VecBuilder[V, A]): VecBuilder[V, A] = V
}

trait ConvertedVecBuilder[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVec[V, A, J]
    with VecBuilder[V, A] {
  def source: VecBuilder[V, J]

  def from(v: FunV[A]): V = source.from(new FunV[J] {
    def len: Int = v.len
    def f(k: Int): J = aToJ(v.f(k))
  })
  override def apply(v: V, at: At1): V = source(v, at)
  override def apply(v: V, at: ::.type): V = source(v, at)
}
