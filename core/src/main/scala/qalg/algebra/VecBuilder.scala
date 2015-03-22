package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecBuilder[V, @sp(Double, Long) A] extends Any with Vec[V, A] { self =>
  def fromFunV(v: FunV[A]): V
  def from[V1](v1: V1)(implicit V1: Vec[V1, A]): V = fromFunV(V1.view(v1, ::))
  def apply(v: V, at: At1): V = fromFunV(view(v, at))
  def apply(v: V, at: ::.type): V = fromFunV(view(v, at))
  def build(elements: A*): V = fromFunV(new FunV[A] {
    def len = elements.size
    def f(k: Int): A = elements(k)
  })
}

object VecBuilder {
  def apply[V, @sp(Double, Long) A](implicit V: VecBuilder[V, A]): VecBuilder[V, A] = V
}

trait ConvertedVecBuilder[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVec[V, A, J]
    with VecBuilder[V, A] {
  def source: VecBuilder[V, J]

  def fromFunV(v: FunV[A]): V = source.fromFunV(new FunV[J] {
    def len: Int = v.len
    def f(k: Int): J = aToJ(v.f(k))
  })
  override def apply(v: V, at: At1): V = source(v, at)
  override def apply(v: V, at: ::.type): V = source(v, at)
}
