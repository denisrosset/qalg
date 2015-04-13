package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecBuilder[V, @sp(Double, Long) A] extends Any with Vec[V, A] { self =>

  // type class methods
  def apply(v: V, at: At1): V = tabulate(at.length)(k => apply(v, at(k)))
  def apply(v: V, at: ::.type): V = tabulate(length(v))(k => apply(v, k))

  // direct methods
  def from[V1](v1: V1)(implicit V1: Vec[V1, A]): V = tabulate(V1.length(v1))(V1.apply(v1, _))
  def build(elements: A*): V = tabulate(elements.size)(elements(_))
  def tabulate(n: Int)(f: Int => A): V
  def fill(n: Int)(a: A): V = tabulate(n)(k => a)
}

object VecBuilder {
  def apply[V, @sp(Double, Long) A](implicit V: VecBuilder[V, A]): VecBuilder[V, A] = V
}

trait ConvertedVecBuilder[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVec[V, A, J]
    with VecBuilder[V, A] {
  def source: VecBuilder[V, J]

  def tabulate(n: Int)(f: Int => A): V = source.tabulate(n)(k => aToJ(f(k)))
  override def apply(v: V, at: At1): V = source(v, at)
  override def apply(v: V, at: ::.type): V = source(v, at)
}
