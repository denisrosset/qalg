package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.ring._

trait VecRing[V, @sp(Double, Long) A] extends Any with VecBuild[V, A] with Module[V, A] { self =>
  override implicit def scalar: Ring[A]

  def zero: V = builder(0).result()

  def id: V = fill(1)(scalar.one)

  // def plus(x: V, y: V): V

  // override def minus(x: V, y: V): V
  // def negate(v: V): V = {

  //  def timesl(a: A, v: V): V
  //  override def timesr(v: V, a: A): V
}

object VecRing {
  def apply[V, @sp(Double, Long) A](implicit V: VecRing[V, A]): VecRing[V, A] = V
}
