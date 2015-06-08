package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.syntax.ring._
import util._

trait VecInRing[V, @sp(Double, Long) A] extends Any with VecBuilder[V, A] with Module[V, A] { self =>
  implicit def A: Ring[A]
  override def scalar = A

  def zero: V = tabulate(0)(k => sys.error("Cannot be called"))

  def id: V = fill(1)(A.one)

  def plus(x: V, y: V): V = {
    require(length(x) == length(y))
    tabulate(length(x))( k => apply(x, k) + apply(y, k) )
  }

  override def minus(x: V, y: V): V = {
    require(length(x) == length(y))
    tabulate(length(x))( k => apply(x, k) - apply(y, k) )
  }

  def negate(v: V): V = tabulate(length(v))( k => -apply(v, k) )

  def timesl(a: A, v: V): V = tabulate(length(v))( k => a * apply(v, k) )
  
}

object VecInRing {
  def apply[V, @sp(Double, Long) A](implicit V: VecInRing[V, A]): VecInRing[V, A] = V
  implicit def fromPack[V, @sp(Double, Long) A](implicit ev: PackVR[V, A]): VecInRing[V, A] = ev.V
}
