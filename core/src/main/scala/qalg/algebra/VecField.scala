package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import indup.algebra._

trait VecField[V, @sp(Double, Long) A] extends Any with VecRing[V, A] with InnerProductSpace[V, A] { self =>
  override implicit def scalar: Field[A]

  // override def dot(x: V, y: V): A 

  // override def divr(v: V, a: A): V
}

object VecField {
  def apply[V, @sp(Double, Long) A](implicit V: VecField[V, A]): VecField[V, A] = V
//  implicit def fromPack[V, @sp(Double, Long) A](implicit ev: PackVR[V, A]): VecRing[V, A] = ev.V
}
