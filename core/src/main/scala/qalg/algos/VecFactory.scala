package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait VecFactory[V] extends Any {
  def zeros(n: Int): V
  def ones(n: Int): V
}

object VecFactory {
  implicit def fromAlg[V, @sp(Double, Long) A](implicit ev: AlgVR[V, A]): VecFactory[V] = ev.VFactory
}

final class VecFactoryImpl[V, @sp(Double, Long) A](implicit V: VecInRing[V, A]) extends VecFactory[V] {
  implicit def A: Ring[A] = V.A

  def zeros(n: Int): V = V.fill(n)(A.zero)
  def ones(n: Int): V = V.fill(n)(A.one)
}
