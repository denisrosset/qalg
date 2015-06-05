package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.syntax.ring._
import util._

trait VecInEuclideanRing[V, @sp(Double, Long) A] extends Any with VecInRing[V, A] with LinInEuclideanRing[V, A] {
  implicit def A: EuclideanRing[A]
  override def scalar = A
  def withPrimes(v: V): V
  def commonFactor(v: V): A
}

object VecInEuclideanRing {
  def apply[V, @sp(Double, Long) A](implicit V: VecInEuclideanRing[V, A]): VecInEuclideanRing[V, A] = V
}
