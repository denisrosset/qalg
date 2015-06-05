package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.ring._
import util._

trait MatInEuclideanRing[M, @sp(Double, Long) A] extends Any with MatInRing[M, A] with LinInEuclideanRing[M, A] { self =>
  implicit def A: EuclideanRing[A]
  override def scalar = A
  def withPrimes(m: M): M
  def commonFactor(m: M): A
}

object MatInEuclideanRing {
  def apply[M, @sp(Double, Long) A](implicit M: MatInEuclideanRing[M, A]): MatInEuclideanRing[M, A] = M
}
