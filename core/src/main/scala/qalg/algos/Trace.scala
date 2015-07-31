package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra.Ring
import spire.syntax.cfor._
import spire.syntax.ring._

import algebra._

trait Trace[M, @sp(Double, Long) A] extends Any {
  /** Computes the trace of the given matrix. */
  def trace(m: M): A
}

object Trace {
  implicit def fromPack[M, @sp(Double, Long) A](implicit pack: PackRing.ForM[M, A]): Trace[M, A] = pack.MTrace
}
