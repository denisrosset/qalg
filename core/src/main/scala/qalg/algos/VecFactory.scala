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
  implicit val V: VecRing[V, _]
  def zeros(n: Int, options: V.Options = V.defaultOptions): V
  def ones(n: Int, options: V.Options = V.defaultOptions): V
}

object VecFactory {
  implicit def fromPack[V, @sp(Double, Long) A](implicit pack: PackRing.ForV[V, _]): VecFactory[V] = pack.VFactory
}
