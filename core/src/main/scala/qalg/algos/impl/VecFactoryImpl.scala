package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

final class VecFactoryImpl[V, @sp(Double, Long) A](implicit val V: VecRing[V, A]) extends VecFactory[V] {
  import V.scalar

  def zeros(n: Int, options: V.Options = V.defaultOptions): V = V.fill(n, options)(scalar.zero)
  def ones(n: Int, options: V.Options = V.defaultOptions): V = V.fill(n, options)(scalar.one)
}
