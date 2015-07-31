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

trait Kron[T] {
  def kron(x: T, y: T): T
}

object Kron {
  implicit def matKronFromPack[M](implicit pack: PackRing.ForM[M, _]): Kron[M] = pack.MKron
  implicit def vecKronFromPack[V](implicit pack: PackRing.ForV[V, _]): Kron[V] = pack.VKron
}
