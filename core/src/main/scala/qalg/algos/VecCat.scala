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
import util._

trait VecCat[V1, @sp(Double, Long) A] extends Any {
  def cat[V2](first: V1, rest: V2*)(implicit V2: Vec[V2, A]): V1
}

object VecCat {
  implicit def fromPack[V, @sp(Double, Long) A](implicit pack: PackRing.ForV[V, A]): VecCat[V, A] = pack.VCat
}
