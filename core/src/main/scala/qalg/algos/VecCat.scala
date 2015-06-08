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
  implicit def fromAlg[V, @sp(Double, Long) A](ev: AlgVR[V, A]): VecCat[V, A] = ev.VCat
}

final class VecCatImpl[V1, @sp(Double, Long) A](implicit V1: VecBuilder[V1, A]) extends VecCat[V1, A] {

  def cat[V2](first: V1, rest: V2*)(implicit V2: Vec[V2, A]): V1 = {
    val startIndices = (first.length +: rest.map(_.length)).scanLeft(0)(_ + _)
    val ranges = (startIndices zip startIndices.tail).zipWithIndex map {
      case ((start, nextStart), i) => (start until nextStart) -> i
    }
    val map = RangeMap(ranges: _*)
    V1.tabulate(startIndices.last) { k =>
      val vec = map(k)
      if (vec == 0) first(k) else {
        val vecK = k - startIndices(vec)
        rest(vec - 1)(vecK)
      }
    }
  }
}
