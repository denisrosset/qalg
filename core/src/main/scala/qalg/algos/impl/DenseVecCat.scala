package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

final class DenseVecCatImpl[V1, @sp(Double, Long) A](implicit val V1: VecBuild[V1, A]) extends VecCat[V1, A] {

  def cat[V2](first: V1, rest: V2*)(implicit V2: Vec[V2, A]): V1 = {
    @tailrec def computeLength(acc: Int, it: Iterator[V2]): Int =
      if (it.hasNext) computeLength(acc + it.next.length, it) else acc
    val l = computeLength(first.length, rest.iterator)
    val b = V1.builder(l, V1.options(first))
    @tailrec def fill(start: Int, it: Iterator[V2]): Unit =
      if (it.hasNext) {
        val v2 = it.next
        cforRange(0 until v2.length) { i =>
          b.add(start + i, v2(i))
        }
        fill(start + v2.length, it)
      }
    cforRange(0 until first.length) { i =>
      b.add(i, first(i))
    }
    fill(first.length, rest.iterator)
    b.result()
  }
}
