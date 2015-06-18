package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecMutable[V, @sp(Double, Long) A] extends Any with Update1[V, A] { self =>
  implicit def V: Vec[V, A]

  def copy(v: V): V

  def update(v: V, genAt: At1, a: A): Unit = {
    val at = genAt.forVec(v)(V)
    val n = at.length
    cforRange(0 until n) { k =>
      update(v, at(k), a)
    }
  }

  def update[V1](v: V, genAt: At1, v1: V1)(implicit V1: Vec[V1, A]): Unit = {
    val at = genAt.forVec(v)(V)
    val n = at.length
    require(n == V1.length(v1))
    cforRange(0 until n) { k =>
      update(v, at(k), V1.apply(v1, k))
    }
  }
}

object VecMutable {
  def apply[V, @sp(Double, Long) A](implicit V: VecMutable[V, A]): VecMutable[V, A] = V
  implicit def fromPack[V, @sp(Double, Long) A](implicit ev: PackUV[V, A]): VecMutable[V, A] = ev.UV
}
