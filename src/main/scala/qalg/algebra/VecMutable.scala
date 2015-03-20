package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecMutable[V, @sp(Double, Long) A] extends Any { self =>
  implicit def V: Vec[V, A]

  def update(v: V, k: Int, a: A): Unit

  def update(v: V, at: At1, a: A): Unit = {
    val n = at.length
    cforRange(0 until n) { k =>
      update(v, at(k), a)
    }
  }

  def update(v: V, at: ::.type, a: A): Unit =
    update(v, AtRange1(0 until V.length(v)), a)

  def update[V1](v: V, at: At1, v1: V1)(implicit V1: Vec[V1, A]): Unit = {
    val n = at.length
    require(n == V1.length(v1))
    cforRange(0 until n) { k =>
      update(v, at(k), V1.apply(v1, k))
    }
  }

  def update[V1](v: V, at: ::.type, v1: V1)(implicit V1: Vec[V1, A]): Unit =
    update(v, AtRange1(0 until V.length(v)), v1)
}

object VecMutable {
  implicit def fromMatVecMutable[M, V, @sp(Double, Long) A](implicit MV: MatVecMutable[M, V, A]): VecMutable[V, A] = MV.V
}
