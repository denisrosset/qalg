package com.faacets.qalg
package optional

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait VecOrder[V, A] extends Order[V] {
  implicit def V: Vec[V, A]
  implicit def orderA: Order[A]

  def compare(u: V, v: V): Int = {
    assert(u.length == v.length)
    cforRange(0 until u.length) { i =>
      val compareValue = orderA.compare(u(i), v(i))
      if (compareValue != 0)
        return compareValue
    }
    0
  }
}
