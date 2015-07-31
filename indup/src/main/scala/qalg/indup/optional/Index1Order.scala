package com.faacets.qalg
package indup
package optional

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._

import algebra._

trait Index1Order[T, A] extends Order[T] {
  implicit def I: Index1[T, _, A]
  implicit def orderA: Order[A]

  def compare(u: T, v: T): Int = {
    assert(I.size(u) == I.size(v))
    cforRange(0 until I.size(u)) { i =>
      val compareValue = orderA.compare(I.apply(u, i), I.apply(v, i))
      if (compareValue != 0)
        return compareValue
    }
    0
  }
}
