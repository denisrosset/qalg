package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait Lin[LA, @sp(Double, Long) A] extends Any with Eq[LA] { self =>
  implicit def eqA: Eq[A]

  def sameShape(x: LA, y: LA): Boolean
  def linearApply(l: LA, k: Int): A
  def linearLength(l: LA): Int
  def eqv(x: LA, y: LA): Boolean =
    sameShape(x, y) && {
      var i = 0
      val n = linearLength(x)
      while (i < n && linearApply(x, i) === linearApply(y, i)) {
        i += 1
      }
      i == n
    }
}
