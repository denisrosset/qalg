package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait LinInEuclideanRing[LA, @sp(Double, Long) A] extends Any with LinBuilder[LA, A] { self =>
  implicit def A: EuclideanRing[A]

  def commonFactor(l: LA): A = {
    var cumGcd = A.zero
    cforRange(0 until linearLength(l)) { k =>
      val a = linearApply(l, k)
      if (!A.isZero(a))
        cumGcd = spire.math.gcd(cumGcd, a)
    }
    cumGcd
  }

  def withPrimes(l: LA): LA = {
    val cf = commonFactor(l)
    map(l)( a => A.quot(a, cf) )
  }
}
