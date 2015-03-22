package com.faacets.qalg
package algos

import spire.algebra._
import spire.math._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait QAlgos {
  def commonFactor[L](l: L)(implicit L: Lin[L, Rational]): Rational = {
    var lcmDen = SafeLong.one
    var gcdNum = SafeLong.zero
    cforRange(0 until l.linearLength) { k =>
      lcmDen = lcm(lcmDen, l.linearApply(k).denominator)
      gcdNum = gcd(gcdNum, l.linearApply(k).numerator)
    }
    Rational(gcdNum, lcmDen)
  }
  def withPrimes[L](l: L)(implicit L: Lin[L, Rational] with VectorSpace[L, Rational]): (L, Rational) = {
    val cf = commonFactor(l)
    (l :/ cf, cf)
  }
}
