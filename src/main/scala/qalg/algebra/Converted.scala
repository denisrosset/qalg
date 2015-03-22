package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.math.Rational

trait Converted[@sp(Double, Long) A, J] extends Any {
  def aToJ(a: A): J
  def jToA(j: J): A
}

trait RationalConverted[J] extends Any
    with Converted[Rational, J] {
  implicit def rationalFieldJ: RationalField[J]
  def aToJ(a: Rational): J = rationalFieldJ.fromRational(a)
  def jToA(j: J): Rational = rationalFieldJ.toRational(j)
}
