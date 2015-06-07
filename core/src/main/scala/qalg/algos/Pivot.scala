package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.math.Rational

trait Pivot[@sp(Double) A] extends Any { // not @sp(Long)
  /** Priority of the given pivot element.
    * 0 = x should not be chosen
    * Higher values are better.
    */
  def priority(x: A): Double
}

object Pivot {
  implicit object double extends Pivot[Double] {
    def priority(x: Double) = x.abs
  }
  implicit object rational extends Pivot[Rational] {
    def priority(x: Rational) = x.toDouble.abs // TODO: implement simplest denominator/numerator bitlength selection
  }
}
