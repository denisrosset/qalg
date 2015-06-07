package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.math.Rational

trait Pivot[@sp(Double) A] extends Any { // not @sp(Long)
  /** Checks if x is a better pivot than y. */
  def betterPivot(x: A, y: A): Boolean
}

object Pivot {
  implicit object double extends Pivot[Double] {
    def betterPivot(x: Double, y: Double) = x.abs > y.abs
  }
  implicit object rational extends Pivot[Rational] {
    def betterPivot(x: Rational, y: Rational) = x.abs > y.abs // TODO: implement simplest denominator/numerator bitlength selection
  }
}
