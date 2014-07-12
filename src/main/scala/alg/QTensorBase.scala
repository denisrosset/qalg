package com.faacets
package alg

import spire.math.Rational
import spire.syntax.eq._
import net.alasc.Index

trait QTensorBase[T <: QTensorBase[T]] extends GenQTensor {
  self: T =>
  def mapElements(f: Rational => Rational): T

  def compact = mapElements( r => RationalCache.apply(r) )

  /** Returns a deep copy of this if mutable, or directly this if immutable. */
  def copy: T

  /** Returns a vector/matrix with prime integer coefficients and a multiplicative factor,
  * such that the returned vector/matrix * factor is equal to the original vector/matrix. */
  def withPrimes: (T, Rational) = {
    val cf = commonFactor
    if (cf === Rational.zero)
      (this, Rational.one)
    else
      (mapElements(_ / cf), cf)
  }
}

class QTensorBaseIndex[T <: QTensorBase[T]] extends Index[Rational, T] {
  def indexLength(t: T) = t.length
  def indexElement(t: T, i: Int) = t(i)
}
