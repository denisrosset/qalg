package com.faacets.qalg
package util

import spire.math.Rational

object RationalCache extends UniquenessCache[(Int, Int), Rational] {
  var maxAbsValue = 100
  def apply(key: Rational): Rational =
    if (key.numerator.abs <= maxAbsValue && key.denominator.abs <= maxAbsValue)
      apply(key.numerator.toInt, key.denominator.toInt)
    else
      key
  override def apply(key: (Int, Int)): Rational =
    if (key._1.abs <= maxAbsValue && key._2.abs <= maxAbsValue)
      super.apply(key)
    else
      Rational(key._1, key._2)
  protected def valueFromKey(key: (Int, Int)): Rational = Rational(key._1, key._2)
  protected def keyFromValue(value: Rational): Option[(Int, Int)] =
    if (value.numerator.isValidInt && value.denominator.isValidInt)
      Some((value.numerator.toInt, value.denominator.toInt))
    else
      None
}

