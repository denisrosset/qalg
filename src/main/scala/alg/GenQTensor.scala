package com.faacets
package alg

import spire.math.{SafeLong, Rational, lcm, gcd}
import spire.syntax.eq._
import net.alasc.Index

object GenQTensor {
  def seed = 0x3EED4E43
}

trait GenQTensor {
  def length: Int
  /** Retries the i-th element of this (matrices: in zero-based column-major order). */
  def apply(i: Int): Rational

  def sameShape(that: GenQTensor): Boolean = (GenQTensor.this, that) match {
    case (a: GenQMatrix, b: GenQMatrix) => a.rows == b.rows && a.cols == b.cols
    case (a: GenQVector, b: GenQVector) => a.length == b.length
    case _ => false
  }

  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3
    var h = GenQTensor.seed
    var k = 0
    while (k < length) {
      h = MurmurHash3.mix(h, apply(k).hashCode)
      k += 1
    }
    MurmurHash3.finalizeHash(h, length)
  }

  override def equals(any: Any): Boolean = any match {
    case that: GenQTensor => 
      if (!sameShape(that))
        return false
      var i = 0
      while (i < length) {
        if( apply(i) =!= that.apply(i) )
          return false
        i += 1
      }
      true
    case _ => false
  }

  def toArray: Array[Rational]
  protected[alg] def unsafeToArray: Array[Rational]

  def commonFactor: Rational = {
    var lcmDen = SafeLong.one
    var gcdNum = SafeLong.zero
    var i = 0
    while (i < length) {
      lcmDen = lcm(lcmDen, apply(i).denominator)
      gcdNum = gcd(gcdNum, apply(i).numerator)
      i += 1
    }
    Rational(gcdNum, lcmDen)
  }

/*
  def factors: (BigInt, BigInt) = {
    val lcmDenominator = elements.map(_.denominator).fold(BigInt(1))(lcm)
    val gcdNumerator = elements.map(_.numerator).fold(BigInt(0))(gcd)
    ((lcmDenominator.abs, gcdNumerator.abs))
  }

  def commonFactor: Rational = {
    val (lcmDenominator, gcdNumerator) = factors
    Rational(gcdNumerator, lcmDenominator)
  }

  def integerCoefficients: (Array[BigInt], BigInt) = {
    val commonDenominator = elements.map(_.denominator).fold(BigInt(1))(lcm)
    val coeffsArray = elements.map( r => r.numerator * (commonDenominator/r.denominator) ).toArray
    (coeffsArray, commonDenominator)
  }

  def longCoefficients: (Array[Long], Long) = {
    val (coeffsArray, commonDenominator) = integerCoefficients
    if (coeffsArray.exists(!_.isValidLong))
      throw new IllegalArgumentException("Coefficients do not fit into a long.")
    if (!commonDenominator.isValidLong)
      throw new IllegalArgumentException("Denominator does not fit into a long.")
    (coeffsArray.map(_.longValue), commonDenominator.longValue)
  }
*/
}
