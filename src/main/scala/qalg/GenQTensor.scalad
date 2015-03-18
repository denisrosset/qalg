package com.faacets
package qalg

import spire.math.{SafeLong, Rational, lcm, gcd}
import spire.syntax.eq._

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
  protected[qalg] def unsafeToArray: Array[Rational]

  def commonFactor: Rational = {
    val (gcdNum, lcmDen) = factors1
    if (gcdNum == 0)
      Rational.one
    else
      Rational(gcdNum, lcmDen)
  }

  // TODO: rename back, temp rename because return values have been switched
  def factors1: (BigInt, BigInt) = {
    var lcmDen = SafeLong.one
    var gcdNum = SafeLong.zero
    var i = 0
    while (i < length) {
      lcmDen = lcm(lcmDen, apply(i).denominator)
      gcdNum = gcd(gcdNum, apply(i).numerator)
      i += 1
    }
    (gcdNum.toBigInt, lcmDen.toBigInt)
  }
}
