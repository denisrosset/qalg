package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational

trait RationalField[A] extends Field[A] with Order[A] {
  def numerator(a: A): BigInt
  def denominator(a: A): BigInt
  def ratio(numerator: BigInt, denominator: BigInt): A
  def toRational(a: A): Rational = Rational(numerator(a), denominator(a))
  def fromRational(r: Rational): A = ratio(r.numerator, r.denominator)

  // EuclideanRing

  def gcd(x: A, y: A): A = 
    fromRational(toRational(x).gcd(toRational(y)))
  def mod(x: A, y: A): A = 
    fromRational(toRational(x).gcd(toRational(y)))
  def quot(x: A, y: A): A = 
    fromRational(toRational(x).gcd(toRational(y)))
}
