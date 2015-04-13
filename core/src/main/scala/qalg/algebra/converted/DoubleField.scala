package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.std.double._
import spire.syntax.field._

trait DoubleField[A] extends Field[A] with Order[A] {
  def toDouble(a: A): Double
  def fromDouble(r: Double): A

  // EuclideanRing

  def gcd(x: A, y: A): A = 
    fromDouble(toDouble(x).gcd(toDouble(y)))
  def mod(x: A, y: A): A = 
    fromDouble(toDouble(x).gcd(toDouble(y)))
  def quot(x: A, y: A): A = 
    fromDouble(toDouble(x).gcd(toDouble(y)))
}
