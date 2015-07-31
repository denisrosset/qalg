package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

import spire.math.Rational

trait Zero[@sp(Double, Long) A] {
  def zero: A
  def canBeNonZero(a: A): Boolean
}

object Zero {
  def apply[@sp(Double, Long) A](implicit Z: Zero[A]): Zero[A] = Z
  implicit object IntZero extends Zero[Int] {
    def zero: Int = 0
    def canBeNonZero(a: Int): Boolean = a != 0
  }
  implicit object LongZero extends Zero[Long] {
    def zero: Long = 0L
    def canBeNonZero(a: Long): Boolean = a != 0L
  }
  implicit object DoubleZero extends Zero[Double] {
    def zero: Double = 0.0
    def canBeNonZero(a: Double): Boolean = a != 0.0
  }
  implicit object RationalZero extends Zero[Rational] {
    def zero: Rational = Rational.zero
    def canBeNonZero(a: Rational): Boolean = a.signum != 0
  }
}
