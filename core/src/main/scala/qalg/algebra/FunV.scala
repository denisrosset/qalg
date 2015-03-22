package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.additiveMonoid._
import spire.syntax.cfor._

import algebra._

trait FunV[@sp(Double, Long) A] extends Any {
  override def toString = (0 until len).map(f(_)).mkString("(", ",", ")")
  def f(k: Int): A
  def len: Int
  /** Returns the next non-zero element after index `k`, or -1 if there is no such element. */
  def nextNZ(k: Int)(implicit am: AdditiveMonoid[A], eq: Eq[A]): Int = {
    cforRange(k + 1 until len) { newK =>
      if (!f(newK).isZero) return newK
    }
    -1
  }
}

object FunV {
  def empty[@sp(Double, Long) A]: FunV[A] = new FunV[A] {
    def len = 0
    def f(k: Int) = sys.error("Cannot get element of empty vector.")
  }
  def fill[@sp(Double, Long) A](n: Int)(a: A): FunV[A] = new FunV[A] {
    def len = n
    def f(k: Int) = a
  }
}

class FunVec[@sp(Double, Long) A](implicit val scalar: AdditiveMonoid[A], val eqA: Eq[A]) extends VecBuilder[FunV[A], A] {
  def length(v: FunV[A]): Int = v.len
  def apply(v: FunV[A], k: Int): A = v.f(k)
  def fromFunV(v: FunV[A]): FunV[A] = v
}
