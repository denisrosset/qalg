package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecBuilder[VA, @sp(Double, Long) A] extends Any with Vec[VA, A] { self =>
  implicit def scalar: AdditiveMonoid[A]
  def from(v: FunV[A]): VA
  def apply(v: VA, at: At1): VA = from(new FunV[A] {
    def len: Int = at.length
    def f(k: Int): A = self.apply(v, at(k))
  })
}

object VecBuilder {
  def apply[VA, @sp(Double, Long) A](implicit VA: VecBuilder[VA, A]): VecBuilder[VA, A] = VA
}
