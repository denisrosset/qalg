package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.syntax.ring._
import util._

trait VecInRing[VA, @sp(Double, Long) A] extends Any with VecBuilder[VA, A] with Module[VA, A] with Monoid[VA] { self =>
  implicit def scalar: Ring[A]
  def zero: VA = from(FunV.empty[A])
  def id: VA = from(FunV.fill[A](1)(scalar.one))
  def op(x: VA, y: VA): VA = from(new FunV[A] {
    val nx = self.length(x)
    val ny = self.length(y)
    def len = nx * ny
    def f(k: Int): A = {
      val kx = k / ny
      val ky = k % ny
      self.apply(x, kx) * self.apply(y, ky)
    }
  })
  def plus(x: VA, y: VA): VA = {
    require(length(x) == length(y))
    from(new FunV[A] {
      def len = self.length(x)
      def f(k: Int): A = self.apply(x, k) + self.apply(y, k)
    })
  }
  override def minus(x: VA, y: VA): VA = {
    require(length(x) == length(y))
    from(new FunV[A] {
      def len = self.length(x)
      def f(k: Int): A = self.apply(x, k) - self.apply(y, k)
    })
  }
  def negate(v: VA): VA = from(new FunV[A] {
    def len = self.length(v)
    def f(k: Int): A = -self.apply(v, k)
  })
  def timesl(a: A, v: VA): VA = from(new FunV[A] {
    def len = self.length(v)
    def f(k: Int): A = a * self.apply(v, k)
  })
}

object VecInRing {
  implicit def fromMatVecInRing[M, V, @sp(Double, Long) A](implicit MV: MatVecInRing[M, V, A]): Vec[V, A] = MV.V
}
