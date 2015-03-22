package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.syntax.ring._
import util._

trait VecInRing[V, @sp(Double, Long) A] extends Any with VecBuilder[V, A] with Module[V, A] with Monoid[V] { self =>
  implicit def scalar: Ring[A]
  def zero: V = fromFunV(FunV.empty[A])
  def id: V = fromFunV(FunV.fill[A](1)(scalar.one))
  def op(x: V, y: V): V = fromFunV(new FunV[A] {
    val nx = self.length(x)
    val ny = self.length(y)
    def len = nx * ny
    def f(k: Int): A = {
      val kx = k / ny
      val ky = k % ny
      self.apply(x, kx) * self.apply(y, ky)
    }
  })
  def plus(x: V, y: V): V = {
    require(length(x) == length(y))
    fromFunV(new FunV[A] {
      def len = self.length(x)
      def f(k: Int): A = self.apply(x, k) + self.apply(y, k)
    })
  }
  override def minus(x: V, y: V): V = {
    require(length(x) == length(y))
    fromFunV(new FunV[A] {
      def len = self.length(x)
      def f(k: Int): A = self.apply(x, k) - self.apply(y, k)
    })
  }
  def negate(v: V): V = fromFunV(new FunV[A] {
    def len = self.length(v)
    def f(k: Int): A = -self.apply(v, k)
  })
  def timesl(a: A, v: V): V = fromFunV(new FunV[A] {
    def len = self.length(v)
    def f(k: Int): A = a * self.apply(v, k)
  })
}

object VecInRing {
  def apply[V, @sp(Double, Long) A](implicit V: VecInRing[V, A]): VecInRing[V, A] = V
}

trait ConvertedVecInRing[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVecBuilder[V, A, J]
    with VecInRing[V, A] {
  def source: VecInRing[V, J]
  override def zero: V = source.zero
  override def id: V = source.id
  override def op(x: V, y: V): V = source.op(x, y)
  override def plus(x: V, y: V): V = source.plus(x, y)
  override def minus(x: V, y: V): V = source.minus(x, y)
  override def negate(v: V): V = source.negate(v)
  override def timesl(a: A, v: V): V = source.timesl(aToJ(a), v)
}
