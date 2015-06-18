package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait Vec[V, @sp(Double, Long) A] extends Any with Lin[V, A] with Index1[V, A] { self =>

  def sameShape(x: V, y: V): Boolean = length(x) == length(y)
  def linearLength(v: V): Int = length(v)
  def linearApply(v: V, k: Int): A = apply(v, k)

  def length(v: V): Int
  def toIndexedSeq(v: V): IndexedSeq[A] = new IndexedSeq[A] {
    def length: Int = self.length(v)
    def apply(k: Int): A = self.apply(v, k)
  }
  def view(v: V, genAt: At1): FunV[A] = {
    val at = genAt.forVec(v)(self)
    new FunV[A] {
      def len: Int = at.length
      def f(k: Int): A = self.apply(v, at(k))
    }
  }
}

object Vec {
  def apply[V, @sp(Double, Long) A](implicit V: Vec[V, A]): Vec[V, A] = V
  implicit def fromPack[V, @sp(Double, Long) A](implicit ev: PackVR[V, A]): Vec[V, A] = ev.V
}
