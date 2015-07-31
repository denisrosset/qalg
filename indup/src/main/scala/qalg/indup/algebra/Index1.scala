package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

import spire.syntax.cfor._

trait Index1[T1, +R1, @sp A] extends Any with Size1[T1] with Index[T1, Int, A] { self =>
  def apply(t: T1, i: Int): A
  def offsetX0(t: T1, offset: Offset): Int

  def toIndexedSeq(t: T1): IndexedSeq[A] = new IndexedSeq[A] {
    def length: Int = self.size(t)
    def apply(k: Int): A = self.apply(t, k)
  }
}
