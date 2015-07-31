package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

import spire.syntax.cfor._

trait Index2[T2, +R2, +R1, @sp A] extends Any with Size2[T2] with Index[T2, IntInt, A] {
  def apply(t: T2, i0: Int, i1: Int): A
  def offsetX0(t: T2, offset: Offset): Int
  def offsetX1(t: T2, offset: Offset): Int
}
