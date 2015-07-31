package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait VecShift[V] extends Any {
  def circShifted(v: V, shift: Int): V
  def permuted(v: V, k1: Int, k2: Int): V
  def permuted(v: V, perm: Array[Int]): V
}

object VecShift {
  implicit def fromPack[V, @sp(Double, Long) A](implicit pack: PackRing.ForV[V, A]): VecShift[V] = pack.VShift
}

trait MutableVecShift[V] extends Any with VecShift[V] {
  def circShift(v: V, shift: Int): Unit
  def permute(v: V, k1: Int, k2: Int): Unit
  def permuteInverse(v: V, inversePerm: Array[Int]): Unit
}

object MutableVecShift {
  implicit def fromPack[V, @sp(Double, Long) A](implicit pack: PackRingMutable.ForV[V, A]): MutableVecShift[V] = pack.VShift
}
