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
  implicit def fromAlg[V, @sp(Double, Long) A](implicit ev: AlgVR[V, A]): VecShift[V] = ev.VShift
}

trait MutableVecShift[V] extends Any with VecShift[V] {
  def circShift(v: V, shift: Int): Unit
  def permute(v: V, k1: Int, k2: Int): Unit
  def permuteInverse(v: V, inversePerm: Array[Int]): Unit
}

object MutableVecShift {
  implicit def fromAlg[V, M, @sp(Double, Long) A](implicit ev: AlgUMVR[M, V, A]): MutableVecShift[V] = ev.VShift
}

final class VecShiftImpl[V, @sp(Double, Long) A](implicit V: VecBuilder[V, A]) extends VecShift[V] {

  def circShifted(v: V, shift: Int): V = {
    val n = V.length(v)
    V.tabulate(n) { k => V.apply(v, (k + shift) % n) }
  }

  def permuted(v: V, k1: Int, k2: Int): V =
    V.tabulate(V.length(v)) { k =>
      if (k == k1) V.apply(v, k2)
      else if (k == k2) V.apply(v, k1)
      else V.apply(v, k)
    }

  def permuted(v: V, perm: Array[Int]): V =
    V.tabulate(V.length(v)) { k => V.apply(v, perm(k)) }
}

final class MutableVecShiftImpl[V, @sp(Double, Long) A](implicit V: VecBuilder[V, A], VM: VecMutable[V, A]) extends MutableVecShift[V] {

  def circShift(v: V, shift: Int): Unit = ??? // TODO

  def circShifted(v: V, shift: Int): V = {
    val n = V.length(v)
    V.tabulate(n) { k => V.apply(v, (k + shift) % n) }
  }

  def permuted(v: V, k1: Int, k2: Int): V =
    V.tabulate(V.length(v)) { k =>
      if (k == k1) V.apply(v, k2)
      else if (k == k2) V.apply(v, k1)
      else V.apply(v, k)
    }

  def permuted(v: V, perm: Array[Int]): V =
    V.tabulate(V.length(v)) { k => V.apply(v, perm(k)) }

  def permute(v: V, k1: Int, k2: Int): Unit = {
    val t = V.apply(v, k1)
    VM.update(v, k1, V.apply(v, k2))
    VM.update(v, k2, t)
  }

  def permuteInverse(v: V, permInverse: Array[Int]): Unit = {
    val bs = scala.collection.mutable.BitSet.empty
    cforRange(0 until V.length(v)) { i => bs += i }
    while (bs.nonEmpty) {
      val start = bs.head
      bs -= start
      if (start != permInverse(start)) {
        cforRange(0 until V.length(v)) { k =>
          var last = start
          var current = permInverse(start)
          val temp = V.apply(v, start)
          while (current != start) {
            bs -= current
            VM.update(v, last, V.apply(v, current))
            last = current
            current = permInverse(current)
          }
          VM.update(v, last, temp)
        }
      }
    }
  }
}
