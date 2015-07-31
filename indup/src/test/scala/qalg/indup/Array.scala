package com.faacets.qalg
package indup

import algebra._

class ArrayIntAlgebra extends Dense1[Array[Int], Array[Int], Int] with IndexAt1[Array[Int], Array[Int], Int] with Update1[Array[Int], Int] {
  implicit def index1: Index1[Array[Int], Array[Int], Int] = this
  implicit def zeroA: Zero[Int] = Zero.IntZero
  def update(t: Array[Int], i: Int, a: Int): Unit = { t(i) = a }
  def size(t: Array[Int]): Int = t.length
  def apply(t: Array[Int], i: Int): Int = t(i)
  def apply(t: Array[Int], at: At): Array[Int] = {
    val a = at.forSize(t.length)
    Array.tabulate(a.size)(k => t(a(k)))
  }
}
