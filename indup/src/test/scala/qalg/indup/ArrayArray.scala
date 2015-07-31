package com.faacets.qalg
package indup

import algebra._

class ArrayArrayIntAlgebra extends Dense2[Array[Array[Int]], Array[Array[Int]], Array[Int], Int] with IndexAt2To1[Array[Array[Int]], Array[Array[Int]], Array[Int], Int] with Update2[Array[Array[Int]], Int] {
  implicit def index2: Index2[Array[Array[Int]], Array[Array[Int]], Array[Int], Int] = this
  implicit def zeroA: Zero[Int] = Zero.IntZero
  def apply(t: Array[Array[Int]], i: IntInt): Int = apply(t, i._1, i._2)
  def apply(t: Array[Array[Int]], i: Int, j: Int): Int = t(i)(j)
  def size(t: Array[Array[Int]], dim: Int): Int = if (dim == 0) t.length else t(0).length
  def size(t: Array[Array[Int]]): IntInt = IntInt(t.length, t(0).length)
  def size0(t: Array[Array[Int]]): Int = t.length
  def size1(t: Array[Array[Int]]): Int = t(0).length
  def update(t: Array[Array[Int]], i: IntInt, a: Int): Unit = { t(i._1)(i._2) = a }
  def update(t: Array[Array[Int]], i: Int, j: Int, a: Int): Unit = { t(i)(j) = a }
  def apply(t: Array[Array[Int]], at0: At, at1: At): Array[Array[Int]] = {
    val a0 = at0.forSize(size0(t))
    val a1 = at1.forSize(size1(t))
    Array.tabulate(a0.size, a1.size)( (i, j) => t(a0(i))(a1(j)) )
  }
  def apply(t: Array[Array[Int]], at0: At, i1: Int): Array[Int] = {
    val a0 = at0.forSize(size0(t))
    Array.tabulate(a0.size)( i => t(a0(i))(i1) )
  }
  def apply(t: Array[Array[Int]], i0: Int, at1: At): Array[Int] = {
    val a1 = at1.forSize(size1(t))
    Array.tabulate(a1.size)( i => t(i0)(a1(i)) )
  }
}
