package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

import spire.syntax.cfor._

trait Update2[T2, @sp A] extends Any with Update[T2, IntInt, A] {
  implicit def index2: Index2[T2, _, _, A]

  def update(t: T2, i0: Int, i1: Int, a: A): Unit

  def update(t: T2, at0: At, i1: Int, a: A): Unit = {
    val n0 = at0.sizeOrElse(index2.size0(t))
    cforRange(0 until n0) { k0 =>
      val i0 = at0(k0)
      update(t, i0, i1, a)
    }
  }
  def update(t: T2, i0: Int, at1: At, a: A): Unit = {
    val n1 = at1.sizeOrElse(index2.size1(t))
    cforRange(0 until n1) { k1 =>
      val i1 = at1(k1)
      update(t, i0, i1, a)
    }
  }
  def update(t: T2, at0: At, at1: At, a: A): Unit = {
    val n0 = at0.sizeOrElse(index2.size0(t))
    val n1 = at1.sizeOrElse(index2.size1(t))
    cforRange(0 until n0) { k0 =>
      val i0 = at0(k0)
      cforRange(0 until n1) { k1 =>
        val i1 = at1(k1)
        update(t, i0, i1, a)
      }
    }
  }
  def update[U1](t: T2, at0: At, i1: Int, u: U1)(implicit U1: Index1[U1, _, A]): Unit = {
    val n0 = at0.sizeOrElse(index2.size0(t))
    cforRange(0 until n0) { k0 =>
      val i0 = at0(k0)
      update(t, i0, i1, U1.apply(u, k0))
    }
  }
  def update[U1](t: T2, i0: Int, at1: At, u: U1)(implicit U1: Index1[U1, _, A]): Unit = {
    val n1 = at1.sizeOrElse(index2.size1(t))
    cforRange(0 until n1) { k1 =>
      val i1 = at1(k1)
      update(t, i0, i1, U1.apply(u, k1))
    }
  }
  def update[U2](t: T2, at0: At, at1: At, u: U2)(implicit U2: Index2[U2, _, _, A]): Unit = {
    val n0 = at0.sizeOrElse(index2.size0(t))
    val n1 = at1.sizeOrElse(index2.size1(t))
    cforRange(0 until n0) { k0 =>
      val i0 = at0(k0)
      cforRange(0 until n1) { k1 =>
        val i1 = at1(k1)
        update(t, i0, i1, U2.apply(u, k0, k1))
      }
    }
  }
}
