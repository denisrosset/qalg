package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

import spire.syntax.cfor._

trait Update1[T, @sp A] extends Any with Update[T, Int, A] {
  implicit def index1: Index1[T, _, A]
  def update(t: T, i: Int, a: A): Unit
  def update(t: T, at: At, a: A): Unit = at match {
    case AtAll => cforRange(0 until index1.size(t)) { update(t, _, a) }
    case sized: AtSized =>
      cforRange(0 until sized.size) { k =>
        update(t, sized(k), a)
      }
  }

  def update[U](t: T, at: At, u: U)(implicit U: Index1[U, _, A]): Unit = at match {
    case AtAll => cforRange(0 until index1.size(t)) ( k => update(t, k, U.apply(u, k)) )
    case sized: AtSized =>
      cforRange(0 until sized.size) { k =>
        update(t, sized(k), U.apply(u, k))
      }
  }
}
