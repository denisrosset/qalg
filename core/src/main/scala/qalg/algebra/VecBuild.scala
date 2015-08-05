package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import indup.algebra._

import spire.syntax.cfor._

trait VecBuild[V, @sp(Double, Long) A] extends Any with Vec[V, A] with WithOptions[V] { self =>

  // direct methods

  def builder(accordingTo: V): VecBuilder[V, A] = builder(size(accordingTo), options(accordingTo))

  def builder(size: Int, options: Options): VecBuilder[V, A]

  def builder(size: Int): VecBuilder[V, A] = builder(size, defaultOptions)

  def from[U](u: U, options: Options = defaultOptions)(implicit U: Vec[U, A]): V = {
    val b = builder(U.size(u), options)
    U.feedTo(u, b)
    b.result()
  }

  def build(size: Int, options: Options)(elements: (Int, A)*): V = {
    val b = builder(size, options)
    cforRange(0 until elements.size) { k =>
      val element = elements(k)
      b.add(element._1, element._2)
    }
    b.result()
  }

  def build(size: Int)(elements: (Int, A)*): V = build(size, defaultOptions)(elements: _*)

  def build(options: Options)(elements: A*): V = {
    val b = builder(elements.size, options)
    cforRange(0 until elements.size) { k =>
      b.add(k, elements(k))
    }
    b.result()
  }

  def build(elements: A*): V = build(defaultOptions)(elements: _*)

  def tabulate(n: Int, options: Options = defaultOptions)(f: Int => A): V = {
    val b = builder(n, options)
    cforRange(0 until n) { k =>
      b.add(k, f(k))
    }
    b.result()
  }

  def fill(n: Int, options: Options = defaultOptions)(a: A): V = {
    val b = builder(n, options)
    if (zeroA.canBeNonZero(a)) {
      cforRange(0 until n) { k =>
        b.add(k, a)
      }
    }
    b.result()
  }

  // type class methods

  def copy(v: V): V

  // default implementations

  def apply(v: V, at: At): V = at match {
    case AtAll => copy(v)
    case sized: AtSized =>
      val b = builder(sized.size)
      cforRange(0 until sized.size) { k =>
        b.add(k, apply(v, at(k)))
      }
      b.result()
  }
}

object VecBuild {
  type WithOptions[V, @sp(Double, Long) A, O] = VecBuild[V, A] { type Options = O }
  def apply[V, @sp(Double, Long) A](implicit V: VecBuild[V, A]): VecBuild[V, A] = V
}
