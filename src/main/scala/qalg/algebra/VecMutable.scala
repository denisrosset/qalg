package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait VecMutable[VA, @sp(Double, Long) A] extends Any { self =>
  def update(v: VA, k: Int, a: A): Unit
  def update[VA1](v: VA, at: At1, a: A): Unit = {
    val n = at.length
    cforRange(0 until n) { k =>
      update(v, at(k), a)
    }
  }
  def update[VA1](v: VA, at: At1, va1: VA1)(implicit ev1: Vec[VA1, A]): Unit = {
    val n = at.length
    require(n == ev1.length(va1))
    cforRange(0 until n) { k =>
      update(v, at(k), ev1.apply(va1, k))
    }
  }
}
