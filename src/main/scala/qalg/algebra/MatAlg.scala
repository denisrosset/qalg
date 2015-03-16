package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait MatAlg[MA, @sp(Double, Long) A] extends Any { self =>
  def rank(m: MA): Int
  def rref(m: MA): MA
  def det(m: MA): A
  def trace(m: MA): A
  def inverse(m: MA): MA
}
