package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait VecBuilder[V, @sp(Double, Long) A] extends Any {
  def size: Int
  def add(i: Int, a: A): Unit
  def result(): V
}
