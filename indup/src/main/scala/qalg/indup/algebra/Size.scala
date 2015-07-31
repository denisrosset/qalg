package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait Size[T, @sp(Int) I] extends Any {
  def dims: Int
  def size(t: T): I
  def size(t: T, dim: Int): Int
}
