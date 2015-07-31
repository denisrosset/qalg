package com.faacets.qalg.indup
package algebra

trait Size1[T] extends Any with Size[T, Int] {
  def dims = 1
  def size(t: T, dim: Int): Int = size(t)
}
