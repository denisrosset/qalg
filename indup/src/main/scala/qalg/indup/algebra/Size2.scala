package com.faacets.qalg.indup
package algebra

trait Size2[T2] extends Any with Size[T2, IntInt] {
  def dims = 2
  def size0(t: T2): Int
  def size1(t: T2): Int
}
