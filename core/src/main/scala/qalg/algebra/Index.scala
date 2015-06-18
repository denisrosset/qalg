package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Index1[T, @sp A] extends Any {
  def apply(t: T, i: Int): A
}

trait Index2[T, @sp A] extends Any {
  def apply(t: T, i: Int, j: Int): A
}
