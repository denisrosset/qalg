package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait Update1[T, @sp A] extends Any {
  def update(t: T, i: Int, a: A): Unit
}

trait Update2[T, @sp A] extends Any {
  def update(t: T, i: Int, j: Int, a: A): Unit
}
