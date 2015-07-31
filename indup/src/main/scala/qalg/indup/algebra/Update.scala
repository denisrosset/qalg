package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait Update[T, I, @sp A] extends Any {
  def update(t: T, i: I, a: A): Unit
}
