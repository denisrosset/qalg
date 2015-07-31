package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait IndexAt1[T1, +R1, @sp A] extends Any with Index1[T1, R1, A] {
  def apply(t: T1, at: At): R1
}
