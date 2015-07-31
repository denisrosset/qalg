package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait IndexAt2[T2, +R2, +R1, @sp A] extends Any with Index2[T2, R2, R1, A] {
  def apply(t: T2, at0: At, at1: At): R2
}
