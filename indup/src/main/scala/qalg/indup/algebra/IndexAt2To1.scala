package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait IndexAt2To1[T2, +R2, +R1, @sp A] extends Any with IndexAt2[T2, R2, R1, A] {
  def apply(t: T2, at0: At, i1: Int): R1
  def apply(t: T2, i0: Int, at1: At): R1
}
